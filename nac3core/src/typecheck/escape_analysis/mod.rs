use std::{collections::HashMap, sync::Arc};

use nac3parser::ast::{Constant, Expr, ExprKind, StrRef};

use crate::{
    symbol_resolver::SymbolResolver,
    toplevel::{TopLevelContext, TopLevelDef},
};

use self::lifetime::{BlockLifetimeContext, Lifetime, LifetimeTable};

use super::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, TypeEnum, Unifier},
};

pub mod lifetime;

struct LifetimeContext<'a> {
    variable_mapping: HashMap<StrRef, (Lifetime, bool)>,
    scope_ctx: BlockLifetimeContext,
    lifetime_table: LifetimeTable,
    primitive_store: PrimitiveStore,
    unifier: &'a mut Unifier,
    resolver: Arc<dyn SymbolResolver + Send + Sync>,
    top_level: &'a TopLevelContext,
}

impl<'a> LifetimeContext<'a> {
    pub fn new(
        unifier: &'a mut Unifier,
        primitive_store: PrimitiveStore,
        resolver: Arc<dyn SymbolResolver + Send + Sync>,
        top_level: &'a TopLevelContext,
    ) -> LifetimeContext<'a> {
        LifetimeContext {
            variable_mapping: HashMap::new(),
            scope_ctx: BlockLifetimeContext::new(),
            lifetime_table: LifetimeTable::new(),
            primitive_store,
            unifier,
            resolver,
            top_level,
        }
    }

    fn get_expr_lifetime(
        &mut self,
        expr: &Expr<Option<Type>>,
    ) -> Result<Option<(Lifetime, bool)>, String> {
        let ty = expr.custom.unwrap();
        let is_primitive = self.unifier.unioned(ty, self.primitive_store.int32)
            || self.unifier.unioned(ty, self.primitive_store.int64)
            || self.unifier.unioned(ty, self.primitive_store.uint32)
            || self.unifier.unioned(ty, self.primitive_store.uint64)
            || self.unifier.unioned(ty, self.primitive_store.float)
            || self.unifier.unioned(ty, self.primitive_store.bool)
            || self.unifier.unioned(ty, self.primitive_store.none)
            || self.unifier.unioned(ty, self.primitive_store.range);

        Ok(match &expr.node {
            ExprKind::Name { id, .. } => {
                if let Some(lifetime) = self.variable_mapping.get(id) {
                    Some(*lifetime)
                } else {
                    if is_primitive {
                        None
                    } else {
                        let lifetime =
                            self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Global);
                        self.variable_mapping.insert(id.clone(), (lifetime, false));
                        Some((lifetime, false))
                    }
                }
            }
            ExprKind::Attribute { value, attr, .. } => {
                if is_primitive {
                    self.get_expr_lifetime(value)?;
                    None
                } else {
                    self.get_expr_lifetime(value)?.map(|lifetime| {
                        (
                            self.lifetime_table.get_field_lifetime(
                                lifetime.0,
                                *attr,
                                &mut self.scope_ctx,
                            ),
                            false, // not sure if it is strong update for now...
                        )
                    })
                }
            }
            ExprKind::Constant { .. } => {
                if is_primitive {
                    None
                } else {
                    Some((self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Global), false))
                }
            }
            ExprKind::List { elts, .. } => {
                let elems =
                    elts.iter()
                        .map(|expr| self.get_expr_lifetime(expr))
                        .collect::<Result<Vec<_>, _>>()?;
                let elem = elems.into_iter().reduce(|prev, next| {
                    if prev.is_some() {
                        self.lifetime_table.unify(
                            prev.unwrap().0,
                            next.unwrap().0,
                            &mut self.scope_ctx,
                        );
                    }
                    prev
                });
                let list_lifetime = self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Local);

                if let Some(Some(elem)) = elem {
                    self.lifetime_table
                        .set_field_lifetime(
                            list_lifetime,
                            "elem".into(),
                            elem.0,
                            true,
                            &mut self.scope_ctx,
                        )
                        .unwrap();
                }
                Some((list_lifetime, true))
            }
            ExprKind::Subscript { value, slice, .. } => {
                // value must be a list, so lifetime cannot be None
                let (value_lifetime, _) = self.get_expr_lifetime(value)?.unwrap();
                match &slice.node {
                    ExprKind::Slice { lower, upper, step } => {
                        for expr in [lower, upper, step].iter().filter_map(|x| x.as_ref()) {
                            // account for side effects when computing the slice
                            self.get_expr_lifetime(expr)?;
                        }
                        Some((
                            self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Local),
                            true,
                        ))
                    }
                    ExprKind::Constant { value: Constant::Int(v), .. } => {
                        if is_primitive {
                            None
                        } else if let TypeEnum::TList { .. } =
                            &*self.unifier.get_ty(value.custom.unwrap())
                        {
                            Some((
                                self.lifetime_table.get_field_lifetime(
                                    value_lifetime,
                                    "elem".into(),
                                    &mut self.scope_ctx,
                                ),
                                false,
                            ))
                        } else {
                            // tuple
                            Some((
                                self.lifetime_table.get_field_lifetime(
                                    value_lifetime,
                                    format!("elem{}", v).into(),
                                    &mut self.scope_ctx,
                                ),
                                false,
                            ))
                        }
                    }
                    _ => {
                        // account for side effects when computing the index
                        self.get_expr_lifetime(slice)?;
                        if is_primitive {
                            None
                        } else {
                            Some((
                                self.lifetime_table.get_field_lifetime(
                                    value_lifetime,
                                    "elem".into(),
                                    &mut self.scope_ctx,
                                ),
                                false,
                            ))
                        }
                    }
                }
            }
            ExprKind::Tuple { elts, .. } => {
                let elems =
                    elts.iter()
                        .map(|expr| self.get_expr_lifetime(expr))
                        .collect::<Result<Vec<_>, _>>()?;
                let tuple_lifetime =
                    self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Local);
                for (i, lifetime) in elems.into_iter().enumerate() {
                    if let Some((lifetime, _)) = lifetime {
                        self.lifetime_table
                            .set_field_lifetime(
                                tuple_lifetime,
                                format!("elem{}", i).into(),
                                lifetime,
                                true,
                                &mut self.scope_ctx,
                            )
                            .unwrap();
                    }
                }
                Some((tuple_lifetime, true))
            }
            ExprKind::Call { func, args, keywords } => {
                let mut lifetimes = Vec::new();
                for arg in args.iter() {
                    if let Some(lifetime) = self.get_expr_lifetime(arg)? {
                        lifetimes.push(lifetime.0);
                    }
                }
                for keyword in keywords.iter() {
                    if let Some(lifetime) = self.get_expr_lifetime(&keyword.node.value)? {
                        lifetimes.push(lifetime.0);
                    }
                }
                match &func.node {
                    ExprKind::Name { id, .. } => {
                        for lifetime in lifetimes.into_iter() {
                            self.lifetime_table.set_function_params(lifetime, &mut self.scope_ctx);
                        }
                        if is_primitive {
                            None
                        } else {
                            let id = self
                                .resolver
                                .get_identifier_def(*id)
                                .map_err(|e| format!("{} (at {})", e, func.location))?;
                            // constructors
                            if let TopLevelDef::Class { .. } =
                                &*self.top_level.definitions.read()[id.0].read()
                            {
                                Some((
                                    self.lifetime_table.add_lifetime(lifetime::LifetimeKind::Local),
                                    true,
                                ))
                            } else {
                                Some((self.lifetime_table.get_unknown_lifetime(), false))
                            }
                        }
                    }
                    ExprKind::Attribute { value, .. } => {
                        if let Some(lifetime) = self.get_expr_lifetime(value)? {
                            lifetimes.push(lifetime.0);
                        }
                        for lifetime in lifetimes.into_iter() {
                            self.lifetime_table.set_function_params(lifetime, &mut self.scope_ctx);
                        }
                        if is_primitive {
                            None
                        } else {
                            Some((self.lifetime_table.get_unknown_lifetime(), false))
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                let mut lifetimes = Vec::new();
                if let Some(l) = self.get_expr_lifetime(left)? {
                    lifetimes.push(l.0);
                }
                if let Some(l) = self.get_expr_lifetime(right)? {
                    lifetimes.push(l.0);
                }
                for lifetime in lifetimes.into_iter() {
                    self.lifetime_table.set_function_params(lifetime, &mut self.scope_ctx);
                }
                if is_primitive {
                    None
                } else {
                    Some((self.lifetime_table.get_unknown_lifetime(), false))
                }
            }
            ExprKind::BoolOp { values, .. } => {
                for v in values {
                    self.get_expr_lifetime(v)?;
                }
                None
            }
            ExprKind::UnaryOp { operand, .. } => {
                if let Some(l) = self.get_expr_lifetime(operand)? {
                    self.lifetime_table.set_function_params(l.0, &mut self.scope_ctx);
                }
                if is_primitive {
                    None
                } else {
                    Some((self.lifetime_table.get_unknown_lifetime(), false))
                }
            }
            ExprKind::Compare { left,  comparators, .. } => {
                let mut lifetimes = Vec::new();
                if let Some(l) = self.get_expr_lifetime(left)? {
                    lifetimes.push(l.0);
                }
                for c in comparators {
                    if let Some(l) = self.get_expr_lifetime(c)? {
                        lifetimes.push(l.0);
                    }
                }
                for lifetime in lifetimes.into_iter() {
                    self.lifetime_table.set_function_params(lifetime, &mut self.scope_ctx);
                }
                // compare should give bool output, which does not have lifetime
                None
            }
            // TODO: listcomp, ifexpr
            _ => unimplemented!(),
        })

    }
}
