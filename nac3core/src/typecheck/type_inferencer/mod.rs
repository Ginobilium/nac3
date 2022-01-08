use std::collections::{HashMap, HashSet};
use std::convert::{From, TryInto};
use std::iter::once;
use std::{cell::RefCell, sync::Arc};

use super::typedef::{Call, FunSignature, FuncArg, Type, TypeEnum, Unifier};
use super::{magic_methods::*, typedef::CallId};
use crate::{symbol_resolver::SymbolResolver, toplevel::TopLevelContext};
use itertools::izip;
use nac3parser::ast::{
    self,
    fold::{self, Fold},
    Arguments, Comprehension, ExprContext, ExprKind, Located, Location, StrRef,
};

#[cfg(test)]
mod test;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct CodeLocation {
    row: usize,
    col: usize,
}

impl From<Location> for CodeLocation {
    fn from(loc: Location) -> CodeLocation {
        CodeLocation { row: loc.row(), col: loc.column() }
    }
}

#[derive(Clone, Copy)]
pub struct PrimitiveStore {
    pub int32: Type,
    pub int64: Type,
    pub float: Type,
    pub bool: Type,
    pub none: Type,
    pub range: Type,
    pub str: Type,
}

pub struct FunctionData {
    pub resolver: Arc<dyn SymbolResolver + Send + Sync>,
    pub return_type: Option<Type>,
    pub bound_variables: Vec<Type>,
}

pub struct Inferencer<'a> {
    pub top_level: &'a TopLevelContext,
    pub defined_identifiers: HashSet<StrRef>,
    pub function_data: &'a mut FunctionData,
    pub unifier: &'a mut Unifier,
    pub primitives: &'a PrimitiveStore,
    pub virtual_checks: &'a mut Vec<(Type, Type)>,
    pub variable_mapping: HashMap<StrRef, Type>,
    pub calls: &'a mut HashMap<CodeLocation, CallId>,
}

struct NaiveFolder();
impl fold::Fold<()> for NaiveFolder {
    type TargetU = Option<Type>;
    type Error = String;
    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }
}

fn report_error<T>(msg: &str, location: Location) -> Result<T, String> {
    Err(format!("{} at {}", msg, location))
}

impl<'a> fold::Fold<()> for Inferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }

    fn fold_stmt(
        &mut self,
        mut node: ast::Stmt<()>,
    ) -> Result<ast::Stmt<Self::TargetU>, Self::Error> {
        let stmt = match node.node {
            // we don't want fold over type annotation
            ast::StmtKind::AnnAssign { mut target, annotation, value, simple, config_comment } => {
                self.infer_pattern(&target)?;
                // fix parser problem...
                if let ExprKind::Attribute { ctx, .. } = &mut target.node {
                    *ctx = ExprContext::Store;
                }

                let target = Box::new(self.fold_expr(*target)?);
                let value = if let Some(v) = value {
                    let ty = Box::new(self.fold_expr(*v)?);
                    self.unify(target.custom.unwrap(), ty.custom.unwrap(), &node.location)?;
                    Some(ty)
                } else {
                    return Err(format!(
                        "declaration without definition is not yet supported, at {}",
                        node.location
                    ));
                };
                let top_level_defs = self.top_level.definitions.read();
                let annotation_type = self.function_data.resolver.parse_type_annotation(
                    top_level_defs.as_slice(),
                    self.unifier,
                    self.primitives,
                    annotation.as_ref(),
                )?;
                self.unify(annotation_type, target.custom.unwrap(), &node.location)?;
                let annotation = Box::new(NaiveFolder().fold_expr(*annotation)?);
                Located {
                    location: node.location,
                    custom: None,
                    node: ast::StmtKind::AnnAssign {
                        target,
                        annotation,
                        value,
                        simple,
                        config_comment,
                    },
                }
            }
            ast::StmtKind::For { target, iter, body, orelse, config_comment, type_comment } => {
                self.infer_pattern(&target)?;
                let target = self.fold_expr(*target)?;
                let iter = self.fold_expr(*iter)?;
                if self.unifier.unioned(iter.custom.unwrap(), self.primitives.range) {
                    self.unify(self.primitives.int32, target.custom.unwrap(), &target.location)?;
                } else {
                    let list = self.unifier.add_ty(TypeEnum::TList { ty: target.custom.unwrap() });
                    self.unify(list, iter.custom.unwrap(), &iter.location)?;
                }
                let body = body
                    .into_iter()
                    .map(|b| self.fold_stmt(b))
                    .collect::<Result<Vec<_>, _>>()?;
                let orelse = orelse
                    .into_iter()
                    .map(|o| self.fold_stmt(o))
                    .collect::<Result<Vec<_>, _>>()?;
                Located {
                    location: node.location,
                    node: ast::StmtKind::For {
                        target: Box::new(target),
                        iter: Box::new(iter),
                        body,
                        orelse,
                        config_comment,
                        type_comment,
                    },
                    custom: None
                }
            }
            ast::StmtKind::Assign { ref mut targets, ref config_comment, .. } => {
                for target in targets.iter_mut() {
                    if let ExprKind::Attribute { ctx, .. } = &mut target.node {
                        *ctx = ExprContext::Store;
                    }
                }
                if targets.iter().all(|t| matches!(t.node, ast::ExprKind::Name { .. })) {
                    if let ast::StmtKind::Assign { targets, value, .. } = node.node {
                        let value = self.fold_expr(*value)?;
                        let value_ty = value.custom.unwrap();
                        let targets: Result<Vec<_>, _> = targets
                            .into_iter()
                            .map(|target| {
                                if let ast::ExprKind::Name { id, ctx } = target.node {
                                    self.defined_identifiers.insert(id);
                                    let target_ty = if let Some(ty) = self.variable_mapping.get(&id)
                                    {
                                        *ty
                                    } else {
                                        let unifier = &mut self.unifier;
                                        self.function_data
                                            .resolver
                                            .get_symbol_type(
                                                unifier,
                                                &self.top_level.definitions.read(),
                                                self.primitives,
                                                id,
                                            )
                                            .unwrap_or_else(|| {
                                                self.variable_mapping.insert(id, value_ty);
                                                value_ty
                                            })
                                    };
                                    let location = target.location;
                                    self.unifier.unify(value_ty, target_ty).map(|_| Located {
                                        location,
                                        node: ast::ExprKind::Name { id, ctx },
                                        custom: Some(target_ty),
                                    })
                                } else {
                                    unreachable!()
                                }
                            })
                            .collect();
                        let targets = targets?;
                        return Ok(Located {
                            location: node.location,
                            node: ast::StmtKind::Assign {
                                targets,
                                value: Box::new(value),
                                type_comment: None,
                                config_comment: config_comment.clone(),
                            },
                            custom: None,
                        });
                    } else {
                        unreachable!()
                    }
                }
                for target in targets {
                    self.infer_pattern(target)?;
                }
                fold::fold_stmt(self, node)?
            }
            ast::StmtKind::With { ref items, .. } => {
                for item in items.iter() {
                    if let Some(var) = &item.optional_vars {
                        self.infer_pattern(var)?;
                    }
                }
                fold::fold_stmt(self, node)?
            }
            _ => fold::fold_stmt(self, node)?,
        };
        match &stmt.node {
            ast::StmtKind::For { .. } => {}
            ast::StmtKind::If { test, .. } | ast::StmtKind::While { test, .. } => {
                self.unify(test.custom.unwrap(), self.primitives.bool, &test.location)?;
            }
            ast::StmtKind::Assign { targets, value, .. } => {
                for target in targets.iter() {
                    self.unify(target.custom.unwrap(), value.custom.unwrap(), &target.location)?;
                }
            }
            ast::StmtKind::AnnAssign { .. } | ast::StmtKind::Expr { .. } => {}
            ast::StmtKind::Break { .. }
            | ast::StmtKind::Continue { .. }
            | ast::StmtKind::Pass { .. } => {}
            ast::StmtKind::With { items, .. } => {
                for item in items.iter() {
                    let ty = item.context_expr.custom.unwrap();
                    // if we can simply unify without creating new types...
                    let mut fast_path = false;
                    if let TypeEnum::TObj { fields, .. } = &*self.unifier.get_ty(ty) {
                        let fields = fields.borrow();
                        fast_path = true;
                        if let Some(enter) = fields.get(&"__enter__".into()).cloned() {
                            if let TypeEnum::TFunc(signature) = &*self.unifier.get_ty(enter.0) {
                                let signature = signature.borrow();
                                if !signature.args.is_empty() {
                                    return report_error(
                                        "__enter__ method should take no argument other than self",
                                        stmt.location,
                                    );
                                }
                                if let Some(var) = &item.optional_vars {
                                    if signature.vars.is_empty() {
                                        self.unify(
                                            signature.ret,
                                            var.custom.unwrap(),
                                            &stmt.location,
                                        )?;
                                    } else {
                                        fast_path = false;
                                    }
                                }
                            } else {
                                fast_path = false;
                            }
                        } else {
                            return report_error(
                                "__enter__ method is required for context manager",
                                stmt.location,
                            );
                        }
                        if let Some(exit) = fields.get(&"__exit__".into()).cloned() {
                            if let TypeEnum::TFunc(signature) = &*self.unifier.get_ty(exit.0) {
                                let signature = signature.borrow();
                                if !signature.args.is_empty() {
                                    return report_error(
                                        "__exit__ method should take no argument other than self",
                                        stmt.location,
                                    );
                                }
                            } else {
                                fast_path = false;
                            }
                        } else {
                            return report_error(
                                "__exit__ method is required for context manager",
                                stmt.location,
                            );
                        }
                    }
                    if !fast_path {
                        let enter = TypeEnum::TFunc(RefCell::new(FunSignature {
                            args: vec![],
                            ret: item.optional_vars.as_ref().map_or_else(
                                || self.unifier.get_fresh_var().0,
                                |var| var.custom.unwrap(),
                            ),
                            vars: Default::default(),
                        }));
                        let enter = self.unifier.add_ty(enter);
                        let exit = TypeEnum::TFunc(RefCell::new(FunSignature {
                            args: vec![],
                            ret: self.unifier.get_fresh_var().0,
                            vars: Default::default(),
                        }));
                        let exit = self.unifier.add_ty(exit);
                        let mut fields = HashMap::new();
                        fields.insert("__enter__".into(), (enter, false));
                        fields.insert("__exit__".into(), (exit, false));
                        let record = self.unifier.add_record(fields);
                        self.unify(ty, record, &stmt.location)?;
                    }
                }
            }
            ast::StmtKind::Return { value, .. } => match (value, self.function_data.return_type) {
                (Some(v), Some(v1)) => {
                    self.unify(v.custom.unwrap(), v1, &v.location)?;
                }
                (Some(_), None) => {
                    return report_error("Unexpected return value", stmt.location);
                }
                (None, Some(_)) => {
                    return report_error("Expected return value", stmt.location);
                }
                (None, None) => {}
            },
            ast::StmtKind::AugAssign { target, op, value, .. } => {
                let res_ty = self.infer_bin_ops(stmt.location, target, op, value)?;
                self.unify(res_ty, target.custom.unwrap(), &stmt.location)?;
            }
            _ => return report_error("Unsupported statement type", stmt.location),
        };
        Ok(stmt)
    }

    fn fold_expr(&mut self, node: ast::Expr<()>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        let expr = match node.node {
            ast::ExprKind::Call { func, args, keywords } => {
                return self.fold_call(node.location, *func, args, keywords);
            }
            ast::ExprKind::Lambda { args, body } => {
                return self.fold_lambda(node.location, *args, *body);
            }
            ast::ExprKind::ListComp { elt, generators } => {
                return self.fold_listcomp(node.location, *elt, generators);
            }
            _ => fold::fold_expr(self, node)?,
        };
        let custom = match &expr.node {
            ast::ExprKind::Constant { value, .. } => Some(self.infer_constant(value)?),
            ast::ExprKind::Name { id, .. } => {
                if !self.defined_identifiers.contains(id) {
                    if self
                        .function_data
                        .resolver
                        .get_symbol_type(
                            self.unifier,
                            &self.top_level.definitions.read(),
                            self.primitives,
                            *id,
                        )
                        .is_some()
                    {
                        self.defined_identifiers.insert(*id);
                    } else {
                        return Err(format!(
                            "unknown identifier {} (use before def?) at {}",
                            id, expr.location
                        ));
                    }
                }
                Some(self.infer_identifier(*id)?)
            }
            ast::ExprKind::List { elts, .. } => Some(self.infer_list(elts)?),
            ast::ExprKind::Tuple { elts, .. } => Some(self.infer_tuple(elts)?),
            ast::ExprKind::Attribute { value, attr, ctx } => {
                Some(self.infer_attribute(value, *attr, ctx)?)
            }
            ast::ExprKind::BoolOp { values, .. } => Some(self.infer_bool_ops(values)?),
            ast::ExprKind::BinOp { left, op, right } => {
                Some(self.infer_bin_ops(expr.location, left, op, right)?)
            }
            ast::ExprKind::UnaryOp { op, operand } => Some(self.infer_unary_ops(op, operand)?),
            ast::ExprKind::Compare { left, ops, comparators } => {
                Some(self.infer_compare(left, ops, comparators)?)
            }
            ast::ExprKind::Subscript { value, slice, .. } => {
                Some(self.infer_subscript(value.as_ref(), slice.as_ref())?)
            }
            ast::ExprKind::IfExp { test, body, orelse } => {
                Some(self.infer_if_expr(test, body.as_ref(), orelse.as_ref())?)
            }
            ast::ExprKind::ListComp { .. }
            | ast::ExprKind::Lambda { .. }
            | ast::ExprKind::Call { .. } => expr.custom, // already computed
            ast::ExprKind::Slice { .. } => None, // we don't need it for slice
            _ => return Err("not supported yet".into()),
        };
        Ok(ast::Expr { custom, location: expr.location, node: expr.node })
    }
}

type InferenceResult = Result<Type, String>;

impl<'a> Inferencer<'a> {
    /// Constrain a <: b
    /// Currently implemented as unification
    fn constrain(&mut self, a: Type, b: Type, location: &Location) -> Result<(), String> {
        self.unifier.unify(a, b).map_err(|old| format!("{} at {}", old, location))
    }

    fn unify(&mut self, a: Type, b: Type, location: &Location) -> Result<(), String> {
        self.unifier.unify(a, b).map_err(|old| format!("{} at {}", old, location))
    }

    fn infer_pattern(&mut self, pattern: &ast::Expr<()>) -> Result<(), String> {
        match &pattern.node {
            ExprKind::Name { id, .. } => {
                if !self.defined_identifiers.contains(id) {
                    self.defined_identifiers.insert(*id);
                }
                Ok(())
            }
            ExprKind::Tuple { elts, .. } => {
                for elt in elts.iter() {
                    self.infer_pattern(elt)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn build_method_call(
        &mut self,
        location: Location,
        method: StrRef,
        obj: Type,
        params: Vec<Type>,
        ret: Option<Type>,
    ) -> InferenceResult {
        if let TypeEnum::TObj { params: class_params, fields, .. } = &*self.unifier.get_ty(obj) {
            if class_params.borrow().is_empty() {
                if let Some(ty) = fields.borrow().get(&method) {
                    let ty = ty.0;
                    if let TypeEnum::TFunc(sign) = &*self.unifier.get_ty(ty) {
                        let sign = sign.borrow();
                        if sign.vars.is_empty() {
                            let call = Call {
                                posargs: params,
                                kwargs: HashMap::new(),
                                ret: sign.ret,
                                fun: RefCell::new(None),
                            };
                            if let Some(ret) = ret {
                                self.unifier.unify(sign.ret, ret).unwrap();
                            }
                            let required: Vec<_> = sign
                                .args
                                .iter()
                                .filter(|v| v.default_value.is_none())
                                .map(|v| v.name)
                                .rev()
                                .collect();
                            self.unifier
                                .unify_call(&call, ty, &sign, &required)
                                .map_err(|old| format!("{} at {}", old, location))?;
                            return Ok(sign.ret);
                        }
                    }
                }
            }
        }
        let ret = ret.unwrap_or_else(|| self.unifier.get_fresh_var().0);

        let call = self.unifier.add_call(Call {
            posargs: params,
            kwargs: HashMap::new(),
            ret,
            fun: RefCell::new(None),
        });
        self.calls.insert(location.into(), call);
        let call = self.unifier.add_ty(TypeEnum::TCall(vec![call].into()));
        let fields = once((method, (call, false))).collect();
        let record = self.unifier.add_record(fields);
        self.constrain(obj, record, &location)?;
        Ok(ret)
    }

    fn fold_lambda(
        &mut self,
        location: Location,
        args: Arguments,
        body: ast::Expr<()>,
    ) -> Result<ast::Expr<Option<Type>>, String> {
        if !args.posonlyargs.is_empty()
            || args.vararg.is_some()
            || !args.kwonlyargs.is_empty()
            || args.kwarg.is_some()
            || !args.defaults.is_empty()
        {
            // actually I'm not sure whether programs violating this is a valid python program.
            return Err(
                "We only support positional or keyword arguments without defaults for lambdas."
                    .to_string(),
            );
        }

        let mut defined_identifiers = self.defined_identifiers.clone();
        for arg in args.args.iter() {
            let name = &arg.node.arg;
            if !defined_identifiers.contains(name) {
                defined_identifiers.insert(*name);
            }
        }
        let fn_args: Vec<_> =
            args.args.iter().map(|v| (v.node.arg, self.unifier.get_fresh_var().0)).collect();
        let mut variable_mapping = self.variable_mapping.clone();
        variable_mapping.extend(fn_args.iter().cloned());
        let ret = self.unifier.get_fresh_var().0;

        let mut new_context = Inferencer {
            function_data: self.function_data,
            unifier: self.unifier,
            primitives: self.primitives,
            virtual_checks: self.virtual_checks,
            calls: self.calls,
            top_level: self.top_level,
            defined_identifiers,
            variable_mapping,
        };
        let fun = FunSignature {
            args: fn_args
                .iter()
                .map(|(k, ty)| FuncArg { name: *k, ty: *ty, default_value: None })
                .collect(),
            ret,
            vars: Default::default(),
        };
        let body = new_context.fold_expr(body)?;
        new_context.unify(fun.ret, body.custom.unwrap(), &location)?;
        let mut args = new_context.fold_arguments(args)?;
        for (arg, (name, ty)) in args.args.iter_mut().zip(fn_args.iter()) {
            assert_eq!(&arg.node.arg, name);
            arg.custom = Some(*ty);
        }
        Ok(Located {
            location,
            node: ExprKind::Lambda { args: args.into(), body: body.into() },
            custom: Some(self.unifier.add_ty(TypeEnum::TFunc(fun.into()))),
        })
    }

    fn fold_listcomp(
        &mut self,
        location: Location,
        elt: ast::Expr<()>,
        mut generators: Vec<Comprehension>,
    ) -> Result<ast::Expr<Option<Type>>, String> {
        if generators.len() != 1 {
            return Err(
                "Only 1 generator statement for list comprehension is supported.".to_string()
            );
        }
        let variable_mapping = self.variable_mapping.clone();
        let defined_identifiers = self.defined_identifiers.clone();
        let mut new_context = Inferencer {
            function_data: self.function_data,
            unifier: self.unifier,
            virtual_checks: self.virtual_checks,
            top_level: self.top_level,
            variable_mapping,
            primitives: self.primitives,
            calls: self.calls,
            defined_identifiers,
        };
        let generator = generators.pop().unwrap();
        if generator.is_async {
            return Err("Async iterator not supported.".to_string());
        }
        new_context.infer_pattern(&generator.target)?;
        let target = new_context.fold_expr(*generator.target)?;
        let iter = new_context.fold_expr(*generator.iter)?;
        if new_context.unifier.unioned(iter.custom.unwrap(), new_context.primitives.range) {
            new_context.unify(
                target.custom.unwrap(),
                new_context.primitives.int32,
                &target.location,
            )?;
        } else {
            let list = new_context.unifier.add_ty(TypeEnum::TList { ty: target.custom.unwrap() });
            new_context.unify(iter.custom.unwrap(), list, &iter.location)?;
        }
        let ifs: Vec<_> = generator
            .ifs
            .into_iter()
            .map(|v| new_context.fold_expr(v))
            .collect::<Result<_, _>>()?;

        let elt = new_context.fold_expr(elt)?;
        // iter should be a list of targets...
        // actually it should be an iterator of targets, but we don't have iter type for now
        // if conditions should be bool
        for v in ifs.iter() {
            new_context.unify(v.custom.unwrap(), new_context.primitives.bool, &v.location)?;
        }

        Ok(Located {
            location,
            custom: Some(new_context.unifier.add_ty(TypeEnum::TList { ty: elt.custom.unwrap() })),
            node: ExprKind::ListComp {
                elt: Box::new(elt),
                generators: vec![ast::Comprehension {
                    target: Box::new(target),
                    iter: Box::new(iter),
                    ifs,
                    is_async: false,
                }],
            },
        })
    }

    fn fold_call(
        &mut self,
        location: Location,
        func: ast::Expr<()>,
        mut args: Vec<ast::Expr<()>>,
        keywords: Vec<Located<ast::KeywordData>>,
    ) -> Result<ast::Expr<Option<Type>>, String> {
        let func =
            if let Located { location: func_location, custom, node: ExprKind::Name { id, ctx } } =
                func
            {
                // handle special functions that cannot be typed in the usual way...
                if id == "virtual".into() {
                    if args.is_empty() || args.len() > 2 || !keywords.is_empty() {
                        return Err(
                            "`virtual` can only accept 1/2 positional arguments.".to_string()
                        );
                    }
                    let arg0 = self.fold_expr(args.remove(0))?;
                    let ty = if let Some(arg) = args.pop() {
                        let top_level_defs = self.top_level.definitions.read();
                        self.function_data.resolver.parse_type_annotation(
                            top_level_defs.as_slice(),
                            self.unifier,
                            self.primitives,
                            &arg,
                        )?
                    } else {
                        self.unifier.get_fresh_var().0
                    };
                    self.virtual_checks.push((arg0.custom.unwrap(), ty));
                    let custom = Some(self.unifier.add_ty(TypeEnum::TVirtual { ty }));
                    return Ok(Located {
                        location,
                        custom,
                        node: ExprKind::Call {
                            func: Box::new(Located {
                                custom: None,
                                location: func.location,
                                node: ExprKind::Name { id, ctx },
                            }),
                            args: vec![arg0],
                            keywords: vec![],
                        },
                    });
                }
                // int64 is special because its argument can be a constant larger than int32
                if id == "int64".into() && args.len() == 1 {
                    if let ExprKind::Constant { value: ast::Constant::Int(val), kind } =
                        &args[0].node
                    {
                        let custom = Some(self.primitives.int64);
                        if val.is_none() {
                            return Err("Integer out of bound".into());
                        }
                        return Ok(Located {
                            location: args[0].location,
                            custom,
                            node: ExprKind::Constant {
                                value: ast::Constant::Int(*val),
                                kind: kind.clone(),
                            },
                        });
                    }
                }
                Located { location: func_location, custom, node: ExprKind::Name { id, ctx } }
            } else {
                func
            };
        let func = Box::new(self.fold_expr(func)?);
        let args = args.into_iter().map(|v| self.fold_expr(v)).collect::<Result<Vec<_>, _>>()?;
        let keywords = keywords
            .into_iter()
            .map(|v| fold::fold_keyword(self, v))
            .collect::<Result<Vec<_>, _>>()?;

        if let TypeEnum::TFunc(sign) = &*self.unifier.get_ty(func.custom.unwrap()) {
            let sign = sign.borrow();
            if sign.vars.is_empty() {
                let call = Call {
                    posargs: args.iter().map(|v| v.custom.unwrap()).collect(),
                    kwargs: keywords
                        .iter()
                        .map(|v| (*v.node.arg.as_ref().unwrap(), v.custom.unwrap()))
                        .collect(),
                    fun: RefCell::new(None),
                    ret: sign.ret,
                };
                let required: Vec<_> = sign
                    .args
                    .iter()
                    .filter(|v| v.default_value.is_none())
                    .map(|v| v.name)
                    .rev()
                    .collect();
                self.unifier
                    .unify_call(&call, func.custom.unwrap(), &sign, &required)
                    .map_err(|old| format!("{} at {}", old, location))?;
                return Ok(Located {
                    location,
                    custom: Some(sign.ret),
                    node: ExprKind::Call { func, args, keywords },
                });
            }
        }

        let ret = self.unifier.get_fresh_var().0;
        let call = self.unifier.add_call(Call {
            posargs: args.iter().map(|v| v.custom.unwrap()).collect(),
            kwargs: keywords
                .iter()
                .map(|v| (*v.node.arg.as_ref().unwrap(), v.custom.unwrap()))
                .collect(),
            fun: RefCell::new(None),
            ret,
        });
        self.calls.insert(location.into(), call);
        let call = self.unifier.add_ty(TypeEnum::TCall(vec![call].into()));
        self.unify(func.custom.unwrap(), call, &func.location)?;

        Ok(Located { location, custom: Some(ret), node: ExprKind::Call { func, args, keywords } })
    }

    fn infer_identifier(&mut self, id: StrRef) -> InferenceResult {
        if let Some(ty) = self.variable_mapping.get(&id) {
            Ok(*ty)
        } else {
            let variable_mapping = &mut self.variable_mapping;
            let unifier = &mut self.unifier;
            Ok(self
                .function_data
                .resolver
                .get_symbol_type(unifier, &self.top_level.definitions.read(), self.primitives, id)
                .unwrap_or_else(|| {
                    let ty = unifier.get_fresh_var().0;
                    variable_mapping.insert(id, ty);
                    ty
                }))
        }
    }

    fn infer_constant(&mut self, constant: &ast::Constant) -> InferenceResult {
        match constant {
            ast::Constant::Bool(_) => Ok(self.primitives.bool),
            ast::Constant::Int(val) => {
                match val {
                    Some(val) => {
                        let int32: Result<i32, _> = (*val).try_into();
                        // int64 is handled separately in functions
                        if int32.is_ok() {
                            Ok(self.primitives.int32)
                        } else {
                            Err("Integer out of bound".into())
                        }
                    },
                    None => Err("Integer out of bound".into())
                }
            }
            ast::Constant::Float(_) => Ok(self.primitives.float),
            ast::Constant::Tuple(vals) => {
                let ty: Result<Vec<_>, _> = vals.iter().map(|x| self.infer_constant(x)).collect();
                Ok(self.unifier.add_ty(TypeEnum::TTuple { ty: ty? }))
            }
            ast::Constant::Str(_) => Ok(self.primitives.str),
            _ => Err("not supported".into()),
        }
    }

    fn infer_list(&mut self, elts: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let (ty, _) = self.unifier.get_fresh_var();
        for t in elts.iter() {
            self.unify(ty, t.custom.unwrap(), &t.location)?;
        }
        Ok(self.unifier.add_ty(TypeEnum::TList { ty }))
    }

    fn infer_tuple(&mut self, elts: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let ty = elts.iter().map(|x| x.custom.unwrap()).collect();
        Ok(self.unifier.add_ty(TypeEnum::TTuple { ty }))
    }

    fn infer_attribute(
        &mut self,
        value: &ast::Expr<Option<Type>>,
        attr: StrRef,
        ctx: &ExprContext,
    ) -> InferenceResult {
        let ty = value.custom.unwrap();
        if let TypeEnum::TObj { fields, .. } = &*self.unifier.get_ty(ty) {
            // just a fast path
            let fields = fields.borrow();
            match (fields.get(&attr), ctx == &ExprContext::Store) {
                (Some((ty, true)), _) => Ok(*ty),
                (Some((ty, false)), false) => Ok(*ty),
                (Some((_, false)), true) => {
                    report_error(&format!("Field {} should be immutable", attr), value.location)
                }
                (None, _) => report_error(&format!("No such field {}", attr), value.location),
            }
        } else {
            let (attr_ty, _) = self.unifier.get_fresh_var();
            let fields = once((attr, (attr_ty, ctx == &ExprContext::Store))).collect();
            let record = self.unifier.add_record(fields);
            self.constrain(value.custom.unwrap(), record, &value.location)?;
            Ok(attr_ty)
        }
    }

    fn infer_bool_ops(&mut self, values: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let b = self.primitives.bool;
        for v in values {
            self.constrain(v.custom.unwrap(), b, &v.location)?;
        }
        Ok(b)
    }

    fn infer_bin_ops(
        &mut self,
        location: Location,
        left: &ast::Expr<Option<Type>>,
        op: &ast::Operator,
        right: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        let method = binop_name(op).into();
        self.build_method_call(
            location,
            method,
            left.custom.unwrap(),
            vec![right.custom.unwrap()],
            None,
        )
    }

    fn infer_unary_ops(
        &mut self,
        op: &ast::Unaryop,
        operand: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        let method = unaryop_name(op).into();
        self.build_method_call(operand.location, method, operand.custom.unwrap(), vec![], None)
    }

    fn infer_compare(
        &mut self,
        left: &ast::Expr<Option<Type>>,
        ops: &[ast::Cmpop],
        comparators: &[ast::Expr<Option<Type>>],
    ) -> InferenceResult {
        let boolean = self.primitives.bool;
        for (a, b, c) in izip!(once(left).chain(comparators), comparators, ops) {
            let method =
                comparison_name(c).ok_or_else(|| "unsupported comparator".to_string())?.into();
            self.build_method_call(
                a.location,
                method,
                a.custom.unwrap(),
                vec![b.custom.unwrap()],
                Some(boolean),
            )?;
        }
        Ok(boolean)
    }

    fn infer_subscript(
        &mut self,
        value: &ast::Expr<Option<Type>>,
        slice: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        let ty = self.unifier.get_fresh_var().0;
        match &slice.node {
            ast::ExprKind::Slice { lower, upper, step } => {
                for v in [lower.as_ref(), upper.as_ref(), step.as_ref()].iter().flatten() {
                    self.constrain(v.custom.unwrap(), self.primitives.int32, &v.location)?;
                }
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.constrain(value.custom.unwrap(), list, &value.location)?;
                Ok(list)
            }
            ast::ExprKind::Constant { value: ast::Constant::Int(val), .. } => {
                // the index is a constant, so value can be a sequence.
                let ind: Option<i32> = match val {
                    Some(val) => (*val).try_into().ok(),
                    None => None,
                };
                let ind = ind.ok_or_else(|| "Index must be int32".to_string())?;
                let map = once((ind, ty)).collect();
                let seq = self.unifier.add_sequence(map);
                self.constrain(value.custom.unwrap(), seq, &value.location)?;
                Ok(ty)
            }
            _ => {
                // the index is not a constant, so value can only be a list
                self.constrain(slice.custom.unwrap(), self.primitives.int32, &slice.location)?;
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.constrain(value.custom.unwrap(), list, &value.location)?;
                Ok(ty)
            }
        }
    }

    fn infer_if_expr(
        &mut self,
        test: &ast::Expr<Option<Type>>,
        body: &ast::Expr<Option<Type>>,
        orelse: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        self.constrain(test.custom.unwrap(), self.primitives.bool, &test.location)?;
        let ty = self.unifier.get_fresh_var().0;
        self.constrain(body.custom.unwrap(), ty, &body.location)?;
        self.constrain(orelse.custom.unwrap(), ty, &orelse.location)?;
        Ok(ty)
    }
}
