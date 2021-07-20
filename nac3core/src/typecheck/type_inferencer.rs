use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;
use std::rc::Rc;

use super::magic_methods::*;
use super::symbol_resolver::{SymbolResolver, SymbolType};
use super::typedef::{Call, FunSignature, FuncArg, Type, TypeEnum, Unifier};
use itertools::izip;
use rustpython_parser::ast::{
    self,
    fold::{self, Fold},
    Arguments, Expr, ExprKind, Located, Location,
};

pub struct PrimitiveStore {
    int32: Type,
    int64: Type,
    float: Type,
    bool: Type,
    none: Type,
}

pub struct Inferencer<'a> {
    resolver: &'a mut Box<dyn SymbolResolver>,
    unifier: &'a mut Unifier,
    variable_mapping: HashMap<String, Type>,
    calls: &'a mut Vec<Rc<Call>>,
    primitives: &'a PrimitiveStore,
}

impl<'a> fold::Fold<()> for Inferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }

    fn fold_expr(&mut self, node: ast::Expr<()>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        let expr = match node.node {
            ast::ExprKind::Call {
                func,
                args,
                keywords,
            } => unimplemented!(),
            ast::ExprKind::Lambda { args, body } => {
                self.fold_lambda(node.location, *args, *body)?
            }
            ast::ExprKind::ListComp { elt, generators } => unimplemented!(),
            _ => fold::fold_expr(self, node)?,
        };
        let custom = match &expr.node {
            ast::ExprKind::Constant { value, .. } => Some(self.infer_constant(value)?),
            ast::ExprKind::Name { id, .. } => Some(self.infer_identifier(id)?),
            ast::ExprKind::List { elts, .. } => Some(self.infer_list(elts)?),
            ast::ExprKind::Tuple { elts, .. } => Some(self.infer_tuple(elts)?),
            ast::ExprKind::Attribute {
                value,
                attr,
                ctx: _,
            } => Some(self.infer_attribute(value, attr)?),
            ast::ExprKind::BoolOp { op: _, values } => Some(self.infer_bool_ops(values)?),
            ast::ExprKind::BinOp { left, op, right } => Some(self.infer_bin_ops(left, op, right)?),
            ast::ExprKind::UnaryOp { op, operand } => Some(self.infer_unary_ops(op, operand)?),
            ast::ExprKind::Compare {
                left,
                ops,
                comparators,
            } => Some(self.infer_compare(left, ops, comparators)?),
            ast::ExprKind::Call { .. } => expr.custom,
            ast::ExprKind::Subscript {
                value,
                slice,
                ctx: _,
            } => Some(self.infer_subscript(value.as_ref(), slice.as_ref())?),
            ast::ExprKind::IfExp { test, body, orelse } => {
                Some(self.infer_if_expr(test, body.as_ref(), orelse.as_ref())?)
            }
            ast::ExprKind::ListComp {
                elt: _,
                generators: _,
            } => expr.custom, // already computed
            ast::ExprKind::Slice { .. } => None, // we don't need it for slice
            _ => return Err("not supported yet".into()),
        };
        Ok(ast::Expr {
            custom,
            location: expr.location,
            node: expr.node,
        })
    }
}

type InferenceResult = Result<Type, String>;

impl<'a> Inferencer<'a> {
    /// Constrain a <: b
    /// Currently implemented as unification
    fn constrain(&mut self, a: Type, b: Type) -> Result<(), String> {
        self.unifier.unify(a, b)
    }

    fn build_method_call(
        &mut self,
        method: String,
        obj: Type,
        params: Vec<Type>,
        ret: Type,
    ) -> InferenceResult {
        let call = Rc::new(Call {
            posargs: params,
            kwargs: HashMap::new(),
            ret,
            fun: RefCell::new(None),
        });
        self.calls.push(call.clone());
        let call = self.unifier.add_ty(TypeEnum::TCall { calls: vec![call] });
        let fields = once((method, call)).collect();
        let record = self.unifier.add_ty(TypeEnum::TRecord { fields });
        self.constrain(obj, record)?;
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

        let fn_args: Vec<_> = args
            .args
            .iter()
            .map(|v| (v.node.arg.clone(), self.unifier.get_fresh_var().0))
            .collect();
        let mut variable_mapping = self.variable_mapping.clone();
        variable_mapping.extend(fn_args.iter().cloned());
        let ret = self.unifier.get_fresh_var().0;
        let mut new_context = Inferencer {
            resolver: self.resolver,
            unifier: self.unifier,
            variable_mapping,
            calls: self.calls,
            primitives: self.primitives,
        };
        let fun = FunSignature {
            args: fn_args
                .iter()
                .map(|(k, ty)| FuncArg {
                    name: k.clone(),
                    ty: *ty,
                    is_optional: false,
                })
                .collect(),
            ret,
            vars: Default::default(),
        };
        let body = new_context.fold_expr(body)?;
        new_context.unifier.unify(fun.ret, body.custom.unwrap())?;
        let mut args = new_context.fold_arguments(args)?;
        for (arg, (name, ty)) in args.args.iter_mut().zip(fn_args.iter()) {
            assert_eq!(&arg.node.arg, name);
            arg.custom = Some(*ty);
        }
        Ok(Located {
            location,
            node: ExprKind::Lambda {
                args: args.into(),
                body: body.into(),
            },
            custom: Some(self.unifier.add_ty(TypeEnum::TFunc(fun))),
        })
    }

    fn infer_identifier(&mut self, id: &str) -> InferenceResult {
        if let Some(ty) = self.variable_mapping.get(id) {
            Ok(*ty)
        } else {
            match self.resolver.get_symbol_type(id) {
                Some(SymbolType::TypeName(_)) => {
                    Err("Expected expression instead of type".to_string())
                }
                Some(SymbolType::Identifier(ty)) => Ok(ty),
                None => {
                    let ty = self.unifier.get_fresh_var().0;
                    self.variable_mapping.insert(id.to_string(), ty);
                    Ok(ty)
                }
            }
        }
    }

    fn infer_constant(&mut self, constant: &ast::Constant) -> InferenceResult {
        match constant {
            ast::Constant::Bool(_) => Ok(self.primitives.bool),
            ast::Constant::Int(val) => {
                let int32: Result<i32, _> = val.try_into();
                // int64 would be handled separately in functions
                if int32.is_ok() {
                    Ok(self.primitives.int64)
                } else {
                    Err("Integer out of bound".into())
                }
            }
            ast::Constant::Float(_) => Ok(self.primitives.float),
            ast::Constant::Tuple(vals) => {
                let ty: Result<Vec<_>, _> = vals.iter().map(|x| self.infer_constant(x)).collect();
                Ok(self.unifier.add_ty(TypeEnum::TTuple { ty: ty? }))
            }
            _ => Err("not supported".into()),
        }
    }

    fn infer_list(&mut self, elts: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let (ty, _) = self.unifier.get_fresh_var();
        for t in elts.iter() {
            self.unifier.unify(ty, t.custom.unwrap())?;
        }
        Ok(ty)
    }

    fn infer_tuple(&mut self, elts: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let ty = elts.iter().map(|x| x.custom.unwrap()).collect();
        Ok(self.unifier.add_ty(TypeEnum::TTuple { ty }))
    }

    fn infer_attribute(&mut self, value: &ast::Expr<Option<Type>>, attr: &str) -> InferenceResult {
        let (attr_ty, _) = self.unifier.get_fresh_var();
        let fields = once((attr.to_string(), attr_ty)).collect();
        let record = self.unifier.add_ty(TypeEnum::TRecord { fields });
        self.constrain(value.custom.unwrap(), record)?;
        Ok(attr_ty)
    }

    fn infer_bool_ops(&mut self, values: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let b = self.primitives.bool;
        for v in values {
            self.constrain(v.custom.unwrap(), b)?;
        }
        Ok(b)
    }

    fn infer_bin_ops(
        &mut self,
        left: &ast::Expr<Option<Type>>,
        op: &ast::Operator,
        right: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        let method = binop_name(op);
        let ret = self.unifier.get_fresh_var().0;
        self.build_method_call(
            method.to_string(),
            left.custom.unwrap(),
            vec![right.custom.unwrap()],
            ret,
        )
    }

    fn infer_unary_ops(
        &mut self,
        op: &ast::Unaryop,
        operand: &ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        let method = unaryop_name(op);
        let ret = self.unifier.get_fresh_var().0;
        self.build_method_call(method.to_string(), operand.custom.unwrap(), vec![], ret)
    }

    fn infer_compare(
        &mut self,
        left: &ast::Expr<Option<Type>>,
        ops: &[ast::Cmpop],
        comparators: &[ast::Expr<Option<Type>>],
    ) -> InferenceResult {
        let boolean = self.primitives.bool;
        for (a, b, c) in izip!(once(left).chain(comparators), comparators, ops) {
            let method = comparison_name(c)
                .ok_or_else(|| "unsupported comparator".to_string())?
                .to_string();
            self.build_method_call(method, a.custom.unwrap(), vec![b.custom.unwrap()], boolean)?;
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
                for v in [lower.as_ref(), upper.as_ref(), step.as_ref()]
                    .iter()
                    .flatten()
                {
                    self.constrain(v.custom.unwrap(), self.primitives.int32)?;
                }
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.constrain(value.custom.unwrap(), list)?;
                Ok(list)
            }
            ast::ExprKind::Constant {
                value: ast::Constant::Int(val),
                ..
            } => {
                // the index is a constant, so value can be a sequence.
                let ind: i32 = val
                    .try_into()
                    .map_err(|_| "Index must be int32".to_string())?;
                let map = once((ind, ty)).collect();
                let seq = self.unifier.add_ty(TypeEnum::TSeq { map });
                self.constrain(value.custom.unwrap(), seq)?;
                Ok(ty)
            }
            _ => {
                // the index is not a constant, so value can only be a list
                self.constrain(slice.custom.unwrap(), self.primitives.int32)?;
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.constrain(value.custom.unwrap(), list)?;
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
        self.constrain(test.custom.unwrap(), self.primitives.bool)?;
        let ty = self.unifier.get_fresh_var().0;
        self.constrain(body.custom.unwrap(), ty)?;
        self.constrain(orelse.custom.unwrap(), ty)?;
        Ok(ty)
    }
}
