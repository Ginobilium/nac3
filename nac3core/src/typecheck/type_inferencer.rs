use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;
use std::rc::Rc;

use super::magic_methods::*;
use super::symbol_resolver::{SymbolResolver, SymbolType};
use super::typedef::{Call, Type, TypeEnum, Unifier};
use itertools::izip;
use rustpython_parser::ast::{self, fold::Fold};

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
    variable_mapping: &'a mut HashMap<String, Type>,
    calls: &'a mut Vec<Rc<Call>>,
    primitives: &'a PrimitiveStore,
}

impl<'a> Fold<()> for Inferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }
}

type InferenceResult = Result<Type, String>;

impl<'a> Inferencer<'a> {
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
        self.unifier.unify(obj, record)?;
        Ok(ret)
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
        let parent = self.unifier.add_ty(TypeEnum::TRecord { fields });
        self.unifier.unify(value.custom.unwrap(), parent)?;
        Ok(attr_ty)
    }

    fn infer_bool_ops(&mut self, values: &[ast::Expr<Option<Type>>]) -> InferenceResult {
        let b = self.primitives.bool;
        for v in values {
            self.unifier.unify(v.custom.unwrap(), b)?;
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
                    self.unifier
                        .unify(self.primitives.int32, v.custom.unwrap())?;
                }
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.unifier.unify(value.custom.unwrap(), list)?;
                Ok(list)
            }
            ast::ExprKind::Constant {
                value: ast::Constant::Int(val),
                ..
            } => {
                // the index is a constant, so value can be a sequence (either list/tuple)
                let ind: i32 = val
                    .try_into()
                    .map_err(|_| "Index must be int32".to_string())?;
                let map = once((ind, ty)).collect();
                let seq = self.unifier.add_ty(TypeEnum::TSeq { map });
                self.unifier.unify(value.custom.unwrap(), seq)?;
                Ok(ty)
            }
            _ => {
                // the index is not a constant, so value can only be a list
                self.unifier
                    .unify(slice.custom.unwrap(), self.primitives.int32)?;
                let list = self.unifier.add_ty(TypeEnum::TList { ty });
                self.unifier.unify(value.custom.unwrap(), list)?;
                Ok(ty)
            }
        }
    }

    fn infer_if_expr(
        &mut self,
        test: &ast::Expr<Option<Type>>,
        body: ast::Expr<Option<Type>>,
        orelse: ast::Expr<Option<Type>>,
    ) -> InferenceResult {
        self.unifier
            .unify(test.custom.unwrap(), self.primitives.bool)?;
        self.unifier
            .unify(body.custom.unwrap(), orelse.custom.unwrap())?;
        Ok(body.custom.unwrap())
    }
}
