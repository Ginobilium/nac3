use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::once;
use std::rc::Rc;

use super::magic_methods::*;
use super::symbol_resolver::SymbolResolver;
use super::typedef::{Call, FunSignature, FuncArg, Type, TypeEnum, Unifier};
use itertools::izip;
use rustpython_parser::ast::{
    self,
    fold::{self, Fold},
    Arguments, Comprehension, ExprKind, Located, Location,
};

#[cfg(test)]
mod test;

pub struct PrimitiveStore {
    pub int32: Type,
    pub int64: Type,
    pub float: Type,
    pub bool: Type,
    pub none: Type,
}

pub struct Inferencer<'a> {
    pub resolver: &'a mut Box<dyn SymbolResolver>,
    pub unifier: &'a mut Unifier,
    pub variable_mapping: HashMap<String, Type>,
    pub calls: &'a mut Vec<Rc<Call>>,
    pub primitives: &'a PrimitiveStore,
    pub return_type: Option<Type>,
}

struct NaiveFolder();
impl fold::Fold<()> for NaiveFolder {
    type TargetU = Option<Type>;
    type Error = String;
    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }
}

impl<'a> fold::Fold<()> for Inferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, _: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }

    fn fold_stmt(&mut self, node: ast::Stmt<()>) -> Result<ast::Stmt<Self::TargetU>, Self::Error> {
        let stmt = match node.node {
            // we don't want fold over type annotation
            ast::StmtKind::AnnAssign {
                target,
                annotation,
                value,
                simple,
            } => {
                let target = Box::new(fold::fold_expr(self, *target)?);
                let value = if let Some(v) = value {
                    let ty = Box::new(fold::fold_expr(self, *v)?);
                    self.unifier
                        .unify(target.custom.unwrap(), ty.custom.unwrap())?;
                    Some(ty)
                } else {
                    None
                };
                let annotation_type = self
                    .resolver
                    .parse_type_name(annotation.as_ref())
                    .ok_or_else(|| "cannot parse type name".to_string())?;
                self.unifier
                    .unify(annotation_type, target.custom.unwrap())?;
                let annotation = Box::new(NaiveFolder().fold_expr(*annotation)?);
                Located {
                    location: node.location,
                    custom: None,
                    node: ast::StmtKind::AnnAssign {
                        target,
                        annotation,
                        value,
                        simple,
                    },
                }
            }
            _ => fold::fold_stmt(self, node)?,
        };
        match &stmt.node {
            ast::StmtKind::For { target, iter, .. } => {
                let list = self.unifier.add_ty(TypeEnum::TList {
                    ty: target.custom.unwrap(),
                });
                self.unifier.unify(list, iter.custom.unwrap())?;
            }
            ast::StmtKind::If { test, .. } | ast::StmtKind::While { test, .. } => {
                self.unifier
                    .unify(test.custom.unwrap(), self.primitives.bool)?;
            }
            ast::StmtKind::Assign { targets, value, .. } => {
                for target in targets.iter() {
                    self.unifier
                        .unify(target.custom.unwrap(), value.custom.unwrap())?;
                }
            }
            ast::StmtKind::AnnAssign { .. } | ast::StmtKind::Expr { .. } => {}
            ast::StmtKind::Return { value } => match (value, self.return_type) {
                (Some(v), Some(v1)) => {
                    self.unifier.unify(v.custom.unwrap(), v1)?;
                }
                (Some(_), None) => {
                    return Err("Unexpected return value".to_string());
                }
                (None, Some(_)) => {
                    return Err("Expected return value".to_string());
                }
                (None, None) => {}
            },
            _ => return Err("Unsupported statement type".to_string()),
        };
        Ok(stmt)
    }

    fn fold_expr(&mut self, node: ast::Expr<()>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        let expr = match node.node {
            ast::ExprKind::Call {
                func,
                args,
                keywords,
            } => self.fold_call(node.location, *func, args, keywords)?,
            ast::ExprKind::Lambda { args, body } => {
                self.fold_lambda(node.location, *args, *body)?
            }
            ast::ExprKind::ListComp { elt, generators } => {
                self.fold_listcomp(node.location, *elt, generators)?
            }
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
            ast::ExprKind::BoolOp { values, .. } => Some(self.infer_bool_ops(values)?),
            ast::ExprKind::BinOp { left, op, right } => Some(self.infer_bin_ops(left, op, right)?),
            ast::ExprKind::UnaryOp { op, operand } => Some(self.infer_unary_ops(op, operand)?),
            ast::ExprKind::Compare {
                left,
                ops,
                comparators,
            } => Some(self.infer_compare(left, ops, comparators)?),
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
            return_type: self.return_type,
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

    fn fold_listcomp(
        &mut self,
        location: Location,
        elt: ast::Expr<()>,
        mut generators: Vec<Comprehension>,
    ) -> Result<ast::Expr<Option<Type>>, String> {
        if generators.len() != 1 {
            return Err(
                "Only 1 generator statement for list comprehension is supported.".to_string(),
            );
        }
        let variable_mapping = self.variable_mapping.clone();
        let mut new_context = Inferencer {
            resolver: self.resolver,
            unifier: self.unifier,
            variable_mapping,
            calls: self.calls,
            primitives: self.primitives,
            return_type: self.return_type,
        };
        let elt = new_context.fold_expr(elt)?;
        let generator = generators.pop().unwrap();
        if generator.is_async {
            return Err("Async iterator not supported.".to_string());
        }
        let target = new_context.fold_expr(*generator.target)?;
        let iter = new_context.fold_expr(*generator.iter)?;
        let ifs: Vec<_> = generator
            .ifs
            .into_iter()
            .map(|v| new_context.fold_expr(v))
            .collect::<Result<_, _>>()?;

        // iter should be a list of targets...
        // actually it should be an iterator of targets, but we don't have iter type for now
        let list = new_context.unifier.add_ty(TypeEnum::TList {
            ty: target.custom.unwrap(),
        });
        new_context.unifier.unify(iter.custom.unwrap(), list)?;
        // if conditions should be bool
        for v in ifs.iter() {
            new_context
                .unifier
                .unify(v.custom.unwrap(), new_context.primitives.bool)?;
        }

        Ok(Located {
            location,
            custom: Some(new_context.unifier.add_ty(TypeEnum::TList {
                ty: elt.custom.unwrap(),
            })),
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
        let func = if let Located {
            location: func_location,
            custom,
            node: ExprKind::Name { id, ctx },
        } = func
        {
            // handle special functions that cannot be typed in the usual way...
            if id == "virtual" {
                if args.is_empty() || args.len() > 2 || !keywords.is_empty() {
                    return Err("`virtual` can only accept 1/2 positional arguments.".to_string());
                }
                let arg0 = self.fold_expr(args.remove(0))?;
                let ty = if let Some(arg) = args.pop() {
                    self.resolver
                        .parse_type_name(&arg)
                        .ok_or_else(|| "error parsing type".to_string())?
                } else {
                    self.unifier.get_fresh_var().0
                };
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
            if id == "int64" && args.len() == 1 {
                if let ExprKind::Constant {
                    value: ast::Constant::Int(val),
                    kind,
                } = &args[0].node
                {
                    let int64: Result<i64, _> = val.try_into();
                    let custom;
                    if int64.is_ok() {
                        custom = Some(self.primitives.int64);
                    } else {
                        return Err("Integer out of bound".into());
                    }
                    return Ok(Located {
                        location: args[0].location,
                        custom,
                        node: ExprKind::Constant {
                            value: ast::Constant::Int(val.clone()),
                            kind: kind.clone(),
                        },
                    });
                }
            }
            Located {
                location: func_location,
                custom,
                node: ExprKind::Name { id, ctx },
            }
        } else {
            func
        };
        let func = Box::new(self.fold_expr(func)?);
        let args = args
            .into_iter()
            .map(|v| self.fold_expr(v))
            .collect::<Result<Vec<_>, _>>()?;
        let keywords = keywords
            .into_iter()
            .map(|v| fold::fold_keyword(self, v))
            .collect::<Result<Vec<_>, _>>()?;
        let ret = self.unifier.get_fresh_var().0;
        let call = Rc::new(Call {
            posargs: args.iter().map(|v| v.custom.unwrap()).collect(),
            kwargs: keywords
                .iter()
                .map(|v| (v.node.arg.as_ref().unwrap().clone(), v.custom.unwrap()))
                .collect(),
            fun: RefCell::new(None),
            ret,
        });
        self.calls.push(call.clone());
        let call = self.unifier.add_ty(TypeEnum::TCall { calls: vec![call] });
        self.unifier.unify(func.custom.unwrap(), call)?;

        Ok(Located {
            location,
            custom: Some(ret),
            node: ExprKind::Call {
                func,
                args,
                keywords,
            },
        })
    }

    fn infer_identifier(&mut self, id: &str) -> InferenceResult {
        if let Some(ty) = self.variable_mapping.get(id) {
            Ok(*ty)
        } else {
            Ok(self.resolver.get_symbol_type(id).unwrap_or_else(|| {
                let ty = self.unifier.get_fresh_var().0;
                self.variable_mapping.insert(id.to_string(), ty);
                ty
            }))
        }
    }

    fn infer_constant(&mut self, constant: &ast::Constant) -> InferenceResult {
        match constant {
            ast::Constant::Bool(_) => Ok(self.primitives.bool),
            ast::Constant::Int(val) => {
                let int32: Result<i32, _> = val.try_into();
                // int64 would be handled separately in functions
                if int32.is_ok() {
                    Ok(self.primitives.int32)
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
        Ok(self.unifier.add_ty(TypeEnum::TList { ty }))
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
