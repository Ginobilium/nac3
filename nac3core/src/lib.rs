#![warn(clippy::all)]
#![allow(clippy::clone_double_ref)]

extern crate num_bigint;
extern crate inkwell;
extern crate rustpython_parser;

pub mod typedef;
use std::error::Error;
use std::fmt;
use std::path::Path;
use std::collections::HashMap;

use num_traits::cast::ToPrimitive;

use rustpython_parser::ast;

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::*;
use inkwell::types;
use inkwell::types::BasicType;
use inkwell::values;
use inkwell::{IntPredicate, FloatPredicate};
use inkwell::basic_block;
use inkwell::passes;


#[derive(Debug)]
enum CompileErrorKind {
    Unsupported(&'static str),
    MissingTypeAnnotation,
    UnknownTypeAnnotation,
    IncompatibleTypes,
    UnboundIdentifier,
    BreakOutsideLoop,
    Internal(&'static str)
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileErrorKind::Unsupported(feature)
                => write!(f, "The following Python feature is not supported by NAC3: {}", feature),
            CompileErrorKind::MissingTypeAnnotation
                => write!(f, "Missing type annotation"),
            CompileErrorKind::UnknownTypeAnnotation
                => write!(f, "Unknown type annotation"),
            CompileErrorKind::IncompatibleTypes
                => write!(f, "Incompatible types"),
            CompileErrorKind::UnboundIdentifier
                => write!(f, "Unbound identifier"),
            CompileErrorKind::BreakOutsideLoop
                => write!(f, "Break outside loop"),
            CompileErrorKind::Internal(details)
                => write!(f, "Internal compiler error: {}", details),
        }
    }
}

#[derive(Debug)]
pub struct CompileError {
    location: ast::Location,
    kind: CompileErrorKind,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, at {}", self.kind, self.location)
    }
}

impl Error for CompileError {}

type CompileResult<T> = Result<T, CompileError>;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    pass_manager: passes::PassManager<values::FunctionValue<'ctx>>,
    builder: Builder<'ctx>,
    current_source_location: ast::Location,
    namespace: HashMap<String, values::PointerValue<'ctx>>,
    break_bb: Option<basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> CodeGen<'ctx> {
        let module = context.create_module("kernel");

        let pass_manager = passes::PassManager::create(&module);
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_basic_alias_analysis_pass();
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.initialize();

        let i32_type = context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into()], false);
        module.add_function("output", fn_type, None);

        CodeGen {
            context, module, pass_manager,
            builder: context.create_builder(),
            current_source_location: ast::Location::default(),
            namespace: HashMap::new(),
            break_bb: None,
        }
    }

    fn set_source_location(&mut self, location: ast::Location) {
        self.current_source_location = location;
    }

    fn compile_error(&self, kind: CompileErrorKind) -> CompileError {
        CompileError {
            location: self.current_source_location,
            kind
        }
    }

    fn get_basic_type(&self, name: &str) -> CompileResult<types::BasicTypeEnum<'ctx>> {
        match name {
            "bool" => Ok(self.context.bool_type().into()),
            "int32" => Ok(self.context.i32_type().into()),
            "int64" => Ok(self.context.i64_type().into()),
            "float32" => Ok(self.context.f32_type().into()),
            "float64" => Ok(self.context.f64_type().into()),
            _ => Err(self.compile_error(CompileErrorKind::UnknownTypeAnnotation))
        }
    }

    fn compile_function_def(
        &mut self,
        name: &str,
        args: &ast::Parameters,
        body: &ast::Suite,
        decorator_list: &[ast::Expression],
        returns: &Option<ast::Expression>,
        is_async: bool,
    ) -> CompileResult<values::FunctionValue<'ctx>> {
        if is_async {
            return Err(self.compile_error(CompileErrorKind::Unsupported("async functions")))
        }
        for decorator in decorator_list.iter() {
            self.set_source_location(decorator.location);
            if let ast::ExpressionType::Identifier { name } = &decorator.node {
                if name != "kernel" && name != "portable" {
                    return Err(self.compile_error(CompileErrorKind::Unsupported("custom decorators")))
                }
            } else {
                return Err(self.compile_error(CompileErrorKind::Unsupported("decorator must be an identifier")))
            }
        }

        let args_type = args.args.iter().map(|val| {
            self.set_source_location(val.location);
            if let Some(annotation) = &val.annotation {
                if let ast::ExpressionType::Identifier { name } = &annotation.node {
                    Ok(self.get_basic_type(&name)?)
                } else {
                    Err(self.compile_error(CompileErrorKind::Unsupported("type annotation must be an identifier")))
                }
            } else {
                Err(self.compile_error(CompileErrorKind::MissingTypeAnnotation))
            }
        }).collect::<CompileResult<Vec<types::BasicTypeEnum>>>()?;
        let return_type = if let Some(returns) = returns {
            self.set_source_location(returns.location);
            if let ast::ExpressionType::Identifier { name } = &returns.node {
                if name == "None" { None } else { Some(self.get_basic_type(name)?) }
            } else {
                return Err(self.compile_error(CompileErrorKind::Unsupported("type annotation must be an identifier")))
            }
        } else {
            None
        };

        let fn_type = match return_type {
            Some(ty) => ty.fn_type(&args_type, false),
            None => self.context.void_type().fn_type(&args_type, false)
        };

        let function = self.module.add_function(name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        for (n, arg) in args.args.iter().enumerate() {
            let param = function.get_nth_param(n as u32).unwrap();
            let alloca = self.builder.build_alloca(param.get_type(), &arg.arg);
            self.builder.build_store(alloca, param);
            self.namespace.insert(arg.arg.clone(), alloca);
        }

        self.compile_suite(body, return_type)?;

        Ok(function)
    }

    fn compile_expression(
        &mut self,
        expression: &ast::Expression
    ) -> CompileResult<values::BasicValueEnum<'ctx>> {
        self.set_source_location(expression.location);

        match &expression.node {
            ast::ExpressionType::True => Ok(self.context.bool_type().const_int(1, false).into()),
            ast::ExpressionType::False => Ok(self.context.bool_type().const_int(0, false).into()),
            ast::ExpressionType::Number { value: ast::Number::Integer { value } } => {
                let mut bits = value.bits();
                if value.sign() == num_bigint::Sign::Minus {
                    bits += 1;
                }
                match bits {
                     0..=32 => Ok(self.context.i32_type().const_int(value.to_i32().unwrap() as _, true).into()),
                    33..=64 => Ok(self.context.i64_type().const_int(value.to_i64().unwrap() as _, true).into()),
                          _ => Err(self.compile_error(CompileErrorKind::Unsupported("integers larger than 64 bits")))
                }
            },
            ast::ExpressionType::Number { value: ast::Number::Float { value } } => {
                Ok(self.context.f64_type().const_float(*value).into())
            },
            ast::ExpressionType::Identifier { name } => {
                match self.namespace.get(name) {
                    Some(value) => Ok(self.builder.build_load(*value, name).into()),
                    None => Err(self.compile_error(CompileErrorKind::UnboundIdentifier))
                }
            },
            ast::ExpressionType::Unop { op, a } => {
                let a = self.compile_expression(&a)?;
                match (op, a) {
                    (ast::UnaryOperator::Pos, values::BasicValueEnum::IntValue(a))
                        => Ok(a.into()),
                    (ast::UnaryOperator::Pos, values::BasicValueEnum::FloatValue(a))
                        => Ok(a.into()),
                    (ast::UnaryOperator::Neg, values::BasicValueEnum::IntValue(a))
                        => Ok(self.builder.build_int_neg(a, "tmpneg").into()),
                    (ast::UnaryOperator::Neg, values::BasicValueEnum::FloatValue(a))
                        => Ok(self.builder.build_float_neg(a, "tmpneg").into()),
                    (ast::UnaryOperator::Inv, values::BasicValueEnum::IntValue(a))
                        => Ok(self.builder.build_not(a, "tmpnot").into()),
                    (ast::UnaryOperator::Not, values::BasicValueEnum::IntValue(a)) => {
                        // boolean "not"
                        if a.get_type().get_bit_width() != 1 {
                            Err(self.compile_error(CompileErrorKind::Unsupported("unimplemented unary operation")))
                        } else {
                            Ok(self.builder.build_not(a, "tmpnot").into())
                        }
                    },
                    _ => Err(self.compile_error(CompileErrorKind::Unsupported("unimplemented unary operation"))),
                }
            },
            ast::ExpressionType::Binop { a, op, b } => {
                let a = self.compile_expression(&a)?;
                let b = self.compile_expression(&b)?;
                if a.get_type() != b.get_type() {
                    return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                }
                use ast::Operator::*;
                match (op, a, b) {
                    (Add, values::BasicValueEnum::IntValue(a), values::BasicValueEnum::IntValue(b))
                        => Ok(self.builder.build_int_add(a, b, "tmpadd").into()),
                    (Sub, values::BasicValueEnum::IntValue(a), values::BasicValueEnum::IntValue(b))
                        => Ok(self.builder.build_int_sub(a, b, "tmpsub").into()),
                    (Mult, values::BasicValueEnum::IntValue(a), values::BasicValueEnum::IntValue(b))
                        => Ok(self.builder.build_int_mul(a, b, "tmpmul").into()),

                    (Add, values::BasicValueEnum::FloatValue(a), values::BasicValueEnum::FloatValue(b))
                        => Ok(self.builder.build_float_add(a, b, "tmpadd").into()),
                    (Sub, values::BasicValueEnum::FloatValue(a), values::BasicValueEnum::FloatValue(b))
                        => Ok(self.builder.build_float_sub(a, b, "tmpsub").into()),
                    (Mult, values::BasicValueEnum::FloatValue(a), values::BasicValueEnum::FloatValue(b))
                        => Ok(self.builder.build_float_mul(a, b, "tmpmul").into()),

                    (Div, values::BasicValueEnum::FloatValue(a), values::BasicValueEnum::FloatValue(b))
                        => Ok(self.builder.build_float_div(a, b, "tmpdiv").into()),
                    (FloorDiv, values::BasicValueEnum::IntValue(a), values::BasicValueEnum::IntValue(b))
                        => Ok(self.builder.build_int_signed_div(a, b, "tmpdiv").into()),
                    _ => Err(self.compile_error(CompileErrorKind::Unsupported("unimplemented binary operation"))),
                }
            },
            ast::ExpressionType::Compare { vals, ops } => {
                let mut vals = vals.iter();
                let mut ops = ops.iter();

                let mut result = None;
                let mut a = self.compile_expression(vals.next().unwrap())?;
                loop {
                    if let Some(op) = ops.next() {
                        let b = self.compile_expression(vals.next().unwrap())?;
                        if a.get_type() != b.get_type() {
                            return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                        }
                        let this_result = match (a, b) {
                            (values::BasicValueEnum::IntValue(a), values::BasicValueEnum::IntValue(b)) => {
                                match op {
                                    ast::Comparison::Equal
                                        => self.builder.build_int_compare(IntPredicate::EQ, a, b, "tmpeq"),
                                    ast::Comparison::NotEqual
                                        => self.builder.build_int_compare(IntPredicate::NE, a, b, "tmpne"),
                                    ast::Comparison::Less
                                        => self.builder.build_int_compare(IntPredicate::SLT, a, b, "tmpslt"),
                                    ast::Comparison::LessOrEqual
                                        => self.builder.build_int_compare(IntPredicate::SLE, a, b, "tmpsle"),
                                    ast::Comparison::Greater
                                        => self.builder.build_int_compare(IntPredicate::SGT, a, b, "tmpsgt"),
                                    ast::Comparison::GreaterOrEqual
                                        => self.builder.build_int_compare(IntPredicate::SGE, a, b, "tmpsge"),
                                    _ => return Err(self.compile_error(CompileErrorKind::Unsupported("special comparison"))),
                                }
                            },
                            (values::BasicValueEnum::FloatValue(a), values::BasicValueEnum::FloatValue(b)) => {
                                match op {
                                    ast::Comparison::Equal
                                        => self.builder.build_float_compare(FloatPredicate::OEQ, a, b, "tmpoeq"),
                                    ast::Comparison::NotEqual
                                        => self.builder.build_float_compare(FloatPredicate::UNE, a, b, "tmpune"),
                                    ast::Comparison::Less
                                        => self.builder.build_float_compare(FloatPredicate::OLT, a, b, "tmpolt"),
                                    ast::Comparison::LessOrEqual
                                        => self.builder.build_float_compare(FloatPredicate::OLE, a, b, "tmpole"),
                                    ast::Comparison::Greater
                                        => self.builder.build_float_compare(FloatPredicate::OGT, a, b, "tmpogt"),
                                    ast::Comparison::GreaterOrEqual
                                        => self.builder.build_float_compare(FloatPredicate::OGE, a, b, "tmpoge"),
                                    _ => return Err(self.compile_error(CompileErrorKind::Unsupported("special comparison"))),
                                }
                            },
                            _ => return Err(self.compile_error(CompileErrorKind::Unsupported("comparison of non-numerical types"))),
                        };
                        match result {
                            Some(last) => {
                                result = Some(self.builder.build_and(last, this_result, "tmpand"));
                            }
                            None => {
                                result = Some(this_result);
                            }
                        }
                        a = b;
                    } else {
                        return Ok(result.unwrap().into())
                    }
                }
            },
            ast::ExpressionType::Call { function, args, keywords } => {
                if !keywords.is_empty() {
                    return Err(self.compile_error(CompileErrorKind::Unsupported("keyword arguments")))
                }
                let args = args.iter().map(|val| self.compile_expression(val))
                    .collect::<CompileResult<Vec<values::BasicValueEnum>>>()?;
                self.set_source_location(expression.location);
                if let ast::ExpressionType::Identifier { name } = &function.node {
                    match (name.as_str(), args[0]) {
                        ("int32", values::BasicValueEnum::IntValue(a)) => {
                            let nbits = a.get_type().get_bit_width();
                            if nbits < 32 {
                                Ok(self.builder.build_int_s_extend(a, self.context.i32_type(), "tmpsext").into())
                            } else if nbits > 32 {
                                Ok(self.builder.build_int_truncate(a, self.context.i32_type(), "tmptrunc").into())
                            } else {
                                Ok(a.into())
                            }
                        },
                        ("int64", values::BasicValueEnum::IntValue(a)) => {
                            let nbits = a.get_type().get_bit_width();
                            if nbits < 64 {
                                Ok(self.builder.build_int_s_extend(a, self.context.i64_type(), "tmpsext").into())
                            } else {
                                Ok(a.into())
                            }
                        },
                        ("int32", values::BasicValueEnum::FloatValue(a)) => {
                            Ok(self.builder.build_float_to_signed_int(a, self.context.i32_type(), "tmpfptosi").into())
                        },
                        ("int64", values::BasicValueEnum::FloatValue(a)) => {
                            Ok(self.builder.build_float_to_signed_int(a, self.context.i64_type(), "tmpfptosi").into())
                        },
                        ("float32", values::BasicValueEnum::IntValue(a)) => {
                            Ok(self.builder.build_signed_int_to_float(a, self.context.f32_type(), "tmpsitofp").into())
                        },
                        ("float64", values::BasicValueEnum::IntValue(a)) => {
                            Ok(self.builder.build_signed_int_to_float(a, self.context.f64_type(), "tmpsitofp").into())
                        },
                        ("float32", values::BasicValueEnum::FloatValue(a)) => {
                            if a.get_type() == self.context.f64_type() {
                                Ok(self.builder.build_float_trunc(a, self.context.f32_type(), "tmptrunc").into())
                            } else {
                                Ok(a.into())
                            }
                        },
                        ("float64", values::BasicValueEnum::FloatValue(a)) => {
                            if a.get_type() == self.context.f32_type() {
                                Ok(self.builder.build_float_ext(a, self.context.f64_type(), "tmpext").into())
                            } else {
                                Ok(a.into())
                            }
                        },

                        ("output", values::BasicValueEnum::IntValue(a)) => {
                            let fn_value = self.module.get_function("output").unwrap();
                            Ok(self.builder.build_call(fn_value, &[a.into()], "call")
                                .try_as_basic_value().left().unwrap())
                        },
                        _ => Err(self.compile_error(CompileErrorKind::Unsupported("unrecognized call")))
                    }
                } else {
                    return Err(self.compile_error(CompileErrorKind::Unsupported("function must be an identifier")))
                }
            },
            _ => return Err(self.compile_error(CompileErrorKind::Unsupported("unimplemented expression"))),
        }
    }

    fn compile_statement(
        &mut self,
        statement: &ast::Statement,
        return_type: Option<types::BasicTypeEnum>
    ) -> CompileResult<()> {
        self.set_source_location(statement.location);

        use ast::StatementType::*;
        match &statement.node {
            Assign { targets, value } => {
                let value = self.compile_expression(value)?;
                for target in targets.iter() {
                    self.set_source_location(target.location);
                    if let ast::ExpressionType::Identifier { name } = &target.node {
                        let builder = &self.builder;
                        let target = self.namespace.entry(name.clone()).or_insert_with(
                            || builder.build_alloca(value.get_type(), name));
                        if target.get_type() != value.get_type().ptr_type(inkwell::AddressSpace::Generic) {
                            return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                        }
                        builder.build_store(*target, value);
                    } else {
                        return Err(self.compile_error(CompileErrorKind::Unsupported("assignment target must be an identifier")))
                    }
                }
            },
            Expression { expression } => { self.compile_expression(expression)?; },
            If { test, body, orelse } => {
                let test = self.compile_expression(test)?;
                if test.get_type() != self.context.bool_type().into() {
                    return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                }

                let parent = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");
                self.builder.build_conditional_branch(test.into_int_value(), then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                self.compile_suite(body, return_type)?;
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                if let Some(orelse) = orelse {
                    self.compile_suite(orelse, return_type)?;
                }
                self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(cont_bb);
            },
            While { test, body, orelse } => {
                let parent = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let test_bb = self.context.append_basic_block(parent, "test");
                self.builder.build_unconditional_branch(test_bb);
                self.builder.position_at_end(test_bb);
                let test = self.compile_expression(test)?;
                if test.get_type() != self.context.bool_type().into() {
                    return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                }

                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");
                self.builder.build_conditional_branch(test.into_int_value(), then_bb, else_bb);

                self.break_bb = Some(cont_bb);

                self.builder.position_at_end(then_bb);
                self.compile_suite(body, return_type)?;
                self.builder.build_unconditional_branch(test_bb);

                self.builder.position_at_end(else_bb);
                if let Some(orelse) = orelse {
                    self.compile_suite(orelse, return_type)?;
                }
                self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(cont_bb);

                self.break_bb = None;
            },
            Break => {
                if let Some(bb) = self.break_bb {
                    self.builder.build_unconditional_branch(bb);
                    let parent = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                    let unreachable_bb = self.context.append_basic_block(parent, "unreachable");
                    self.builder.position_at_end(unreachable_bb);
                } else {
                    return Err(self.compile_error(CompileErrorKind::BreakOutsideLoop));
                }
            }
            Return { value: Some(value) } => {
                if let Some(return_type) = return_type {
                    let value = self.compile_expression(value)?;
                    if value.get_type() != return_type {
                        return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                    }
                    self.builder.build_return(Some(&value));
                } else {
                    return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                }
            },
            Return { value: None } => {
                if !return_type.is_none() {
                    return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                }
                self.builder.build_return(None);
            },
            Pass => (),
            _ => return Err(self.compile_error(CompileErrorKind::Unsupported("special statement"))),
        }
        Ok(())
    }

    fn compile_suite(
        &mut self,
        suite: &ast::Suite,
        return_type: Option<types::BasicTypeEnum>
    ) -> CompileResult<()> {
        for statement in suite.iter() {
            self.compile_statement(statement, return_type)?;
        }
        Ok(())
    }

    pub fn compile_toplevel(&mut self, statement: &ast::Statement) -> CompileResult<()> {
        self.set_source_location(statement.location);
        if let ast::StatementType::FunctionDef {
                    is_async,
                    name,
                    args,
                    body,
                    decorator_list,
                    returns,
                } = &statement.node {
            let function = self.compile_function_def(name, args, body, decorator_list, returns, *is_async)?;
            self.pass_manager.run_on(&function);
            Ok(())
        } else {
            Err(self.compile_error(CompileErrorKind::Internal("top-level is not a function definition")))
        }
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn output(&self, filename: &str) {
        //let triple = TargetTriple::create("riscv32-none-linux-gnu");
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .expect("couldn't create target from target triple");

        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("couldn't create target machine");

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(filename))
            .expect("couldn't write module to file");
    }
}
