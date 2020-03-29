extern crate inkwell;
extern crate rustpython_parser;

use rustpython_parser::{ast, parser};

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::*;
use inkwell::types;
use inkwell::types::BasicType;
use inkwell::values;

use std::error::Error;
use std::fmt;
use std::path::Path;
use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
enum CompileErrorKind {
    Unsupported(&'static str),
    MissingTypeAnnotation,
    UnknownTypeAnnotation,
    IncompatibleTypes,
    UnboundIdentifier,
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
            CompileErrorKind::Internal(details)
                => write!(f, "Internal compiler error: {}", details),
        }
    }
}

#[derive(Debug)]
struct CompileError {
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

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_source_location: ast::Location,
    namespace: HashMap<String, values::BasicValueEnum<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module("kernel"),
            builder: context.create_builder(),
            current_source_location: ast::Location::default(),
            namespace: HashMap::new(),
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
        body: &[ast::Statement],
        decorator_list: &[ast::Expression],
        returns: &Option<ast::Expression>,
        is_async: bool,
    ) -> CompileResult<()> {
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
            self.namespace.insert(arg.arg.clone(), function.get_nth_param(n as u32).unwrap());
        }

        for statement in body.iter() {
            self.compile_statement(statement, return_type)?;
        }
        Ok(())
    }

    fn compile_expression(
        &mut self,
        expression: &ast::Expression
    ) -> CompileResult<values::BasicValueEnum<'ctx>> {
        self.set_source_location(expression.location);

        match &expression.node {
            ast::ExpressionType::Identifier { name } => {
                match self.namespace.get(name) {
                    Some(value) => Ok(*value),
                    None => Err(self.compile_error(CompileErrorKind::UnboundIdentifier))
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
                    _ => return Err(self.compile_error(CompileErrorKind::Unsupported("unimplemented operation"))),
                }
            }
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
                        if let Some(existing) = self.namespace.insert(name.clone(), value) {
                            if existing.get_type() != value.get_type() {
                                return Err(self.compile_error(CompileErrorKind::IncompatibleTypes));
                            }
                        }
                    } else {
                        return Err(self.compile_error(CompileErrorKind::Unsupported("assignment target must be an identifier")))
                    }
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

    fn compile_toplevel(&mut self, statement: &ast::Statement) -> CompileResult<()> {
        self.set_source_location(statement.location);
        if let ast::StatementType::FunctionDef {
                    is_async,
                    name,
                    args,
                    body,
                    decorator_list,
                    returns,
                } = &statement.node {
            self.compile_function_def(name, args, body, decorator_list, returns, *is_async)
        } else {
            Err(self.compile_error(CompileErrorKind::Internal("top-level is not a function definition")))
        }
    }

    fn output(&self) {
        let triple = TargetTriple::create("riscv32-none-linux-gnu");
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
            .write_to_file(&self.module, FileType::Object, Path::new("test.o"))
            .expect("couldn't write module to file");
    }
}

fn main() {
    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string("test.py") {
        Ok(program) => program,
        Err(err) => { println!("Cannot open input file: {}", err); return; }
    };
    let ast = match parser::parse_program(&program) {
        Ok(ast) => ast,
        Err(err) => { println!("Parse error: {}", err); return; }
    };

    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    match codegen.compile_toplevel(&ast.statements[0]) {
        Ok(_) => (),
        Err(err) => { println!("Compilation error: {}", err); return; }
    }
    codegen.output();
}
