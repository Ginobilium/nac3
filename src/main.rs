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

use std::error::Error;
use std::fmt;
use std::path::Path;

#[derive(Debug)]
enum CompileErrorKind {
    Unsupported(&'static str),
    MissingTypeAnnotation,
    UnknownTypeAnnotation
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileErrorKind::Unsupported(feature)
                => write!(f, "The following Python feature is not supported by NAC3 {}: ", feature),
            CompileErrorKind::MissingTypeAnnotation
                => write!(f, "Missing type annotation"),
            CompileErrorKind::UnknownTypeAnnotation
                => write!(f, "Unknown type annotation"),
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
        write!(f, "Compilation error at {}: {}", self.location, self.kind)
    }
}

impl Error for CompileError {}

type CompileResult<T> = Result<T, CompileError>;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_source_location: ast::Location,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module("kernel"),
            builder: context.create_builder(),
            current_source_location: ast::Location::default(),
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
                return Err(self.compile_error(CompileErrorKind::Unsupported("complex decorators")))
            }
        }

        let args_type = args.args.iter().map(|val| {
            self.set_source_location(val.location);
            if let Some(annotation) = &val.annotation {
                if let ast::ExpressionType::Identifier { name } = &annotation.node {
                    Ok(self.get_basic_type(&name)?)
                } else {
                    Err(self.compile_error(CompileErrorKind::Unsupported("complex type annotation")))
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
                return Err(self.compile_error(CompileErrorKind::Unsupported("complex type annotation")))
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
        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let sum = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&sum));
        Ok(())
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> CompileResult<()> {
        self.set_source_location(statement.location);

        use ast::StatementType::*;
        match &statement.node {
            FunctionDef {
                is_async,
                name,
                args,
                body,
                decorator_list,
                returns,
            } => {
                self.compile_function_def(name, args, body, decorator_list, returns, *is_async)?;
            },
            Pass => (),
            _ => return Err(self.compile_error(CompileErrorKind::Unsupported("special statement"))),
        }
        Ok(())
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

    let ast = match parser::parse_program("def foo(x: int32, y: int32) -> int32: return x + y") {
        Ok(ast) => ast,
        Err(err) => { println!("{}", err); return; }
    };

    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    match codegen.compile_statement(&ast.statements[0]) {
        Ok(_) => (),
        Err(err) => { println!("{}", err); return; }
    }
    codegen.output();
}
