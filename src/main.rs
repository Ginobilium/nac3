extern crate inkwell;
extern crate rustpython_parser;

use rustpython_parser::{ast, parser};

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::*;
use inkwell::types;
use crate::inkwell::types::BasicType;

use std::error::Error;
use std::fmt;
use std::path::Path;

#[derive(Debug)]
struct CompileError;

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Compilation error")
    }
}

impl Error for CompileError {}

type CompileResult<T> = Result<T, CompileError>;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context) -> CodeGen<'ctx> {
        CodeGen {
            context,
            module: context.create_module("kernel"),
            builder: context.create_builder()
        }
    }

    fn get_basic_type(&self, name: &str) -> CompileResult<types::BasicTypeEnum<'ctx>> {
        match name {
            "int32" => Ok(self.context.i32_type().into()),
            "int64" => Ok(self.context.i64_type().into()),
            _ => Err(CompileError)
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
            return Err(CompileError)
        }

        let args_type = args.args.iter().map(|val| {
            println!("{:?}", val.annotation);
            if let Some(annotation) = &val.annotation {
                if let ast::ExpressionType::Identifier { name } = &annotation.node {
                    Ok(self.get_basic_type(&name)?)
                } else {
                    Err(CompileError)
                }
            } else {
                Err(CompileError)
            }
        }).collect::<CompileResult<Vec<types::BasicTypeEnum>>>()?;
        let return_type = match returns {
            Some(ast::Located { location, node: ast::ExpressionType::Identifier { name }} )
                => if name == "None" { None } else { Some(self.get_basic_type(name)?) },
            Some(_) => return Err(CompileError),
            None => None,
        };

        let i64_type = self.context.i64_type();
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
            _ => return Err(CompileError),
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

fn main() -> Result<(), Box<dyn Error>> {
    Target::initialize_all(&InitializationConfig::default());

    let ast = parser::parse_program("def foo(x: int32, y: int32) -> int32: return x + y")?;
    println!("AST: {:?}", ast);

    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    codegen.compile_statement(&ast.statements[0])?;
    codegen.output();

    Ok(())
}
