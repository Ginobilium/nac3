use std::fs;

use inkwell::context::Context;
use inkwell::targets::*;
use rustpython_parser::parser;

use nac3core::CodeGen;


fn main() {
    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string("mandelbrot.py") {
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
    codegen.print_ir();
    codegen.output("mandelbrot.o");
}
