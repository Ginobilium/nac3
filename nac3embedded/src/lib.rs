use std::collections::HashMap;
use std::collections::hash_map::Entry;

use pyo3::prelude::*;
use pyo3::exceptions;
use rustpython_parser::{ast, parser};
use inkwell::context::Context;
use inkwell::targets::*;

use nac3core::CodeGen;

fn runs_on_core(decorator_list: &[ast::Expression]) -> bool {
    for decorator in decorator_list.iter() {
        if let ast::ExpressionType::Identifier { name } = &decorator.node {
            if name == "kernel" || name == "portable" {
                return true
            }
        }
    }
    false
}

#[pyclass(name=NAC3)]
struct Nac3 {
    type_definitions: HashMap<i64, ast::Program>,
    host_objects: HashMap<i64, i64>,
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new() -> Self {
        Nac3 {
            type_definitions: HashMap::new(),
            host_objects: HashMap::new(),
        }
    }

    fn register_host_object(&mut self, obj: PyObject) -> PyResult<()> {
        Python::with_gil(|py| -> PyResult<()> {
            let obj: &PyAny = obj.extract(py)?;
            let obj_type = obj.get_type();

            let builtins = PyModule::import(py, "builtins")?;
            let type_id = builtins.call1("id", (obj_type, ))?.extract()?;

            let entry = self.type_definitions.entry(type_id);
            if let Entry::Vacant(entry) = entry {
                let source = PyModule::import(py, "inspect")?.call1("getsource", (obj_type, ))?;
                let ast = parser::parse_program(source.extract()?).map_err(|e|
                    exceptions::PySyntaxError::new_err(format!("failed to parse host object source: {}", e)))?;
                entry.insert(ast);
                // TODO: examine AST and recursively register dependencies
            };

            let obj_id = builtins.call1("id", (obj, ))?.extract()?;
            match self.host_objects.entry(obj_id) {
                Entry::Vacant(entry) => entry.insert(type_id),
                Entry::Occupied(_) => return Err(
                    exceptions::PyValueError::new_err("host object registered twice")),
            };
            // TODO: collect other information about host object, e.g. value of fields

            Ok(())
        })
    }

    fn compile_method(&self, obj: PyObject, name: String) -> PyResult<()> {
        Python::with_gil(|py| -> PyResult<()> {
            let obj: &PyAny = obj.extract(py)?;
            let builtins = PyModule::import(py, "builtins")?;
            let obj_id = builtins.call1("id", (obj, ))?.extract()?;

            let type_id = self.host_objects.get(&obj_id).ok_or_else(||
                exceptions::PyKeyError::new_err("type of host object not found"))?;
            let ast = self.type_definitions.get(&type_id).ok_or_else(||
                exceptions::PyKeyError::new_err("type definition not found"))?;

            if let ast::StatementType::ClassDef {
                    name: _,
                    body,
                    bases: _,
                    keywords: _,
                    decorator_list: _ } = &ast.statements[0].node {
                for statement in body.iter() {
                    if let ast::StatementType::FunctionDef {
                            is_async: _,
                            name: funcdef_name,
                            args: _,
                            body: _,
                            decorator_list,
                            returns: _ } = &statement.node {
                        if runs_on_core(decorator_list) && funcdef_name == &name {
                            let context = Context::create();
                            let mut codegen = CodeGen::new(&context);
                            codegen.compile_toplevel(&body[0]).map_err(|e|
                                exceptions::PyRuntimeError::new_err(format!("compilation failed: {}", e)))?;
                            codegen.print_ir();
                        }
                    }
                }
            } else {
                return Err(exceptions::PyValueError::new_err("expected ClassDef for type definition"));
            }

            Ok(())
        })
    }
}

#[pymodule]
fn nac3embedded(_py: Python, m: &PyModule) -> PyResult<()> {
    Target::initialize_all(&InitializationConfig::default());
    m.add_class::<Nac3>()?;
    Ok(())
}
