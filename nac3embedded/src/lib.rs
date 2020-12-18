use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
use pyo3::exceptions;
use rustpython_parser::parser;

#[pyfunction]
fn add_host_object(obj: PyObject) -> PyResult<()> {
    Python::with_gil(|py| -> PyResult<()> {
        let obj: &PyAny = obj.extract(py)?;
        let inspect = PyModule::import(py, "inspect")?;
        let source = inspect.call1("getsource", (obj.get_type(), ))?;
        let ast = parser::parse_program(source.extract()?).map_err(|e|
            exceptions::PySyntaxError::new_err(format!("failed to parse host object source: {}", e)))?;
        println!("{:?}", ast);
        Ok(())
    })?;
    Ok(())
}

#[pymodule]
fn nac3embedded(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(add_host_object, m)?)?;
    Ok(())
}
