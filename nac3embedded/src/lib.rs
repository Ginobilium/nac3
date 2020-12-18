use pyo3::prelude::*;
use pyo3::wrap_pyfunction;

#[pyfunction]
fn add_host_object(obj: PyObject) -> PyResult<()> {
    Python::with_gil(|py| -> PyResult<()> {
        let obj: &PyAny = obj.extract(py)?;
        let inspect = PyModule::import(py, "inspect")?;
        let source = inspect.call1("getsource", (obj.get_type(), ))?;
        println!("source:\n{}", source);
        Ok(())
    })?;
    Ok(())
}

#[pymodule]
fn nac3embedded(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(add_host_object, m)?)?;

    Ok(())
}
