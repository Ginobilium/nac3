use crate::location::Location;
use crate::typecheck::typedef::Type;
use crate::top_level::DefinitionId;
use rustpython_parser::ast::Expr;

#[derive(Clone, PartialEq)]
pub enum SymbolValue {
    I32(i32),
    I64(i64),
    Double(f64),
    Bool(bool),
    Tuple(Vec<SymbolValue>),
    // we should think about how to implement bytes later...
    // Bytes(&'a [u8]),
}

pub trait SymbolResolver {
    fn get_symbol_type(&mut self, str: &str) -> Option<Type>;
    fn parse_type_name(&mut self, expr: &Expr<()>) -> Option<Type>;
    fn get_function_def(&mut self, str: &str) -> DefinitionId;
    fn get_symbol_value(&mut self, str: &str) -> Option<SymbolValue>;
    fn get_symbol_location(&mut self, str: &str) -> Option<Location>;
    // handle function call etc.
}
