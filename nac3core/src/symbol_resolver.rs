use crate::location::Location;
use crate::top_level::DefinitionId;
use crate::typecheck::typedef::Type;
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
    fn get_symbol_type(&self, str: &str) -> Option<Type>;
    fn parse_type_name(&self, expr: &Expr<()>) -> Option<Type>;
    fn get_identifier_def(&self, str: &str) -> DefinitionId;
    fn get_symbol_value(&self, str: &str) -> Option<SymbolValue>;
    fn get_symbol_location(&self, str: &str) -> Option<Location>;
    fn get_module_resolver(&self, module_name: &str) -> Option<&dyn SymbolResolver>; // NOTE: for getting imported modules' symbol resolver?
                                                                                     // handle function call etc.
}
