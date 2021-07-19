use super::typedef::Type;
use super::location::Location;

pub enum SymbolType {
    TypeName(Type),
    Identifier(Type),
}

pub enum SymbolValue<'a> {
    I32(i32),
    I64(i64),
    Double(f64),
    Bool(bool),
    Tuple(&'a [SymbolValue<'a>]),
    Bytes(&'a [u8]),
}

pub trait SymbolResolver {
    fn get_symbol_type(&mut self, str: &str) -> Option<SymbolType>;
    fn get_symbol_value(&mut self, str: &str) -> Option<SymbolValue>;
    fn get_symbol_location(&mut self, str: &str) -> Option<Location>;
    // handle function call etc.
}
