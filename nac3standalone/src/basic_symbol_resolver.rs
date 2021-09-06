use nac3core::{
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Resolver {
    pub id_to_type: HashMap<String, Type>,
    pub id_to_def: HashMap<String, DefinitionId>,
    pub class_names: HashMap<String, Type>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: &str) -> Option<Type> {
        self.id_to_type.get(str).cloned()
    }

    fn get_symbol_value(&self, _: &str) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: &str) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: &str) -> Option<DefinitionId> {
        self.id_to_def.get(id).cloned()
    }

    fn add_id_def(&mut self, _: String, _: DefinitionId) {
        unimplemented!();
    }
}
