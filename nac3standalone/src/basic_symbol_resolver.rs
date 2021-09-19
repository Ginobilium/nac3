use nac3core::{
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use parking_lot::Mutex;
use std::{collections::HashMap, sync::Arc};

pub struct ResolverInternal {
    pub id_to_type: Mutex<HashMap<String, Type>>,
    pub id_to_def: Mutex<HashMap<String, DefinitionId>>,
    pub class_names: Mutex<HashMap<String, Type>>,
}

impl ResolverInternal {
    pub fn add_id_def(&self, id: String, def: DefinitionId) {
        self.id_to_def.lock().insert(id, def);
    }

    pub fn add_id_type(&self, id: String, ty: Type) {
        self.id_to_type.lock().insert(id, ty);
    }
}

pub struct Resolver(pub Arc<ResolverInternal>);

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: &str) -> Option<Type> {
        let ret = self.0.id_to_type.lock().get(str).cloned();
        if ret.is_none() {
            // println!("unknown here resolver {}", str);
        }
        ret
    }

    fn get_symbol_value(&self, _: &str) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: &str) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: &str) -> Option<DefinitionId> {
        self.0.id_to_def.lock().get(id).cloned()
    }
}
