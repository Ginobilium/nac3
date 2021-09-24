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
use rustpython_parser::ast::StrRef;
use std::{collections::HashMap, sync::Arc};

pub struct ResolverInternal {
    pub id_to_type: Mutex<HashMap<StrRef, Type>>,
    pub id_to_def: Mutex<HashMap<StrRef, DefinitionId>>,
    pub class_names: Mutex<HashMap<StrRef, Type>>,
}

impl ResolverInternal {
    pub fn add_id_def(&self, id: StrRef, def: DefinitionId) {
        self.id_to_def.lock().insert(id, def);
    }

    pub fn add_id_type(&self, id: StrRef, ty: Type) {
        self.id_to_type.lock().insert(id, ty);
    }
}

pub struct Resolver(pub Arc<ResolverInternal>);

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: StrRef) -> Option<Type> {
        let ret = self.0.id_to_type.lock().get(&str).cloned();
        if ret.is_none() {
            // println!("unknown here resolver {}", str);
        }
        ret
    }

    fn get_symbol_value(&self, _: StrRef) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        self.0.id_to_def.lock().get(&id).cloned()
    }
}
