use nac3core::{
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use parking_lot::{Mutex, RwLock};
use rustpython_parser::ast::StrRef;
use std::{collections::HashMap, sync::Arc};

pub struct Resolver {
    pub id_to_type: Mutex<HashMap<StrRef, Type>>,
    pub id_to_def: Mutex<HashMap<StrRef, DefinitionId>>,
    pub class_names: Mutex<HashMap<StrRef, Type>>,
    pub pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pub pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    // module specific
    pub name_to_pyid: HashMap<StrRef, u64>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: StrRef) -> Option<Type> {
        let mut id_to_type = self.id_to_type.lock();
        id_to_type.get(&str).cloned().or_else(|| {
            let py_id = self.name_to_pyid.get(&str);
            let result = py_id.and_then(|id| self.pyid_to_type.read().get(&id).copied());
            if let Some(result) = &result {
                id_to_type.insert(str, *result);
            }
            result
        })
    }

    fn get_symbol_value(&self, _: StrRef) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        let mut id_to_def = self.id_to_def.lock();
        id_to_def.get(&id).cloned().or_else(|| {
            let py_id = self.name_to_pyid.get(&id);
            let result = py_id.and_then(|id| self.pyid_to_def.read().get(&id).copied());
            if let Some(result) = &result {
                id_to_def.insert(id, *result);
            }
            result
        })
    }
}
