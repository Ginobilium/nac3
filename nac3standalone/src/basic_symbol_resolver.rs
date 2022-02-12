use nac3core::{
    codegen::CodeGenContext,
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue, ValueEnum},
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use nac3parser::ast::{self, StrRef};
use parking_lot::{Mutex, RwLock};
use std::{collections::HashMap, sync::Arc};

pub struct ResolverInternal {
    pub id_to_type: Mutex<HashMap<StrRef, Type>>,
    pub id_to_def: Mutex<HashMap<StrRef, DefinitionId>>,
    pub class_names: Mutex<HashMap<StrRef, Type>>,
    pub module_globals: Mutex<HashMap<StrRef, SymbolValue>>,
}

impl ResolverInternal {
    pub fn add_id_def(&self, id: StrRef, def: DefinitionId) {
        self.id_to_def.lock().insert(id, def);
    }

    pub fn add_id_type(&self, id: StrRef, ty: Type) {
        self.id_to_type.lock().insert(id, ty);
    }

    pub fn add_module_global(&self, id: StrRef, val: SymbolValue) {
        self.module_globals.lock().insert(id, val);
    }
}

pub struct Resolver(pub Arc<ResolverInternal>);

impl SymbolResolver for Resolver {
    fn get_default_param_value(&self, expr: &ast::Expr) -> Option<SymbolValue> {
        match &expr.node {
            ast::ExprKind::Name { id, .. } => {
                self.0.module_globals.lock().get(id).cloned()
            }
            _ => unimplemented!("other type of expr not supported at {}", expr.location)
        }
    }

    fn get_symbol_type(
        &self,
        _: &mut Unifier,
        _: &[Arc<RwLock<TopLevelDef>>],
        _: &PrimitiveStore,
        str: StrRef,
    ) -> Result<Type, String> {
        self.0.id_to_type.lock().get(&str).cloned().ok_or(format!("cannot get type of {}", str))
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        _: StrRef,
        _: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        self.0.id_to_def.lock().get(&id).cloned()
    }

    fn get_string_id(&self, _: &str) -> i32 {
        unimplemented!()
    }
}
