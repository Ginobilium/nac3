use std::collections::HashMap;
use std::collections::HashSet;

use super::primitives::get_var;
use super::symbol_resolver::*;
use super::typedef::*;
use rustpython_parser::ast::Location;

/// Structure for storing top-level type definitions.
/// Used for collecting type signature from source code.
/// Can be converted to `InferenceContext` for type inference in functions.
#[derive(Clone)]
pub struct GlobalContext<'a> {
    /// List of type definitions.
    pub type_defs: Vec<TypeDef<'a>>,
    /// List of type variable definitions.
    pub var_defs: Vec<VarDef<'a>>,
}

impl<'a> GlobalContext<'a> {
    pub fn new(type_defs: Vec<TypeDef<'a>>) -> GlobalContext {
        GlobalContext {
            type_defs,
            var_defs: Vec::new(),
        }
    }

    pub fn add_type(&mut self, def: TypeDef<'a>) -> TypeId {
        self.type_defs.push(def);
        TypeId(self.type_defs.len() - 1)
    }

    pub fn add_variable(&mut self, def: VarDef<'a>) -> VariableId {
        self.var_defs.push(def);
        VariableId(self.var_defs.len() - 1)
    }

    pub fn get_type_def_mut(&mut self, id: TypeId) -> &mut TypeDef<'a> {
        self.type_defs.get_mut(id.0).unwrap()
    }

    pub fn get_type_def(&self, id: TypeId) -> &TypeDef {
        self.type_defs.get(id.0).unwrap()
    }

    pub fn get_var_def(&self, id: VariableId) -> &VarDef {
        self.var_defs.get(id.0).unwrap()
    }

    pub fn get_var_count(&self) -> usize {
        self.var_defs.len()
    }
}

pub struct InferenceContext<'a> {
    // a: (i, x) means that a.i = x
    pub fields_assignment: HashMap<VariableId, Vec<(&'a str, VariableId, Location)>>,
    pub constraints: Vec<(Type, Type)>,
    global: GlobalContext<'a>,
    resolver: Box<dyn SymbolResolver>,
    local_identifiers: HashMap<&'a str, Type>,
    local_variables: Vec<VarDef<'a>>,
    fresh_var_id: usize,
}

impl<'a> InferenceContext<'a> {
    pub fn new(
        global: GlobalContext<'a>,
        resolver: Box<dyn SymbolResolver>,
    ) -> InferenceContext<'a> {
        let id = global.get_var_count();
        InferenceContext {
            global,
            fields_assignment: HashMap::new(),
            constraints: Vec::new(),
            resolver,
            local_identifiers: HashMap::new(),
            local_variables: Vec::new(),
            fresh_var_id: id,
        }
    }

    fn get_fresh_var(&mut self) -> VariableId {
        self.local_variables.push(VarDef {
            name: None,
            bound: Vec::new(),
        });
        let id = self.fresh_var_id;
        self.fresh_var_id += 1;
        VariableId(id)
    }

    fn get_fresh_var_with_bound(&mut self, bound: Vec<Type>) -> VariableId {
        self.local_variables.push(VarDef { name: None, bound });
        let id = self.fresh_var_id;
        self.fresh_var_id += 1;
        VariableId(id)
    }

    pub fn assign_identifier(&mut self, identifier: &'a str) -> Type {
        if let Some(t) = self.local_identifiers.get(identifier) {
            t.clone()
        } else if let Some(SymbolType::Identifier(t)) = self.resolver.get_symbol_type(identifier) {
            t
        } else {
            get_var(self.get_fresh_var())
        }
    }

    pub fn get_identifier_type(&self, identifier: &'a str) -> Result<Type, String> {
        if let Some(t) = self.local_identifiers.get(identifier) {
            Ok(t.clone())
        } else if let Some(SymbolType::Identifier(t)) = self.resolver.get_symbol_type(identifier) {
            Ok(t)
        } else {
            Err("unbounded identifier".into())
        }
    }

    pub fn get_attribute_type(
        &mut self,
        expr: Type,
        identifier: &'a str,
        location: Location,
    ) -> Result<Type, String> {
        match expr.as_ref() {
            TypeEnum::TypeVariable(id) => {
                if !self.fields_assignment.contains_key(id) {
                    self.fields_assignment.insert(*id, Vec::new());
                }
                let var_id = VariableId(self.fresh_var_id);
                let entry = self.fields_assignment.get_mut(&id).unwrap();
                for (attr, t, _) in entry.iter() {
                    if *attr == identifier {
                        return Ok(get_var(*t));
                    }
                }
                entry.push((identifier, var_id, location));
                self.local_variables.push(VarDef {
                    name: None,
                    bound: Vec::new(),
                });
                self.fresh_var_id += 1;
                Ok(get_var(var_id))
            }
            TypeEnum::ClassType(id, params) => {
                let type_def = self.global.get_type_def(*id);
                let field = type_def
                    .base
                    .fields
                    .get(identifier)
                    .map_or_else(|| Err("no such field".to_owned()), Ok)?;
                // function and tuple can have 0 type variables but with type parameters
                // we require other types have the same number of type variables and type
                // parameters in order to build a mapping
                assert!(type_def.params.is_empty() || type_def.params.len() == params.len());
                let map = type_def
                    .params
                    .clone()
                    .into_iter()
                    .zip(params.clone().into_iter())
                    .collect();
                let field = field.subst(&map);
                Ok(self.get_instance(field))
            }
        }
    }

    fn get_instance(&mut self, t: Type) -> Type {
        let mut vars = HashSet::new();
        t.get_vars(&mut vars);

        let local_min = self.global.get_var_count();
        let bounded = vars.into_iter().filter(|id| id.0 < local_min);
        let map = bounded
            .map(|v| {
                (
                    v,
                    get_var(
                        self.get_fresh_var_with_bound(self.global.get_var_def(v).bound.clone()),
                    ),
                )
            })
            .collect();
        t.subst(&map)
    }

    pub fn get_type_def(&self, id: TypeId) -> &TypeDef {
        self.global.get_type_def(id)
    }
}
