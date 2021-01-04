use crate::typedef::*;
use std::collections::HashMap;
use std::rc::Rc;

/// Structure for storing top-level type definitions.
/// Used for collecting type signature from source code.
/// Can be converted to `InferenceContext` for type inference in functions.
pub struct TopLevelContext<'a> {
    /// List of primitive definitions.
    pub(super) primitive_defs: Vec<TypeDef<'a>>,
    /// List of class definitions.
    pub(super) class_defs: Vec<ClassDef<'a>>,
    /// List of parametric type definitions.
    pub(super) parametric_defs: Vec<ParametricDef<'a>>,
    /// List of type variable definitions.
    pub(super) var_defs: Vec<VarDef<'a>>,
    /// Function name to signature mapping.
    pub(super) fn_table: HashMap<&'a str, FnDef>,
    /// Type name to type mapping.
    pub(super) sym_table: HashMap<&'a str, Type>,

    primitives: Vec<Type>,
    variables: Vec<Type>,
}

impl<'a> TopLevelContext<'a> {
    pub fn new(primitive_defs: Vec<TypeDef<'a>>) -> TopLevelContext {
        let mut sym_table = HashMap::new();
        let mut primitives = Vec::new();
        for (i, t) in primitive_defs.iter().enumerate() {
            primitives.push(TypeEnum::PrimitiveType(PrimitiveId(i)).into());
            sym_table.insert(t.name, TypeEnum::PrimitiveType(PrimitiveId(i)).into());
        }
        TopLevelContext {
            primitive_defs,
            class_defs: Vec::new(),
            parametric_defs: Vec::new(),
            var_defs: Vec::new(),
            fn_table: HashMap::new(),
            sym_table,
            primitives,
            variables: Vec::new(),
        }
    }

    pub fn add_class(&mut self, def: ClassDef<'a>) -> ClassId {
        self.sym_table.insert(
            def.base.name,
            TypeEnum::ClassType(ClassId(self.class_defs.len())).into(),
        );
        self.class_defs.push(def);
        ClassId(self.class_defs.len() - 1)
    }

    pub fn add_parametric(&mut self, def: ParametricDef<'a>) -> ParamId {
        let params = def
            .params
            .iter()
            .map(|&v| Rc::new(TypeEnum::TypeVariable(v)))
            .collect();
        self.sym_table.insert(
            def.base.name,
            TypeEnum::ParametricType(ParamId(self.parametric_defs.len()), params).into(),
        );
        self.parametric_defs.push(def);
        ParamId(self.parametric_defs.len() - 1)
    }

    pub fn add_variable(&mut self, def: VarDef<'a>) -> VariableId {
        self.sym_table.insert(
            def.name,
            TypeEnum::TypeVariable(VariableId(self.var_defs.len())).into(),
        );
        self.add_variable_private(def)
    }

    pub fn add_variable_private(&mut self, def: VarDef<'a>) -> VariableId {
        self.var_defs.push(def);
        self.variables
            .push(TypeEnum::TypeVariable(VariableId(self.var_defs.len() - 1)).into());
        VariableId(self.var_defs.len() - 1)
    }

    pub fn add_fn(&mut self, name: &'a str, def: FnDef) {
        self.fn_table.insert(name, def);
    }

    pub fn get_fn_def(&self, name: &str) -> Option<&FnDef> {
        self.fn_table.get(name)
    }

    pub fn get_primitive_def_mut(&mut self, id: PrimitiveId) -> &mut TypeDef<'a> {
        self.primitive_defs.get_mut(id.0).unwrap()
    }

    pub fn get_primitive_def(&self, id: PrimitiveId) -> &TypeDef {
        self.primitive_defs.get(id.0).unwrap()
    }

    pub fn get_class_def_mut(&mut self, id: ClassId) -> &mut ClassDef<'a> {
        self.class_defs.get_mut(id.0).unwrap()
    }

    pub fn get_class_def(&self, id: ClassId) -> &ClassDef {
        self.class_defs.get(id.0).unwrap()
    }

    pub fn get_parametric_def_mut(&mut self, id: ParamId) -> &mut ParametricDef<'a> {
        self.parametric_defs.get_mut(id.0).unwrap()
    }

    pub fn get_parametric_def(&self, id: ParamId) -> &ParametricDef {
        self.parametric_defs.get(id.0).unwrap()
    }

    pub fn get_variable_def_mut(&mut self, id: VariableId) -> &mut VarDef<'a> {
        self.var_defs.get_mut(id.0).unwrap()
    }

    pub fn get_variable_def(&self, id: VariableId) -> &VarDef {
        self.var_defs.get(id.0).unwrap()
    }

    pub fn get_primitive(&self, id: PrimitiveId) -> Type {
        self.primitives.get(id.0).unwrap().clone()
    }

    pub fn get_variable(&self, id: VariableId) -> Type {
        self.variables.get(id.0).unwrap().clone()
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        // TODO: handle parametric types
        self.sym_table.get(name).cloned()
    }
}
