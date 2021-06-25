use super::TopLevelContext;
use crate::typedef::*;
use std::boxed::Box;
use std::collections::HashMap;

struct ContextStack<'a> {
    /// stack level, starts from 0
    level: u32,
    /// stack of variable definitions containing (id, def, level) where `def` is the original
    /// definition in `level-1`.
    var_defs: Vec<(usize, VarDef<'a>, u32)>,
    /// stack of symbol definitions containing (name, level) where `level` is the smallest level
    /// where the name is assigned a value
    sym_def: Vec<(&'a str, u32)>,
}

pub struct InferenceContext<'a> {
    /// top level context
    top_level: TopLevelContext<'a>,

    /// list of primitive instances
    primitives: Vec<Type>,
    /// list of variable instances
    variables: Vec<Type>,
    /// identifier to (type, readable) mapping.
    /// an identifier might be defined earlier but has no value (for some code path), thus not
    /// readable.
    sym_table: HashMap<&'a str, (Type, bool)>,
    /// resolution function reference, that may resolve unbounded identifiers to some type
    resolution_fn: Box<dyn FnMut(&str) -> Result<Type, String>>,
    /// stack
    stack: ContextStack<'a>,
}

// non-trivial implementations here
impl<'a> InferenceContext<'a> {
    /// return a new `InferenceContext` from `TopLevelContext` and resolution function.
    pub fn new(
        top_level: TopLevelContext,
        resolution_fn: Box<dyn FnMut(&str) -> Result<Type, String>>,
    ) -> InferenceContext {
        let primitives = (0..top_level.primitive_defs.len())
            .map(|v| TypeEnum::PrimitiveType(PrimitiveId(v)).into())
            .collect();
        let variables = (0..top_level.var_defs.len())
            .map(|v| TypeEnum::TypeVariable(VariableId(v)).into())
            .collect();
        InferenceContext {
            top_level,
            primitives,
            variables,
            sym_table: HashMap::new(),
            resolution_fn,
            stack: ContextStack {
                level: 0,
                var_defs: Vec::new(),
                sym_def: Vec::new(),
            },
        }
    }

    /// execute the function with new scope.
    /// variable assignment would be limited within the scope (not readable outside), and type
    /// variable type guard would be limited within the scope.
    /// returns the list of variables assigned within the scope, and the result of the function
    pub fn with_scope<F, R>(&mut self, f: F) -> (Vec<&'a str>, R)
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.stack.level += 1;
        let result = f(self);
        self.stack.level -= 1;
        while !self.stack.var_defs.is_empty() {
            let (_, _, level) = self.stack.var_defs.last().unwrap();
            if *level > self.stack.level {
                let (id, def, _) = self.stack.var_defs.pop().unwrap();
                self.top_level.var_defs[id] = def;
            } else {
                break;
            }
        }
        let mut poped_names = Vec::new();
        while !self.stack.sym_def.is_empty() {
            let (_, level) = self.stack.sym_def.last().unwrap();
            if *level > self.stack.level {
                let (name, _) = self.stack.sym_def.pop().unwrap();
                self.sym_table.remove(name).unwrap();
                poped_names.push(name);
            } else {
                break;
            }
        }
        (poped_names, result)
    }

    /// assign a type to an identifier.
    /// may return error if the identifier was defined but with different type
    pub fn assign(&mut self, name: &'a str, ty: Type) -> Result<Type, String> {
        if let Some((t, x)) = self.sym_table.get_mut(name) {
            if t == &ty {
                if !*x {
                    self.stack.sym_def.push((name, self.stack.level));
                }
                *x = true;
                Ok(ty)
            } else {
                Err("different types".into())
            }
        } else {
            self.stack.sym_def.push((name, self.stack.level));
            self.sym_table.insert(name, (ty.clone(), true));
            Ok(ty)
        }
    }

    /// check if an identifier is already defined
    pub fn defined(&self, name: &str) -> bool {
        self.sym_table.get(name).is_some()
    }

    /// get the type of an identifier
    /// may return error if the identifier is not defined, and cannot be resolved with the
    /// resolution function.
    pub fn resolve(&mut self, name: &str) -> Result<Type, String> {
        if let Some((t, x)) = self.sym_table.get(name) {
            if *x {
                Ok(t.clone())
            } else {
                Err("may not have value".into())
            }
        } else {
            self.resolution_fn.as_mut()(name)
        }
    }

    /// restrict the bound of a type variable by replacing its definition.
    /// used for implementing type guard
    pub fn restrict(&mut self, id: VariableId, mut def: VarDef<'a>) {
        std::mem::swap(self.top_level.var_defs.get_mut(id.0).unwrap(), &mut def);
        self.stack.var_defs.push((id.0, def, self.stack.level));
    }
}

// trivial getters:
impl<'a> InferenceContext<'a> {
    pub fn get_primitive(&self, id: PrimitiveId) -> Type {
        self.primitives.get(id.0).unwrap().clone()
    }
    pub fn get_variable(&self, id: VariableId) -> Type {
        self.variables.get(id.0).unwrap().clone()
    }

    pub fn get_fn_def(&self, name: &str) -> Option<&FnDef> {
        self.top_level.fn_table.get(name)
    }
    pub fn get_primitive_def(&self, id: PrimitiveId) -> &TypeDef {
        self.top_level.primitive_defs.get(id.0).unwrap()
    }
    pub fn get_class_def(&self, id: ClassId) -> &ClassDef {
        self.top_level.class_defs.get(id.0).unwrap()
    }
    pub fn get_parametric_def(&self, id: ParamId) -> &ParametricDef {
        self.top_level.parametric_defs.get(id.0).unwrap()
    }
    pub fn get_variable_def(&self, id: VariableId) -> &VarDef {
        self.top_level.var_defs.get(id.0).unwrap()
    }
    pub fn get_type(&self, name: &str) -> Option<Type> {
        self.top_level.get_type(name)
    }
}

impl TypeEnum {
    pub fn subst(&self, map: &HashMap<VariableId, Type>) -> TypeEnum {
        match self {
            TypeEnum::TypeVariable(id) => map.get(id).map(|v| v.as_ref()).unwrap_or(self).clone(),
            TypeEnum::ParametricType(id, params) => TypeEnum::ParametricType(
                *id,
                params
                    .iter()
                    .map(|v| v.as_ref().subst(map).into())
                    .collect(),
            ),
            _ => self.clone(),
        }
    }

    pub fn get_subst(&self, ctx: &InferenceContext) -> HashMap<VariableId, Type> {
        match self {
            TypeEnum::ParametricType(id, params) => {
                let vars = &ctx.get_parametric_def(*id).params;
                vars.iter()
                    .zip(params)
                    .map(|(v, p)| (*v, p.as_ref().clone().into()))
                    .collect()
            }
            // if this proves to be slow, we can use option type
            _ => HashMap::new(),
        }
    }

    pub fn get_base<'b: 'a, 'a>(&'a self, ctx: &'b InferenceContext) -> Option<&'b TypeDef> {
        match self {
            TypeEnum::PrimitiveType(id) => Some(ctx.get_primitive_def(*id)),
            TypeEnum::ClassType(id) | TypeEnum::VirtualClassType(id) => {
                Some(&ctx.get_class_def(*id).base)
            }
            TypeEnum::ParametricType(id, _) => Some(&ctx.get_parametric_def(*id).base),
            _ => None,
        }
    }
}
