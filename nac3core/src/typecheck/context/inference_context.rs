use super::super::location::{FileID, Location};
use super::super::symbol_resolver::*;
use super::super::typedef::*;
use super::GlobalContext;
use rustpython_parser::ast;
use std::boxed::Box;
use std::collections::HashMap;

pub struct ContextStack {
    /// stack level, starts from 0
    level: u32,
    /// stack of symbol definitions containing (name, level) where `level` is the smallest level
    /// where the name is assigned a value
    sym_def: Vec<(String, u32)>,
}

pub struct InferenceContext<'a> {
    /// global context
    global: GlobalContext<'a>,
    /// per source symbol resolver
    resolver: Box<dyn SymbolResolver>,
    /// File ID
    file: FileID,

    /// identifier to (type, readable, location) mapping.
    /// an identifier might be defined earlier but has no value (for some code path), thus not
    /// readable.
    sym_table: HashMap<String, (Type, bool, Location)>,
    /// stack
    stack: ContextStack,
}

// non-trivial implementations here
impl<'a> InferenceContext<'a> {
    pub fn new(
        global: GlobalContext,
        resolver: Box<dyn SymbolResolver>,
        file: FileID,
    ) -> InferenceContext {
        InferenceContext {
            global,
            resolver,
            file,
            sym_table: HashMap::new(),
            stack: ContextStack {
                level: 0,
                sym_def: Vec::new(),
            },
        }
    }

    /// execute the function with new scope.
    /// variable assignment would be limited within the scope (not readable outside), and type
    /// returns the list of variables assigned within the scope, and the result of the function
    pub fn with_scope<F, R>(&mut self, f: F) -> (Vec<(String, Type, Location)>, R)
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.start_scope();
        let result = f(self);
        let poped_names = self.end_scope();
        (poped_names, result)
    }

    pub fn start_scope(&mut self) {
        self.stack.level += 1;
    }

    pub fn end_scope(&mut self) -> Vec<(String, Type, Location)> {
        self.stack.level -= 1;
        let mut poped_names = Vec::new();
        while !self.stack.sym_def.is_empty() {
            let (_, level) = self.stack.sym_def.last().unwrap();
            if *level > self.stack.level {
                let (name, _) = self.stack.sym_def.pop().unwrap();
                let (t, b, l) = self.sym_table.get_mut(&name).unwrap();
                // set it to be unreadable
                *b = false;
                poped_names.push((name, t.clone(), *l));
            } else {
                break;
            }
        }
        poped_names
    }

    /// assign a type to an identifier.
    /// may return error if the identifier was defined but with different type
    pub fn assign(&mut self, name: String, ty: Type, loc: ast::Location) -> Result<Type, String> {
        if let Some((t, x, _)) = self.sym_table.get_mut(&name) {
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
            self.stack.sym_def.push((name.clone(), self.stack.level));
            self.sym_table.insert(
                name,
                (ty.clone(), true, Location::CodeRange(self.file, loc)),
            );
            Ok(ty)
        }
    }

    /// get the type of an identifier
    /// may return error if the identifier is not defined, and cannot be resolved with the
    /// resolution function.
    pub fn resolve(&self, name: &str) -> Result<Type, String> {
        if let Some((t, x, _)) = self.sym_table.get(name) {
            if *x {
                Ok(t.clone())
            } else {
                Err("may not be defined".into())
            }
        } else {
            match self.resolver.get_symbol_type(name) {
                Some(SymbolType::Identifier(t)) => Ok(t),
                Some(SymbolType::TypeName(_)) => Err("is not a value".into()),
                _ => Err("unbounded identifier".into()),
            }
        }
    }

    pub fn get_location(&self, name: &str) -> Option<Location> {
        if let Some((_, _, l)) = self.sym_table.get(name) {
            Some(*l)
        } else {
            self.resolver.get_symbol_location(name)
        }
    }

    /// check if an identifier is already defined
    pub fn defined(&self, name: &String) -> bool {
        self.sym_table.get(name).is_some()
    }
}

// trivial getters:
impl<'a> InferenceContext<'a> {
    pub fn get_primitive(&self, id: PrimitiveId) -> Type {
        TypeEnum::PrimitiveType(id).into()
    }

    pub fn get_variable(&self, id: VariableId) -> Type {
        TypeEnum::TypeVariable(id).into()
    }

    pub fn get_fn_def(&self, name: &str) -> Option<&FnDef> {
        self.global.fn_table.get(name)
    }
    pub fn get_primitive_def(&self, id: PrimitiveId) -> &TypeDef {
        self.global.primitive_defs.get(id.0).unwrap()
    }
    pub fn get_class_def(&self, id: ClassId) -> &ClassDef {
        self.global.class_defs.get(id.0).unwrap()
    }
    pub fn get_parametric_def(&self, id: ParamId) -> &ParametricDef {
        self.global.parametric_defs.get(id.0).unwrap()
    }
    pub fn get_variable_def(&self, id: VariableId) -> &VarDef {
        self.global.var_defs.get(id.0).unwrap()
    }
    pub fn get_type(&self, name: &str) -> Result<Type, String> {
        match self.resolver.get_symbol_type(name) {
            Some(SymbolType::TypeName(t)) => Ok(t),
            Some(SymbolType::Identifier(_)) => Err("not a type".into()),
            _ => Err("unbounded identifier".into()),
        }
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
