use std::borrow::BorrowMut;
use std::ops::{Deref, DerefMut};
use std::{collections::HashMap, collections::HashSet, sync::Arc};

use self::top_level_type_annotation_info::*;
use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, TypeEnum, Unifier};
use crate::typecheck::{typedef::{FunSignature, FuncArg}};
use crate::symbol_resolver::SymbolResolver;
use itertools::{Itertools, izip};
use parking_lot::{Mutex, RwLock};
use rustpython_parser::ast::{self, Stmt};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct DefinitionId(pub usize);

pub mod top_level_type_annotation_info {
    use super::*;

    #[derive(Clone)]
    pub enum TypeAnnotation {
        PrimitiveKind(Type),
        ConcretizedCustomClassKind {
            id: DefinitionId,
            // can not be type var, others are all fine
            params: Vec<TypeAnnotation>
        },
        // can only be ConcretizedCustomClassKind
        VirtualKind(Box<TypeAnnotation>),
        TypeVarKind(Type),
        SelfTypeKind(DefinitionId),
    }

    pub fn parse_ast_to_type_annotation_kinds<T>(
        resolver: &dyn SymbolResolver,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &ast::Expr<T>,
    ) -> Result<TypeAnnotation, String> {
        let results = vec![
            parse_ast_to_concrete_primitive_kind(resolver, top_level_defs, unifier, primitives, expr),
            parse_ast_to_concretized_custom_class_kind(resolver, top_level_defs, unifier, primitives, expr),
            parse_ast_to_type_variable_kind(resolver, top_level_defs, unifier, primitives, expr),
            parse_ast_to_virtual_kind(resolver, top_level_defs, unifier, primitives, expr)
        ];
        let results = results.iter().filter(|x| x.is_ok()).collect_vec();

        if results.len() == 1 {
            results[0].clone()
        } else {
            Err("cannot be parsed the type annotation without ambiguity".into())
        }

    }

    pub fn get_type_from_type_annotation_kinds(
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        ann: &TypeAnnotation
    ) -> Result<Type, String> {
        match ann {
            TypeAnnotation::ConcretizedCustomClassKind { id, params } => {
                let class_def = top_level_defs[id.0].read();
                if let TopLevelDef::Class {fields, methods, type_vars, .. } = &*class_def {
                    if type_vars.len() != params.len() {
                        Err(format!(
                            "unexpected number of type parameters: expected {} but got {}",
                            type_vars.len(),
                            params.len()
                        ))
                    } else {
                        let param_ty = params
                            .iter()
                            .map(|x| get_type_from_type_annotation_kinds(
                                top_level_defs,
                                unifier,
                                primitives,
                                x
                            ))
                            .collect::<Result<Vec<_>, _>>()?;

                        let subst = type_vars
                            .iter()
                            .map(|x| {
                                if let TypeEnum::TVar { id, .. } = unifier.get_ty(*x).as_ref() {
                                    *id
                                } else {
                                    unreachable!()
                                }
                            })
                            .zip(param_ty.into_iter())
                            .collect::<HashMap<u32, Type>>();

                        let mut tobj_fields = methods
                            .iter()
                            .map(|(name, ty, _)| {
                                let subst_ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                                (name.clone(), subst_ty)
                            })
                            .collect::<HashMap<String, Type>>();
                            
                        tobj_fields.extend(
                             fields
                                .iter()
                                .map(|(name, ty)| {
                                    let subst_ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                                    (name.clone(), subst_ty)
                                })
                        );
                        Ok(unifier.add_ty(TypeEnum::TObj {
                            obj_id: *id,
                            fields: tobj_fields.into(),
                            params: subst.into()
                        }))
                    }
                } else {
                    unreachable!("should be class def here")
                }
            }
            TypeAnnotation::SelfTypeKind(obj_id) => {
                let class_def = top_level_defs[obj_id.0].read();
                if let TopLevelDef::Class {fields, methods, type_vars, .. } = &*class_def {
                    let subst = type_vars
                        .iter()
                        .map(|x| {
                            if let TypeEnum::TVar { id, .. } = unifier.get_ty(*x).as_ref() {
                                (*id, *x)
                            } else {
                                unreachable!()
                            }
                        })
                        .collect::<HashMap<u32, Type>>();

                    let mut tobj_fields = methods
                        .iter()
                        .map(|(name, ty, _)| {
                            (name.clone(), *ty)
                        })
                        .collect::<HashMap<String, Type>>();
                        
                    tobj_fields.extend(fields.clone().into_iter());
                    Ok(unifier.add_ty(TypeEnum::TObj {
                        obj_id: *obj_id,
                        fields: tobj_fields.into(),
                        params: subst.into()
                    }))
                } else {
                    unreachable!("should be class def here")
                }
            }
            TypeAnnotation::PrimitiveKind(ty) => Ok(*ty),
            TypeAnnotation::TypeVarKind(ty) => Ok(*ty),
            TypeAnnotation::VirtualKind(ty) => {
                let ty = get_type_from_type_annotation_kinds(
                    top_level_defs,
                    unifier,
                    primitives,
                    ty.as_ref()
                )?;
                Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
            }
        }
    }

    fn parse_ast_to_concrete_primitive_kind<T>(
        _resolver: &dyn SymbolResolver,
        _top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        _unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &ast::Expr<T>,
    ) -> Result<TypeAnnotation, String> {
        match &expr.node {
            ast::ExprKind::Name { id, .. } => match id.as_str() {
                "int32" => Ok(TypeAnnotation::PrimitiveKind(primitives.int32)),
                "int64" => Ok(TypeAnnotation::PrimitiveKind(primitives.int64)),
                "float" => Ok(TypeAnnotation::PrimitiveKind(primitives.float)),
                "bool" =>  Ok(TypeAnnotation::PrimitiveKind(primitives.bool)),
                "None" =>  Ok(TypeAnnotation::PrimitiveKind(primitives.none)),
                _ => Err("not primitive".into())
            }

            _ => Err("not primitive".into())
        }
    }

    pub fn parse_ast_to_concretized_custom_class_kind<T>(
        resolver: &dyn SymbolResolver,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &ast::Expr<T>,
    ) -> Result<TypeAnnotation, String> {
        match &expr.node {
            ast::ExprKind::Name { id, .. } => match id.as_str() {
                "int32" | "int64" | "float" | "bool" | "None" =>
                    Err("expect custom class instead of primitives here".into()),
                x => {
                    let obj_id = resolver
                        .get_identifier_def(x)
                        .ok_or_else(|| "unknown class name".to_string())?;
                    let def = top_level_defs[obj_id.0].read();
                    if let TopLevelDef::Class { .. } = &*def {
                        Ok(TypeAnnotation::ConcretizedCustomClassKind { id: obj_id, params: vec![]})
                    } else {
                        Err("function cannot be used as a type".into())
                    }
                }
            },

            ast::ExprKind::Subscript { value, slice, .. } => {
                if let ast::ExprKind::Name { id, .. } = &value.node {
                    if vec!["virtual", "Generic"].contains(&id.as_str()) { return Err("keywords cannot be class name".into()) }
                    let obj_id = resolver
                        .get_identifier_def(id)
                        .ok_or_else(|| "unknown class name".to_string())?;
                    let def = top_level_defs[obj_id.0].read();
                    if let TopLevelDef::Class { .. } = &*def {
                        let param_type_infos = 
                            if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                                elts.iter()
                                    .map(|v| {
                                        parse_ast_to_type_annotation_kinds(
                                            resolver,
                                            top_level_defs,
                                            unifier,
                                            primitives,
                                            v
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?
                            } else {
                                vec![parse_ast_to_type_annotation_kinds(
                                    resolver, top_level_defs, unifier, primitives, slice,
                                )?]
                            };
                        if param_type_infos.iter().any(|x| matches!(x, TypeAnnotation::TypeVarKind( .. ))) {
                            return Err("cannot apply type variable to class generic parameters".into())
                        }
                        Ok(TypeAnnotation::ConcretizedCustomClassKind { id: obj_id, params: param_type_infos })
                    } else {
                        Err("function cannot be used as a type".into())
                    }
                } else {
                    Err("unsupported expression type".into())
                }
            },

            _ => Err("unsupported expression type".into())
        }
    }

    pub fn parse_ast_to_virtual_kind<T>(
        resolver: &dyn SymbolResolver,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &ast::Expr<T>,
    ) -> Result<TypeAnnotation, String> {
        match &expr.node {
            ast::ExprKind::Subscript { value, slice, .. }
                if matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "virtual") => {
                let def = parse_ast_to_concretized_custom_class_kind(
                    resolver,
                    top_level_defs,
                    unifier,
                    primitives,
                    slice.as_ref()
                )?;
                if !matches!(def, TypeAnnotation::ConcretizedCustomClassKind { .. }) {
                    unreachable!("should must be concretized custom class kind")
                }
                Ok(TypeAnnotation::VirtualKind(def.into()))
            }

            _ => Err("virtual type annotation must be like `virtual[ .. ]`".into())
        }
    }

    pub fn parse_ast_to_type_variable_kind<T>(
        resolver: &dyn SymbolResolver,
        _top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &ast::Expr<T>,
    ) -> Result<TypeAnnotation, String> {
        if let ast::ExprKind::Name { id, .. } = &expr.node {
            let ty = resolver
                .get_symbol_type(unifier, primitives, id)
                .ok_or_else(|| "unknown type variable name".to_string())?;
            Ok(TypeAnnotation::TypeVarKind(ty))
        } else {
            Err("unsupported expression for type variable".into())
        }
    }
}

pub enum TopLevelDef {
    Class {
        // name for error messages and symbols
        name: String,
        // object ID used for TypeEnum
        object_id: DefinitionId,
        // type variables bounded to the class.
        type_vars: Vec<Type>,
        // class fields
        fields: Vec<(String, Type)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(String, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<TypeAnnotation>,
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    },
    Function {
        // prefix for symbol, should be unique globally, and not ending with numbers
        name: String,
        // function signature.
        signature: Type,
        /// Function instance to symbol mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class.
        /// Value: function symbol name.
        instance_to_symbol: HashMap<String, String>,
        /// Function instances to annotated AST mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class. Excluding rigid type
        /// variables.
        /// Value: AST annotated with types together with a unification table index. Could contain
        /// rigid type variables that would be substituted when the function is instantiated.
        instance_to_stmt: HashMap<String, (Stmt<Option<Type>>, usize)>,
        // symbol resolver of the module defined the class
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    },
    Initializer {
        class_id: DefinitionId,
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<Vec<Arc<RwLock<TopLevelDef>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
}

pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_ast_list: Vec<(Arc<RwLock<TopLevelDef>>, Option<ast::Stmt<()>>)>,
    // start as a primitive unifier, will add more top_level defs inside
    pub unifier: Unifier,
    // primitive store
    pub primitives_ty: PrimitiveStore,
    // mangled class method name to def_id
    // pub class_method_to_def_id: HashMap<String, DefinitionId>,
    // record the def id of the classes whoses fields and methods are to be analyzed
    // pub to_be_analyzed_class: Vec<DefinitionId>,
    pub keyword_list: Vec<String>,
}

impl TopLevelContext {
    pub fn read_top_level_def_list(&self) -> &[Arc<RwLock<TopLevelDef>>] {
        self.definitions.as_slice()
    }
}

impl TopLevelComposer {
    pub fn make_top_level_context(self) -> TopLevelContext {
        TopLevelContext {
            definitions: self
                .definition_ast_list
                .into_iter()
                .map(|(x, ..)| x)
                .collect::<Vec<_>>()
                .into(),
            // FIXME: all the big unifier or?
            unifiers: Default::default(),
        }
    }

    pub fn make_primitives() -> (PrimitiveStore, Unifier) {
        let mut unifier = Unifier::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        crate::typecheck::magic_methods::set_primitives_magic_methods(&primitives, &mut unifier);
        (primitives, unifier)
    }

    /// return a composer and things to make a "primitive" symbol resolver, so that the symbol
    /// resolver can later figure out primitive type definitions when passed a primitive type name
    // TODO: add list and tuples?
    pub fn new() -> (Vec<(String, DefinitionId, Type)>, Self) {
        let primitives = Self::make_primitives();

        let top_level_def_list = vec![
            Arc::new(RwLock::new(Self::make_top_level_class_def(0, None, "int32"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(1, None, "int64"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(2, None, "float"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(3, None, "bool"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(4, None, "none"))),
        ];

        let ast_list: Vec<Option<ast::Stmt<()>>> = vec![None, None, None, None, None];

        let composer = TopLevelComposer {
            definition_ast_list: izip!(top_level_def_list, ast_list).collect_vec(),
            primitives_ty: primitives.0,
            unifier: primitives.1,
            // class_method_to_def_id: Default::default(),
            // to_be_analyzed_class: Default::default(),
            keyword_list: vec![
                "Generic".into(),
                "virtual".into(),
                "list".into(),
                "tuple".into(),
                "int32".into(),
                "int64".into(),
                "float".into(),
                "bool".into(),
                "none".into(),
                "None".into(),
            ]
        };
        (
            vec![
                ("int32".into(), DefinitionId(0), composer.primitives_ty.int32),
                ("int64".into(), DefinitionId(1), composer.primitives_ty.int64),
                ("float".into(), DefinitionId(2), composer.primitives_ty.float),
                ("bool".into(), DefinitionId(3), composer.primitives_ty.bool),
                ("none".into(), DefinitionId(4), composer.primitives_ty.none),
            ],
            composer,
        )
    }

    /// already include the definition_id of itself inside the ancestors vector
    /// when first regitering, the type_vars, fields, methods, ancestors are invalid
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
        name: &str,
    ) -> TopLevelDef {
        TopLevelDef::Class {
            name: name.to_string(),
            object_id: DefinitionId(index),
            type_vars: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            ancestors: vec![TypeAnnotation::SelfTypeKind(DefinitionId(index))],
            resolver,
        }
    }

    /// when first registering, the type is a invalid value
    pub fn make_top_level_function_def(
        name: String,
        ty: Type,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            signature: ty,
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
        }
    }

    fn make_class_method_name(mut class_name: String, method_name: &str) -> String {
        class_name.push_str(method_name);
        class_name
    }

    fn extract_def_list(&self) -> Vec<Arc<RwLock<TopLevelDef>>> {
        self
            .definition_ast_list
            .iter()
            .map(|(def, ..)| def.clone())
            .collect_vec()
    }

    /// step 0, register, just remeber the names of top level classes/function
    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    ) -> Result<(String, DefinitionId), String> {
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                if self.keyword_list.contains(name) {
                    return Err("cannot use keyword as a class name".into())
                }
                
                let class_name = name.to_string();
                let class_def_id = self.definition_ast_list.len();

                // since later when registering class method, ast will still be used,
                // here push None temporarly, later will move the ast inside
                let mut class_def_ast = (
                    Arc::new(RwLock::new(Self::make_top_level_class_def(
                        class_def_id,
                        resolver.clone(),
                        name.as_str(),
                    ))),
                    None,
                );

                // parse class def body and register class methods into the def list.
                // module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id
                let mut class_method_name_def_ids: Vec<(
                    // the simple method name without class name
                    String,
                    // in this top level def, method name is prefixed with the class name
                    Arc<RwLock<TopLevelDef>>,
                    DefinitionId,
                    Type
                )> = Vec::new();
                let mut class_method_index_offset = 0;
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        let method_def_id = self.definition_ast_list.len() + {
                            class_method_index_offset += 1;
                            class_method_index_offset
                        };

                        // dummy method define here
                        let dummy_method_type = self.unifier.get_fresh_var();
                        class_method_name_def_ids.push((
                            method_name.clone(),
                            RwLock::new(Self::make_top_level_function_def(
                                Self::make_class_method_name(class_name.clone(), method_name),
                                // later unify with parsed type
                                dummy_method_type.0,
                                resolver.clone(),
                            ))
                            .into(),
                            DefinitionId(method_def_id),
                            dummy_method_type.0
                        ));

                    } else {
                        // do nothing
                        continue
                    }
                }
                
                // move the ast to the entry of the class in the ast_list
                class_def_ast.1 = Some(ast);
                // get the methods into the class_def
                for (name, _, id, ty) in &class_method_name_def_ids {
                    let mut class_def = class_def_ast.0.write();
                    if let TopLevelDef::Class { methods, .. } = class_def.deref_mut() {
                        methods.push((name.clone(), *ty, *id))
                    } else { unreachable!() }
                }
                // now class_def_ast and class_method_def_ast_ids are ok, put them into actual def list in correct order
                self.definition_ast_list.push(class_def_ast);
                for (_, def, ..) in class_method_name_def_ids {
                    self.definition_ast_list.push((def, None));
                }

                // put the constructor into the def_list
                self.definition_ast_list.push((
                    RwLock::new(TopLevelDef::Initializer { class_id: DefinitionId(class_def_id) })
                        .into(),
                    None,
                ));

                Ok((class_name, DefinitionId(class_def_id)))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();

                // add to the definition list
                self.definition_ast_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        name.into(),
                        // unify with correct type later
                        self.unifier.get_fresh_var().0,
                        resolver,
                    ))
                    .into(),
                    Some(ast),
                ));

                // return
                Ok((fun_name, DefinitionId(self.definition_ast_list.len() - 1)))
            }

            _ => Err("only registrations of top level classes/functions are supprted".into()),
        }
    }

    /// step 1, analyze the type vars associated with top level class
    fn analyze_top_level_class_type_var(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        for (class_def, class_ast) in def_list {
            // only deal with class def here
            let mut class_def = class_def.write();
            let (class_bases_ast, class_def_type_vars, class_resolver) = {
                if let TopLevelDef::Class { type_vars, resolver, .. } = class_def.deref_mut() {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (bases, type_vars, resolver)
                    } else {
                        unreachable!("must be both class")
                    }
                } else {
                    continue;
                }
            };
            let class_resolver = class_resolver.as_ref().unwrap().lock();
            let class_resolver = class_resolver.deref();

            let mut is_generic = false;
            for b in class_bases_ast {
                match &b.node {
                    // analyze typevars bounded to the class,
                    // only support things like `class A(Generic[T, V])`,
                    // things like `class A(Generic[T, V, ImportedModule.T])` is not supported
                    // i.e. only simple names are allowed in the subscript
                    // should update the TopLevelDef::Class.typevars and the TypeEnum::TObj.params
                    ast::ExprKind::Subscript { value, slice, .. }
                        if {
                            matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "Generic") 
                        } => {
                        if !is_generic {
                            is_generic = true;
                        } else {
                            return Err("Only single Generic[...] can be in bases".into());
                        }

                        let mut type_var_list: Vec<&ast::Expr<()>> = vec![];
                        // if `class A(Generic[T, V, G])`
                        if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                            type_var_list.extend(elts.iter());
                        // `class A(Generic[T])`
                        } else {
                            type_var_list.push(slice.deref());
                        }

                        // parse the type vars
                        let type_vars = type_var_list
                            .into_iter()
                            .map(|e| {
                                class_resolver.parse_type_annotation(
                                    &temp_def_list,
                                    unifier,
                                    primitives_store,
                                    e
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        // check if all are unique type vars
                        let mut occured_type_var_id: HashSet<u32> = HashSet::new();
                        let all_unique_type_var = type_vars.iter().all(|x| {
                            let ty = unifier.get_ty(*x);
                            if let TypeEnum::TVar { id, .. } = ty.as_ref() {
                                occured_type_var_id.insert(*id)
                            } else {
                                false
                            }
                        });

                        // NOTE: create a copy of all type vars for the type vars associated with class
                        let type_vars = type_vars
                            .into_iter()
                            .map(|x| {
                                let range = unifier.get_ty(x);
                                if let TypeEnum::TVar { range, .. } = range.as_ref() {
                                    let range = &*range.borrow();
                                    let range = range.as_slice();
                                    unifier.get_fresh_var_with_range(range).0
                                } else {
                                    unreachable!("must be type var here");
                                }
                            })
                            .collect_vec();
                    
                        if !all_unique_type_var {
                            return Err("expect unique type variables".into());
                        }
                    
                        // add to TopLevelDef
                        class_def_type_vars.extend(type_vars);
                    }

                    // if others, do nothing in this function
                    _ => continue,
                }
            }
        }
        Ok(())
    }

    /// step 2, base classes.
    fn analyze_top_level_class_bases(&mut self) -> Result<(), String> {
        let temp_def_list = self.extract_def_list();
        for (class_def, class_ast) in self.definition_ast_list.iter_mut() {
            let mut class_def = class_def.write();
            let (class_bases, class_ancestors, class_resolver) = {
                if let TopLevelDef::Class { ancestors, resolver, .. } = class_def.deref_mut() {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (bases, ancestors, resolver)
                    } else {
                        unreachable!("must be both class")
                    }
                } else {
                    continue;
                }
            };
            let class_resolver = class_resolver.as_ref().unwrap().lock();
            let class_resolver = class_resolver.deref();

            let mut has_base = false;
            for b in class_bases {
                // type vars have already been handled, so skip on `Generic[...]`
                if matches!(
                    &b.node,
                    ast::ExprKind::Subscript { value, .. }
                        if matches!(
                            &value.node,
                            ast::ExprKind::Name { id, .. } if id == "Generic"
                        )
                ) { continue }
                
                if has_base {
                    return Err("a class def can only have at most one base class \
                    declaration and one generic declaration".into())
                }
                has_base = true;

                let base_ty = parse_ast_to_type_annotation_kinds(
                    class_resolver,
                    &temp_def_list,
                    self.unifier.borrow_mut(),
                    &self.primitives_ty,
                    b
                )?;

                if let TypeAnnotation::ConcretizedCustomClassKind { .. } = base_ty {
                    // TODO: check to prevent cyclic base class
                    class_ancestors.push(base_ty);
                } else {
                    return Err("class base declaration can only be concretized custom class".into())
                }
            }
        }
        Ok(())
    }

    /// step 3, class fields and methods
    fn analyze_top_level_class_fields_methods(&mut self) -> Result<(), String> {
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives = &self.primitives_ty;
        let def_ast_list = &self.definition_ast_list;
        
        let mut type_var_to_concrete_def: HashMap<Type, TypeAnnotation> = HashMap::new();
        for (class_def, class_ast) in def_ast_list {
            Self::analyze_single_class(
                class_def.clone(),
                &class_ast.as_ref().unwrap().node,
                &temp_def_list,
                unifier,
                primitives,
                &mut type_var_to_concrete_def
            )?
        }

        // base class methods add and check
        // TODO: 

        // unification of previously assigned typevar
        for (ty, def) in type_var_to_concrete_def {
            let target_ty = get_type_from_type_annotation_kinds(&temp_def_list, unifier, primitives, &def)?;
            unifier.unify(ty, target_ty)?;
        }
        
        Ok(())
    }

    /// step 4, after class methods are done
    fn analyze_top_level_function(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        for (function_def, function_ast) in def_list {
            let function_def = function_def.read();
            let function_def = function_def.deref();
            let function_ast = if let Some(function_ast) = function_ast {
                function_ast
            } else {
                continue;
            };
            if let TopLevelDef::Function { signature: dummy_ty, resolver, .. } = function_def {
                if let ast::StmtKind::FunctionDef { args, returns, .. } = &function_ast.node {
                    let resolver = resolver.as_ref();
                    let resolver = resolver.unwrap();
                    let resolver = resolver.deref().lock();
                    let function_resolver = resolver.deref();
                    
                    let arg_types = {
                        args
                            .args
                            .iter()
                            .map(|x| -> Result<FuncArg, String> {
                                let annotation = x
                                    .node
                                    .annotation
                                    .as_ref()
                                    .ok_or_else(|| "function parameter type annotation needed".to_string())?
                                    .as_ref();
                                Ok(FuncArg {
                                    name: x.node.arg.clone(),
                                    ty: function_resolver.parse_type_annotation(
                                        temp_def_list.as_slice(),
                                        unifier,
                                        primitives_store,
                                        annotation
                                    )?,
                                    // TODO: function type var
                                    default_value: Default::default()
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    };

                    let return_ty = {
                        let return_annotation = returns
                            .as_ref()
                            .ok_or_else(|| "function return type needed".to_string())?
                            .as_ref();
                        function_resolver.parse_type_annotation(
                            temp_def_list.as_slice(),
                            unifier,
                            primitives_store,
                            return_annotation
                        )?
                    };

                    let function_ty = unifier.add_ty(TypeEnum::TFunc(FunSignature {
                        args: arg_types,
                        ret: return_ty,
                        // TODO: handle var map
                        vars: Default::default()
                    }.into()));
                    unifier.unify(*dummy_ty, function_ty)?;
                } else {
                    unreachable!("must be both function");
                }
            } else {
                continue;
            }
        };
        Ok(())
    }

    /// step 5, field instantiation?
    fn analyze_top_level_field_instantiation(&mut self) -> Result<(), String> {
        // TODO:
        unimplemented!()
    }

    fn analyze_single_class(
        class_def: Arc<RwLock<TopLevelDef>>,
        class_ast: &ast::StmtKind<()>,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        type_var_to_concrete_def: &mut HashMap<Type, TypeAnnotation>
    ) -> Result<(), String> {
        let mut class_def = class_def.write();
        let (
            _class_id,
            _class_name,
            _class_bases_ast,
            class_body_ast,
            _class_ancestor_def,
            class_fields_def,
            class_methods_def,
            _class_type_vars_def,
            class_resolver,
        ) = if let TopLevelDef::Class {
            object_id,
            ancestors,
            fields,
            methods,
            resolver,
            type_vars
        } = class_def.deref_mut() {
            if let ast::StmtKind::ClassDef { name, bases, body, .. } = &class_ast {
                (object_id, name.clone(), bases, body, ancestors, fields, methods, type_vars, resolver)
            } else {
                unreachable!("here must be class def ast");
            }
        } else {
            unreachable!("here must be class def ast");
        };
        let class_resolver = class_resolver.as_ref().unwrap();
        let mut class_resolver = class_resolver.lock();
        let class_resolver = class_resolver.deref_mut();

        for b in class_body_ast {
            if let ast::StmtKind::FunctionDef { args, returns, name, body, .. } = &b.node {
                let (method_dummy_ty, ..) = Self::get_class_method_def_info(class_methods_def, name)?;
                // TODO: handle self arg
                // TODO: handle parameter with same name
                let arg_type: Vec<FuncArg> = {
                    let mut result = Vec::new();
                    for x in &args.args{
                        let name = x.node.arg.clone();
                        let type_ann = {
                            let annotation_expr = x
                                .node
                                .annotation
                                .as_ref()
                                .ok_or_else(|| "type annotation needed".to_string())?
                                .as_ref();
                            parse_ast_to_type_annotation_kinds(
                                class_resolver,
                                temp_def_list,
                                unifier,
                                primitives,
                                annotation_expr
                            )?
                        };
                        if let TypeAnnotation::TypeVarKind(_ty) = &type_ann {
                            // TODO: need to handle to different type vars that are
                            // asscosiated with the class and that are not
                        }
                        let dummy_func_arg = FuncArg {
                            name,
                            ty: unifier.get_fresh_var().0,
                            // TODO: symbol default value?
                            default_value: None
                        };
                        // push the dummy type and the type annotation
                        // into the list for later unification
                        type_var_to_concrete_def.insert(dummy_func_arg.ty, type_ann.clone());
                        result.push(dummy_func_arg)
                    }
                    result
                };
                let ret_type = {
                    let result = returns
                        .as_ref()
                        .ok_or_else(|| "method return type annotation needed".to_string())?
                        .as_ref();
                    let annotation = parse_ast_to_type_annotation_kinds(
                        class_resolver,
                        temp_def_list,
                        unifier,
                        primitives,
                        result
                    )?;
                    let dummy_return_type = unifier.get_fresh_var().0;
                    type_var_to_concrete_def.insert(dummy_return_type, annotation.clone());
                    dummy_return_type
                };
                // TODO: handle var map, to create a new copy of type var
                // while tracking the type var associated with class
                let method_var_map: HashMap<u32, Type> = HashMap::new();
                let method_type = unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    args: arg_type,
                    ret: ret_type,
                    vars: method_var_map
                }.into()));
                // unify now since function type is not in type annotation define
                // which is fine since type within method_type will be subst later
                unifier.unify(method_dummy_ty, method_type)?;

                // class fields
                if name == "__init__" {
                    for b in body {
                        let mut defined_fields: HashSet<String> = HashSet::new();
                        // TODO: check the type of value, field instantiation check
                        if let ast::StmtKind::AnnAssign { annotation, target, value: _, .. } = &b.node {
                            if let ast::ExprKind::Attribute { value, attr, .. } = &target.node {
                                if matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "self") {
                                    if defined_fields.insert(attr.to_string()) {
                                        let dummy_field_type = unifier.get_fresh_var().0;
                                        class_fields_def.push((attr.to_string(), dummy_field_type));
                                        let annotation = parse_ast_to_type_annotation_kinds(
                                            class_resolver,
                                            &temp_def_list,
                                            unifier,
                                            primitives,
                                            annotation.as_ref()
                                        )?;
                                        type_var_to_concrete_def.insert(dummy_field_type, annotation);
                                    } else {
                                        return Err("same class fields defined twice".into());
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                continue;
            }
        };
        Ok(())
    }

    fn get_class_method_def_info(
        class_methods_def: &[(String, Type, DefinitionId)],
        method_name: &str
    ) -> Result<(Type, DefinitionId), String> {
        for (name, ty, def_id) in class_methods_def {
            if name == method_name {
                return Ok((*ty, *def_id));
            }
        }
        Err(format!("no method {} in the current class", method_name))
    }
}