use crate::{
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use indoc::indoc;
use parking_lot::{Mutex, RwLock};
use rustpython_parser::{ast::fold::Fold, parser::parse_program};
use std::{borrow::BorrowMut, collections::{HashMap, HashSet}, sync::Arc};
use test_case::test_case;

use super::TopLevelComposer;

struct Resolver {
    id_to_type: HashMap<String, Type>,
    id_to_def: HashMap<String, DefinitionId>,
    class_names: HashMap<String, Type>,
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

    fn add_id_def(&mut self, id: String, def: DefinitionId) {
        self.id_to_def.insert(id, def);
    }
}



#[test_case(
    vec![
        indoc! {"
            def fun(a: int) -> int:
                return a
        "},
        indoc! {"
            class A:
                def __init__(self):
                    self.a: int = 3
        "},
        indoc! {"
            class B:
                def __init__(self):
                    self.b: float = 4.3
                
                def fun(self):
                    self.b = self.b + 3.0
        "},
        indoc! {"
            def foo(a: float):
                a + 1.0
        "},
        indoc! {"
            class C(B):
                def __init__(self):
                    self.c: int = 4
                    self.a: bool = True
        "}
    ]
)]
fn test_simple_register(source: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        composer.register_top_level(ast, None).unwrap();
    }
}

#[test_case(
    vec![
        indoc! {"
            def fun(a: int) -> int:
                return a
        "},
        // indoc! {"
        //     class A:
        //         def __init__(self):
        //             self.a: int = 3
        // "},
        // indoc! {"
        //     class B:
        //         def __init__(self):
        //             self.b: float = 4.3
                
        //         def fun(self):
        //             self.b = self.b + 3.0
        // "},
        // indoc! {"
        //     def foo(a: float):
        //         a + 1.0
        // "},
        // indoc! {"
        //     class C(B):
        //         def __init__(self):
        //             self.c: int = 4
        //             self.a: bool = True
        // "}
    ]
)]
fn test_simple_analyze(source: Vec<&str>) {
    let mut composer = TopLevelComposer::new();
    
    let resolver = Arc::new(Mutex::new(Box::new(Resolver {
        id_to_def: Default::default(),
        id_to_type: Default::default(),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>));

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        let (id, def_id) = composer.register_top_level(ast, Some(resolver.clone())).unwrap();
        resolver.lock().add_id_def(id, def_id);
    }

    composer.start_analysis().unwrap();
}
