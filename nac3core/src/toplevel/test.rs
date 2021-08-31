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
use rustpython_parser::{ast::fold::Fold, parser::parse_program};
use std::collections::{HashMap, HashSet};
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
