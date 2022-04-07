use super::EscapeAnalyzer;
use crate::typecheck::{type_inferencer::test::TestEnvironment, typedef::TypeEnum};
use indoc::indoc;
use nac3parser::ast::fold::Fold;
use std::collections::hash_set::HashSet;
use test_case::test_case;

use nac3parser::parser::parse_program;

#[test_case(indoc! {"
    # a: list[list[int32]]
    b = [1]
    a[0] = b
"}, Err("field lifetime error in unknown: line 3 column 2".into())
; "assign global elem")]
#[test_case(indoc! {"
    # a: list[list[int32]]
    b = [[], []]
    b[1] = a
    b[0][0] = [0]
"}, Err("field lifetime error in unknown: line 4 column 5".into())
; "global unify")]
#[test_case(indoc! {"
    b = [1, 2, 3]
    c = [a]
    c[0][0] = b
"}, Err("field lifetime error in unknown: line 3 column 5".into())
; "global unify 2")]
fn test_simple(source: &str, expected_result: Result<(), String>) {
    let mut env = TestEnvironment::basic_test_env();
    let mut defined_identifiers: HashSet<_> = env.identifier_mapping.keys().cloned().collect();
    defined_identifiers.insert("a".into());
    let mut inferencer = env.get_inferencer();
    inferencer.defined_identifiers = defined_identifiers.clone();

    let list_int = inferencer.unifier.add_ty(TypeEnum::TList { ty: inferencer.primitives.int32 });
    let list_list_int = inferencer.unifier.add_ty(TypeEnum::TList { ty: list_int });

    inferencer.variable_mapping.insert("a".into(), list_list_int);
    let statements = parse_program(source, Default::default()).unwrap();
    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    inferencer.check_block(&statements, &mut defined_identifiers).unwrap();

    let mut lifetime_ctx = EscapeAnalyzer::new(
        &mut inferencer.unifier,
        &mut inferencer.primitives,
        inferencer.function_data.resolver.clone(),
        &inferencer.top_level,
    );
    lifetime_ctx.handle_statements(&statements).unwrap();
    lifetime_ctx.builder.remove_empty_bb();
    let result = lifetime_ctx.builder.analyze();
    assert_eq!(result, expected_result);

}
