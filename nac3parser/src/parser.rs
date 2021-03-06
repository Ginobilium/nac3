//! Python parsing.
//!
//! Use this module to parse python code into an AST.
//! There are three ways to parse python code. You could
//! parse a whole program, a single statement, or a single
//! expression.

use std::iter;

use crate::ast::{self, FileName};
use crate::error::ParseError;
use crate::lexer;
pub use crate::mode::Mode;
use crate::python;

/*
 * Parse python code.
 * Grammar may be inspired by antlr grammar for python:
 * https://github.com/antlr/grammars-v4/tree/master/python3
 */

/// Parse a full python program, containing usually multiple lines.
pub fn parse_program(source: &str, file: FileName) -> Result<ast::Suite, ParseError> {
    parse(source, Mode::Module, file).map(|top| match top {
        ast::Mod::Module { body, .. } => body,
        _ => unreachable!(),
    })
}

/// Parses a python expression
///
/// # Example
/// ```
/// use nac3parser::{parser, ast};
/// let expr = parser::parse_expression("1 + 2").unwrap();
///
/// assert_eq!(
///     expr,
///     ast::Expr {
///         location: ast::Location::new(1, 3, Default::default()),
///         custom: (),
///         node: ast::ExprKind::BinOp {
///             left: Box::new(ast::Expr {
///                 location: ast::Location::new(1, 1, Default::default()),
///                 custom: (),
///                 node: ast::ExprKind::Constant {
///                     value: ast::Constant::Int(1.into()),
///                     kind: None,
///                 }
///             }),
///             op: ast::Operator::Add,
///             right: Box::new(ast::Expr {
///                 location: ast::Location::new(1, 5, Default::default()),
///                 custom: (),
///                 node: ast::ExprKind::Constant {
///                     value: ast::Constant::Int(2.into()),
///                     kind: None,
///                 }
///             })
///         }
///     },
/// );
///
/// ```
pub fn parse_expression(source: &str) -> Result<ast::Expr, ParseError> {
    parse(source, Mode::Expression, Default::default()).map(|top| match top {
        ast::Mod::Expression { body } => *body,
        _ => unreachable!(),
    })
}

// Parse a given source code
pub fn parse(source: &str, mode: Mode, file: FileName) -> Result<ast::Mod, ParseError> {
    let lxr = lexer::make_tokenizer(source, file);
    let marker_token = (Default::default(), mode.to_marker(), Default::default());
    let tokenizer = iter::once(Ok(marker_token)).chain(lxr);

    python::TopParser::new()
        .parse(tokenizer)
        .map_err(ParseError::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty() {
        let parse_ast = parse_program("", Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_print_hello() {
        let source = String::from("print('Hello world')");
        let parse_ast = parse_program(&source, Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_print_2() {
        let source = String::from("print('Hello world', 2)");
        let parse_ast = parse_program(&source, Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_kwargs() {
        let source = String::from("my_func('positional', keyword=2)");
        let parse_ast = parse_program(&source, Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_if_elif_else() {
        let source = String::from("if 1: 10\nelif 2: 20\nelse: 30");
        let parse_ast = parse_program(&source, Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_lambda() {
        let source = "lambda x, y: x * y"; // lambda(x, y): x * y";
        let parse_ast = parse_program(source, Default::default()).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_tuples() {
        let source = "a, b = 4, 5";

        insta::assert_debug_snapshot!(parse_program(source, Default::default()).unwrap());
    }

    #[test]
    fn test_parse_class() {
        let source = "\
class Foo(A, B):
 def __init__(self):
  pass
 def method_with_default(self, arg='default'):
  pass";
        insta::assert_debug_snapshot!(parse_program(source, Default::default()).unwrap());
    }

    #[test]
    fn test_parse_dict_comprehension() {
        let source = String::from("{x1: x2 for y in z}");
        let parse_ast = parse_expression(&source).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_list_comprehension() {
        let source = String::from("[x for y in z]");
        let parse_ast = parse_expression(&source).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }

    #[test]
    fn test_parse_double_list_comprehension() {
        let source = String::from("[x for y, y2 in z for a in b if a < 5 if a > 10]");
        let parse_ast = parse_expression(&source).unwrap();
        insta::assert_debug_snapshot!(parse_ast);
    }
    
    #[test]
    fn test_more_comment() {
        let source = "\
a: int # nac3: sf1
# nac3: sdf4
for i in (1, '12'): # nac3: sf2
    a: int
# nac3: 3
# nac3: 5
while i < 2: # nac3: 4
    # nac3: real pass
    pass
    # nac3: expr1
    # nac3: expr3
    1 + 2 # nac3: expr2
    # nac3: if3
    # nac3: if1
    if 1: # nac3: if2
        3";
        insta::assert_debug_snapshot!(parse_program(source, Default::default()).unwrap());
    }
    
    #[test]
    fn test_sample_comment() {
        let source = "\
# nac3: while1 
# nac3: while2 
# normal comment 
while test: # nac3: while3 
    # nac3: simple assign0 
    a = 3 # nac3: simple assign1
";
        insta::assert_debug_snapshot!(parse_program(source, Default::default()).unwrap());
    }

    #[test]
    fn test_comment_ambiguity() {
        let source = "\
if a: d; # nac3: for d
if b: c # nac3: for c
if d: # nac3: for if d
    b; b + 3; # nac3: for b + 3
a = 3; a + 3; b = a; # nac3: notif
# nac3: smallsingle1
# nac3: smallsingle3
aa = 3 # nac3: smallsingle2
if a: # nac3: small2
    a
for i in a: # nac3: for1
    pass
";
        insta::assert_debug_snapshot!(parse_program(source, Default::default()).unwrap());
    }

    #[test]
    fn test_comment_should_fail() {
        let source = "\
if a: # nac3: something
a = 3
";
        assert!(parse_program(source, Default::default()).is_err());
    }
}
