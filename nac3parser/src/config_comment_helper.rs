use lalrpop_util::ParseError;
use nac3ast::*;
use crate::ast::Ident;
use crate::ast::Location;
use crate::token::Tok;
use crate::error::*;

pub fn make_config_comment(
    com_loc: Location,
    stmt_loc: Location,
    nac3com_above: Vec<(Ident, Tok)>,
    nac3com_end: Option<Ident>
) -> Result<Vec<Ident>, ParseError<Location, Tok, LexicalError>> {
    if com_loc.column() != stmt_loc.column() && !nac3com_above.is_empty() {
        return Err(ParseError::User {
            error: LexicalError {
                location: com_loc,
                error: LexicalErrorType::OtherError(
                    format!(
                        "config comment at top must have the same indentation with what it applies(comment at {}, statement at {})",
                        com_loc,
                        stmt_loc,
                    )
                )
            }
        })
    };
    Ok(
        nac3com_above
            .into_iter()
            .map(|(com, _)| com)
            .chain(nac3com_end.map_or_else(|| vec![].into_iter(), |com| vec![com].into_iter()))
            .collect()
    )
}

pub fn handle_small_stmt<U>(stmts: &mut [Stmt<U>], nac3com_above: Vec<(Ident, Tok)>, nac3com_end: Option<Ident>, com_above_loc: Location) -> Result<(), ParseError<Location, Tok, LexicalError>> {
    if com_above_loc.column() != stmts[0].location.column() && !nac3com_above.is_empty() {
        return Err(ParseError::User {
            error: LexicalError {
                location: com_above_loc,
                error: LexicalErrorType::OtherError(
                    format!(
                        "config comment at top must have the same indentation with what it applies(comment at {}, statement at {})",
                        com_above_loc,
                        stmts[0].location,
                    )
                )
            }
        })
    }
    apply_config_comments(
        &mut stmts[0],
        nac3com_above
            .into_iter()
            .map(|(com, _)| com).collect()
    );
    apply_config_comments(
        stmts.last_mut().unwrap(),
        nac3com_end.map_or_else(Vec::new, |com| vec![com])
    );
    Ok(())
}

fn apply_config_comments<U>(stmt: &mut Stmt<U>, comments: Vec<Ident>) {
    match &mut stmt.node {
        StmtKind::Pass { config_comment, .. }
        | StmtKind::Delete { config_comment, .. }
        | StmtKind::Expr { config_comment, .. }
        | StmtKind::Assign { config_comment, .. }
        | StmtKind::AugAssign { config_comment, .. }
        | StmtKind::AnnAssign { config_comment, .. }
        | StmtKind::Break { config_comment, .. }
        | StmtKind::Continue { config_comment, .. }
        | StmtKind::Return { config_comment, .. } 
        | StmtKind::Raise { config_comment, .. }
        | StmtKind::Import { config_comment, .. }
        | StmtKind::ImportFrom { config_comment, .. }
        | StmtKind::Global { config_comment, .. }
        | StmtKind::Nonlocal { config_comment, .. }
        | StmtKind::Assert { config_comment, .. } => config_comment.extend(comments),

        _ => { unreachable!("only small statements should call this function") }
    }
}
