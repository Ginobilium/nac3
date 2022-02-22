use std::collections::HashMap;
use std::fmt::Display;

use crate::typecheck::typedef::TypeEnum;

use super::typedef::{RecordKey, Type, Unifier};
use nac3parser::ast::{Location, StrRef};

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    TooManyArguments {
        expected: usize,
        got: usize,
    },
    MissingArgs(String),
    UnknownArgName(StrRef),
    IncorrectArgType {
        name: StrRef,
        expected: Type,
        got: Type,
    },
    FieldUnificationError {
        field: RecordKey,
        types: (Type, Type),
        loc: (Option<Location>, Option<Location>),
    },
    IncompatibleRange(Type, Vec<Type>),
    IncompatibleTypes(Type, Type),
    MutationError(RecordKey, Type),
    NoSuchField(RecordKey, Type),
    TupleIndexOutOfBounds {
        index: i32,
        len: i32,
    },
    RequiresTypeAnn,
    PolymorphicFunctionPointer,
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub loc: Option<Location>,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, loc: Option<Location>) -> TypeError {
        TypeError { kind, loc }
    }

    pub fn at(mut self, loc: Option<Location>) -> TypeError {
        self.loc = self.loc.or(loc);
        self
    }

    pub fn to_display(self, unifier: &Unifier) -> DisplayTypeError {
        DisplayTypeError { err: self, unifier }
    }
}

pub struct DisplayTypeError<'a> {
    pub err: TypeError,
    pub unifier: &'a Unifier,
}

fn loc_to_str(loc: Option<Location>) -> String {
    match loc {
        Some(loc) => format!("(in {})", loc),
        None => "".to_string(),
    }
}

impl<'a> Display for DisplayTypeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use TypeErrorKind::*;
        let mut notes = Some(HashMap::new());
        match &self.err.kind {
            TooManyArguments { expected, got } => {
                write!(f, "Too many arguments. Expected {} but got {}", expected, got)
            }
            MissingArgs(args) => {
                write!(f, "Missing arguments: {}", args)
            }
            UnknownArgName(name) => {
                write!(f, "Unknown argument name: {}", name)
            }
            IncorrectArgType { name, expected, got } => {
                let expected = self.unifier.stringify_with_notes(*expected, &mut notes);
                let got = self.unifier.stringify_with_notes(*got, &mut notes);
                write!(
                    f,
                    "Incorrect argument type for {}. Expected {}, but got {}",
                    name, expected, got
                )
            }
            FieldUnificationError { field, types, loc } => {
                let lhs = self.unifier.stringify_with_notes(types.0, &mut notes);
                let rhs = self.unifier.stringify_with_notes(types.1, &mut notes);
                write!(
                    f,
                    "Unable to unify field {}: Got types {}{} and {}{}",
                    field,
                    lhs,
                    loc_to_str(loc.0),
                    rhs,
                    loc_to_str(loc.1)
                )
            }
            IncompatibleRange(t, ts) => {
                let t = self.unifier.stringify_with_notes(*t, &mut notes);
                let ts = ts
                    .iter()
                    .map(|t| self.unifier.stringify_with_notes(*t, &mut notes))
                    .collect::<Vec<_>>();
                write!(f, "Expected any one of these types: {}, but got {}", ts.join(", "), t)
            }
            IncompatibleTypes(t1, t2) => {
                let type1 = self.unifier.get_ty_immutable(*t1);
                let type2 = self.unifier.get_ty_immutable(*t2);
                match (&*type1, &*type2) {
                    (TypeEnum::TCall(calls), _) => {
                        let loc = self.unifier.calls[calls[0].0].loc;
                        let result = write!(
                            f,
                            "{} is not callable",
                            self.unifier.stringify_with_notes(*t2, &mut notes)
                        );
                        if let Some(loc) = loc {
                            result?;
                            write!(f, " (in {})", loc)?;
                            return Ok(());
                        }
                        result
                    }
                    (TypeEnum::TTuple { ty: ty1 }, TypeEnum::TTuple { ty: ty2 })
                        if ty1.len() != ty2.len() =>
                    {
                        let t1 = self.unifier.stringify_with_notes(*t1, &mut notes);
                        let t2 = self.unifier.stringify_with_notes(*t2, &mut notes);
                        write!(f, "Tuple length mismatch: got {} and {}", t1, t2)
                    }
                    _ => {
                        let t1 = self.unifier.stringify_with_notes(*t1, &mut notes);
                        let t2 = self.unifier.stringify_with_notes(*t2, &mut notes);
                        write!(f, "Incompatible types: {} and {}", t1, t2)
                    }
                }
            }
            MutationError(name, t) => {
                if let TypeEnum::TTuple { .. } = &*self.unifier.get_ty_immutable(*t) {
                    write!(f, "Cannot assign to an element of a tuple")
                } else {
                    let t = self.unifier.stringify_with_notes(*t, &mut notes);
                    write!(f, "Cannot assign to field {} of {}, which is immutable", name, t)
                }
            }
            NoSuchField(name, t) => {
                let t = self.unifier.stringify_with_notes(*t, &mut notes);
                write!(f, "`{}::{}` field/method does not exist", t, name)
            }
            TupleIndexOutOfBounds { index, len } => {
                write!(
                    f,
                    "Tuple index out of bounds. Got {} but tuple has only {} elements",
                    index, len
                )
            }
            RequiresTypeAnn => {
                write!(f, "Unable to infer virtual object type: Type annotation required")
            }
            PolymorphicFunctionPointer => {
                write!(f, "Polymorphic function pointers is not supported")
            }
        }?;
        if let Some(loc) = self.err.loc {
            write!(f, " at {}", loc)?;
        }
        let notes = notes.unwrap();
        if !notes.is_empty() {
            write!(f, "\n\nNotes:")?;
            for line in notes.values() {
                write!(f, "\n    {}", line)?;
            }
        }
        Ok(())
    }
}
