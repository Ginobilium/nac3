#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    None,
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
    Int(i128),
    Tuple(Vec<Constant>),
    Float(f64),
    Complex { real: f64, imag: f64 },
    Ellipsis,
}

impl From<String> for Constant {
    fn from(s: String) -> Constant {
        Self::Str(s)
    }
}
impl From<Vec<u8>> for Constant {
    fn from(b: Vec<u8>) -> Constant {
        Self::Bytes(b)
    }
}
impl From<bool> for Constant {
    fn from(b: bool) -> Constant {
        Self::Bool(b)
    }
}
impl From<i32> for Constant {
    fn from(i: i32) -> Constant {
        Self::Int(i as i128)
    }
}
impl From<i64> for Constant {
    fn from(i: i64) -> Constant {
        Self::Int(i as i128)
    }
}

/// Transforms a value prior to formatting it.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum ConversionFlag {
    /// Converts by calling `str(<value>)`.
    Str = b's',
    /// Converts by calling `ascii(<value>)`.
    Ascii = b'a',
    /// Converts by calling `repr(<value>)`.
    Repr = b'r',
}

impl ConversionFlag {
    pub fn try_from_byte(b: u8) -> Option<Self> {
        match b {
            b's' => Some(Self::Str),
            b'a' => Some(Self::Ascii),
            b'r' => Some(Self::Repr),
            _ => None,
        }
    }
}

#[cfg(feature = "constant-optimization")]
#[derive(Default)]
pub struct ConstantOptimizer {
    _priv: (),
}

#[cfg(feature = "constant-optimization")]
impl ConstantOptimizer {
    #[inline]
    pub fn new() -> Self {
        Self { _priv: () }
    }
}

#[cfg(feature = "constant-optimization")]
impl<U> crate::fold::Fold<U> for ConstantOptimizer {
    type TargetU = U;
    type Error = std::convert::Infallible;
    #[inline]
    fn map_user(&mut self, user: U) -> Result<Self::TargetU, Self::Error> {
        Ok(user)
    }
    fn fold_expr(&mut self, node: crate::Expr<U>) -> Result<crate::Expr<U>, Self::Error> {
        match node.node {
            crate::ExprKind::Tuple { elts, ctx } => {
                let elts = elts
                    .into_iter()
                    .map(|x| self.fold_expr(x))
                    .collect::<Result<Vec<_>, _>>()?;
                let expr = if elts
                    .iter()
                    .all(|e| matches!(e.node, crate::ExprKind::Constant { .. }))
                {
                    let tuple = elts
                        .into_iter()
                        .map(|e| match e.node {
                            crate::ExprKind::Constant { value, .. } => value,
                            _ => unreachable!(),
                        })
                        .collect();
                    crate::ExprKind::Constant {
                        value: Constant::Tuple(tuple),
                        kind: None,
                    }
                } else {
                    crate::ExprKind::Tuple { elts, ctx }
                };
                Ok(crate::Expr {
                    node: expr,
                    custom: node.custom,
                    location: node.location,
                })
            }
            _ => crate::fold::fold_expr(self, node),
        }
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "constant-optimization")]
    #[test]
    fn test_constant_opt() {
        use super::*;
        use crate::fold::Fold;
        use crate::*;

        let location = Location::new(0, 0, Default::default());
        let custom = ();
        let ast = Located {
            location,
            custom,
            node: ExprKind::Tuple {
                ctx: ExprContext::Load,
                elts: vec![
                    Located {
                        location,
                        custom,
                        node: ExprKind::Constant {
                            value: 1.into(),
                            kind: None,
                        },
                    },
                    Located {
                        location,
                        custom,
                        node: ExprKind::Constant {
                            value: 2.into(),
                            kind: None,
                        },
                    },
                    Located {
                        location,
                        custom,
                        node: ExprKind::Tuple {
                            ctx: ExprContext::Load,
                            elts: vec![
                                Located {
                                    location,
                                    custom,
                                    node: ExprKind::Constant {
                                        value: 3.into(),
                                        kind: None,
                                    },
                                },
                                Located {
                                    location,
                                    custom,
                                    node: ExprKind::Constant {
                                        value: 4.into(),
                                        kind: None,
                                    },
                                },
                                Located {
                                    location,
                                    custom,
                                    node: ExprKind::Constant {
                                        value: 5.into(),
                                        kind: None,
                                    },
                                },
                            ],
                        },
                    },
                ],
            },
        };
        let new_ast = ConstantOptimizer::new()
            .fold_expr(ast)
            .unwrap_or_else(|e| match e {});
        assert_eq!(
            new_ast,
            Located {
                location,
                custom,
                node: ExprKind::Constant {
                    value: Constant::Tuple(vec![
                        1.into(),
                        2.into(),
                        Constant::Tuple(vec![
                            3.into(),
                            4.into(),
                            5.into(),
                        ])
                    ]),
                    kind: None
                },
            }
        );
    }
}
