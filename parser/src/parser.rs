#![allow(clippy::type_complexity)]

use nom::{
    branch::alt,
    combinator::{map, map_res},
    error::{Error as NomError, ErrorKind, ParseError},
    multi::{fold_many0, many0, separated_list0},
    sequence::{pair, tuple},
    IResult,
};

use crate::{
    ast::{
        ArithmBinaryOp, Ast, BasicBlock, BinaryOp, BitBinaryOp, Call, Function, GotoInsn, Insn,
        Literal, LogicalBinaryOp, RValue, Type, UnaryOp, Value, Variable,
    },
    lexer::{
        Keyword, Lexer,
        Token::{self, *},
    },
};

use super::{Error, Result};

fn nom_error(i: &[Token], kind: ErrorKind) -> nom::Err<NomError<&[Token]>> {
    nom::Err::Error(NomError::from_error_kind(i, kind))
}

/// Combinator that takes one token from the input.
fn take1(i: &[Token]) -> IResult<&[Token], Token> {
    let (first_token, remaining) = i
        .split_first()
        .ok_or_else(|| nom_error(i, ErrorKind::Complete))?;
    Ok((remaining, first_token.clone()))
}

/// Combinator that checks if an input starts from one of the enumerated tokens.
fn one_of<'a>(tokens: &'a [Token]) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], Token> {
    move |i| {
        map_res(take1, |first_token| {
            tokens
                .iter()
                .find(|&token| *token == first_token)
                .map(|_| first_token)
                .ok_or_else(|| nom_error(i, ErrorKind::OneOf))
        })(i)
    }
}

/// Combinator that wraps the result of the other combinator with option,
/// returning [`None`] if it return an recoverable error.
fn maybe<I: Clone, O, E: ParseError<I>, F>(
    mut parser: F,
) -> impl FnMut(I) -> IResult<I, Option<O>, E>
where
    F: nom::Parser<I, O, E>,
{
    move |i| {
        let i_cloned = i.clone();
        let res = parser.parse(i);
        match res {
            Ok((i, o)) => Ok((i, Some(o))),
            Err(nom::Err::Error(_)) => Ok((i_cloned, None)),
            Err(err) => Err(err),
        }
    }
}

/// Parse a type.
fn typ(i: &[Token]) -> IResult<&[Token], Type> {
    map(
        pair(
            one_of(&[
                Token::Keyword(Keyword::Void),
                Token::Keyword(Keyword::Int),
                Token::Keyword(Keyword::Unsigned),
                Token::Keyword(Keyword::Bool),
            ]),
            fold_many0(tok(Token::Star), || 0, |s, _| s + 1),
        ),
        |(token, stars)| {
            let typ = match token {
                Token::Keyword(Keyword::Void) => Type::Void,
                Token::Keyword(Keyword::Int) => Type::Int32,
                Token::Keyword(Keyword::Unsigned) => Type::Unsigned32,
                Token::Keyword(Keyword::Bool) => Type::Bool,
                _ => unreachable!(),
            };
            (0..stars).fold(typ, |acc, _| Type::Ref(Box::new(acc)))
        },
    )(i)
}

/// Parse a specified token.
fn tok(token: Token) -> impl Fn(&[Token]) -> IResult<&[Token], Token> {
    move |i| {
        map_res(take1, |first_token| {
            if token == first_token {
                Ok(token.clone())
            } else {
                Err(nom_error(i, ErrorKind::Tag))
            }
        })(i)
    }
}

/// Parse an ident.
fn ident(i: &[Token]) -> IResult<&[Token], Token> {
    map_res(take1, |token| match token {
        Token::Ident(_) => Ok(token),
        _ => Err(nom_error(i, ErrorKind::Tag)),
    })(i)
}

/// Parse a literal.
fn literal(i: &[Token]) -> IResult<&[Token], Token> {
    map_res(
        pair(maybe(one_of(&[Token::Plus, Token::Minus])), take1),
        |(sign, token)| match token {
            Token::Number(n) => match sign {
                Some(Token::Plus) | None => Ok(Token::Number(n)),
                // TODO: overflow.
                Some(Token::Minus) => Ok(Token::Number(-n)),
                _ => unreachable!(),
            },
            _ => Err(nom_error(i, ErrorKind::Tag)),
        },
    )(i)
}

/// Parse a function signature.
fn signature(i: &[Token]) -> IResult<&[Token], (Type, String, Vec<(Type, Variable)>)> {
    map(
        tuple((
            // Actual grammar is here.
            typ,
            ident,
            tok(LPar),
            separated_list0(tok(Comma), pair(typ, ident)),
            tok(RPar),
        )),
        |(return_type, name, _, decl, _)| {
            (
                return_type,
                name.into_ident().unwrap(),
                decl.into_iter()
                    .map(|(typ, name)| {
                        (
                            typ,
                            Variable {
                                name: name.into_ident().unwrap(),
                            },
                        )
                    })
                    .collect(),
            )
        },
    )(i)
}

/// Parse a `return` instruction.
fn ret(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        pair(tok(Token::Keyword(Keyword::Return)), value),
        |(_, value)| Insn::Return(value),
    )(i)
}

/// Parse a variable.
fn variable(i: &[Token]) -> IResult<&[Token], Variable> {
    map(ident, |token| Variable {
        name: token.into_ident().unwrap(),
    })(i)
}

/// Parse a binary operation.
fn binary_op(i: &[Token]) -> IResult<&[Token], (Value, BinaryOp, Value)> {
    fn operator(i: &[Token]) -> IResult<&[Token], BinaryOp> {
        map(
            one_of(&[
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Percent,
                Token::Gt,
                Token::Lt,
                Token::And,
                Token::Xor,
                Token::Or,
                Token::Eq,
                Token::Gq,
                Token::Lq,
                Token::Shl,
                Token::Shr,
            ]),
            |token| match token {
                // Arithmetic.
                Token::Plus => BinaryOp::ArithmOp(ArithmBinaryOp::Add),
                Token::Minus => BinaryOp::ArithmOp(ArithmBinaryOp::Sub),
                Token::Star => BinaryOp::ArithmOp(ArithmBinaryOp::Mul),
                Token::Slash => BinaryOp::ArithmOp(ArithmBinaryOp::Div),
                Token::Percent => BinaryOp::ArithmOp(ArithmBinaryOp::Mod),

                // Logical.
                Token::Gt => BinaryOp::LogicalOp(LogicalBinaryOp::Gt),
                Token::Lt => BinaryOp::LogicalOp(LogicalBinaryOp::Lt),
                Token::And => BinaryOp::LogicalOp(LogicalBinaryOp::And),
                Token::Xor => BinaryOp::LogicalOp(LogicalBinaryOp::Xor),
                Token::Or => BinaryOp::LogicalOp(LogicalBinaryOp::Or),
                Token::Eq => BinaryOp::LogicalOp(LogicalBinaryOp::Eq),
                Token::Gq => BinaryOp::LogicalOp(LogicalBinaryOp::Gq),
                Token::Lq => BinaryOp::LogicalOp(LogicalBinaryOp::Lq),

                Token::Shl => BinaryOp::BitOp(BitBinaryOp::Shl),
                Token::Shr => BinaryOp::BitOp(BitBinaryOp::Shr),
                _ => unreachable!(),
            },
        )(i)
    }

    tuple((value, operator, value))(i)
}

/// Parse an unary operation.
fn unary_op(i: &[Token]) -> IResult<&[Token], (UnaryOp, Value)> {
    fn operator(i: &[Token]) -> IResult<&[Token], UnaryOp> {
        map(
            one_of(&[
                Token::Minus,
                Token::Plus,
                Token::Star,
                Token::Not,
                Token::And,
                Token::Tilda,
            ]),
            |token| match token {
                Token::Minus => UnaryOp::Minus,
                Token::Plus => UnaryOp::Plus,
                Token::Not => UnaryOp::Not,
                Token::Star => UnaryOp::Deref,
                Token::And => UnaryOp::Ref,
                Token::Tilda => UnaryOp::Neg,
                _ => unreachable!(),
            },
        )(i)
    }

    pair(operator, value)(i)
}

/// Parse a function call.
fn call(i: &[Token]) -> IResult<&[Token], Call> {
    map(
        tuple((
            ident,
            tok(Token::LPar),
            separated_list0(tok(Token::Comma), value),
            tok(Token::RPar),
        )),
        |(fun, _, args, _)| Call {
            fun: fun.into_ident().unwrap(),
            args,
        },
    )(i)
}

/// Parse a rvalue.
fn rvalue(i: &[Token]) -> IResult<&[Token], RValue> {
    alt((
        map(unary_op, |(op, arg)| RValue::UnaryOp { op, arg }),
        map(binary_op, |(lhs, op, rhs)| RValue::BinaryOp {
            lhs,
            op,
            rhs,
        }),
        // Call must be considered prior to value, because of:
        // `int a = b();` vs `int a = b;`
        map(call, RValue::Call),
        map(value, RValue::Value),
    ))(i)
}

/// Parse a value.
fn value(i: &[Token]) -> IResult<&[Token], Value> {
    map(alt((ident, literal)), |token| match token {
        Token::Ident(n) => Value::Variable(Variable { name: n }),
        Token::Number(n) => Value::Literal(Literal { val: n }),
        _ => unreachable!(),
    })(i)
}

/// Parse an assign statement.
fn assign(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        tuple((variable, tok(Token::Assign), rvalue)),
        |(lhs, _, rhs)| Insn::Assign { lhs, rhs },
    )(i)
}

/// Parse an assign dereference statement.
fn assign_deref(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        tuple((tok(Token::Star), variable, tok(Token::Assign), value)),
        |(_, lhs, _, rhs)| Insn::AssignDeref { lhs, rhs },
    )(i)
}

/// Parse a goto instruction.
fn goto(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        pair(tok(Token::Keyword(Keyword::Goto)), ident),
        |(_, label)| {
            Insn::Goto(GotoInsn {
                label: label.into_ident().unwrap(),
            })
        },
    )(i)
}

/// Parse a conditional operator.
fn iff(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        tuple((
            tok(Token::Keyword(Keyword::If)),
            tok(Token::LPar),
            value,
            tok(Token::RPar),
            goto,
            maybe(tuple((
                tok(Token::Semicolon),
                tok(Token::Keyword(Keyword::Else)),
                goto,
            ))),
        )),
        |(_, _, condition, _, then, otherwise)| Insn::If {
            condition,
            then: then.into_goto().unwrap(),
            otherwise: otherwise.map(|(_, _, goto)| goto.into_goto().unwrap()),
        },
    )(i)
}

/// Parse a variable declaration.
fn declaration(i: &[Token]) -> IResult<&[Token], Insn> {
    map(
        tuple((typ, variable, maybe(pair(tok(Token::Assign), rvalue)))),
        |(typ, lhs, rhs)| Insn::Declaration {
            typ,
            lhs,
            rhs: rhs.map(|rhs| rhs.1),
        },
    )(i)
}

/// Parse a instruction.
fn insn(i: &[Token]) -> IResult<&[Token], Insn> {
    alt((ret, assign, assign_deref, goto, iff, declaration))(i)
}

/// Parse a basic block.
fn basic_block(i: &[Token]) -> IResult<&[Token], BasicBlock> {
    map(
        tuple((
            ident,
            tok(Token::Colon),
            separated_list0(tok(Token::Semicolon), insn),
            tok(Token::Semicolon),
        )),
        |(label, _, insns, _)| BasicBlock {
            label: label.into_ident().unwrap(),
            insns,
        },
    )(i)
}

/// Parse a function body.
fn body(i: &[Token]) -> IResult<&[Token], Vec<BasicBlock>> {
    map(many0(basic_block), |blocks| blocks)(i)
}

/// Parse a function definition.
fn function(i: &[Token]) -> IResult<&[Token], Function> {
    map(
        tuple((signature, tok(LCb), body, tok(RCb))),
        |((return_type, name, args), _, blocks, _)| Function {
            return_type,
            args,
            name,
            blocks,
        },
    )(i)
}

/// Parse a module definition.
fn module(i: &[Token]) -> IResult<&[Token], Ast> {
    map(many0(function), |funs| Ast { funs })(i)
}

fn run_parser<'a, O, E: std::fmt::Debug>(
    mut parser: impl nom::Parser<&'a [Token], O, E>,
    tokens: &'a [Token],
) -> Result<O> {
    let (rest, o) = parser.parse(tokens).expect("no unrecover errors");
    if rest.is_empty() {
        Ok(o)
    } else {
        Err(Error::ParseError)
    }
}

/// Helps to parse input to the AST.
pub struct Parser<'a> {
    input: &'a str,
}

impl<'a> Parser<'a> {
    /// Makes a new [`Parser`].
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Parse an input to AST consuming parser.
    pub fn parse(self) -> Result<Ast> {
        let tokens = Lexer::new(self.input).lex()?;
        println!("{:?}", tokens);
        run_parser(module, &tokens)
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Read;
    use std::path::PathBuf;

    use crate::lexer::{Keyword, Token};
    use crate::parser::run_parser;

    use super::super::{Error, Result};
    use super::{typ, Parser};

    const TEST_FILES_DIR_NAME: &str = "test_files";

    fn get_test_files_dir() -> PathBuf {
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
            .join("..")
            .join(TEST_FILES_DIR_NAME)
    }

    fn read_file(path: &PathBuf) -> Result<String> {
        (|| -> std::io::Result<String> {
            let mut content = String::with_capacity(64);
            let mut file = std::fs::File::open(path)?;
            file.read_to_string(&mut content)?;
            Ok(content)
        })()
        .map_err(Error::internal)
    }

    #[test]
    fn test_pointer_type() -> Result<()> {
        let tokens = [Token::Keyword(Keyword::Int), Token::Star, Token::Star];
        let typ = run_parser(typ, &tokens)?;
        assert_eq!(format!("{:?}", typ), "Ref(Ref(Int32))");
        Ok(())
    }

    #[test]
    fn test_simple_type() -> Result<()> {
        let tokens = [Token::Keyword(Keyword::Void)];
        let typ = run_parser(typ, &tokens)?;
        assert_eq!(format!("{:?}", typ), "Void");
        Ok(())
    }

    #[test]
    fn run_test_files() -> Result<()> {
        let test_dir = get_test_files_dir();

        for entry in std::fs::read_dir(test_dir).map_err(Error::internal)? {
            let entry = entry.map_err(Error::internal)?;
            let path = entry.path();

            if path.extension().and_then(|ext| ext.to_str()) == Some("ir") {
                println!("testing file: {:?}", path.file_name().unwrap());

                let mut expected_path = path.clone();
                expected_path.set_extension("ans");

                let expected = read_file(&expected_path)?;
                let prog = read_file(&path)?;

                let actual = format!("{}", Parser::new(&prog).parse()?);
                println!("{}\n", actual);

                assert_eq!(expected, actual);
                println!("\n")
            }
        }

        Ok(())
    }
}
