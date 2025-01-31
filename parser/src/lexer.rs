use std::{collections::HashMap, fmt::Write};

use lazy_static::lazy_static;

use super::{Error, Result};

/// Represents the set of reserved keywords.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    If,       // `if`
    Else,     // `else`
    Int,      // `int`
    Unsigned, // `unsigned`
    Void,     // `void`
    Bool,     // `bool `
    Goto,     // `goto`
    Return,   // `return`
}

impl Keyword {
    fn from_str(kw: &str) -> Option<Self> {
        match kw {
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "int" => Some(Self::Int),
            "unsigned" => Some(Self::Unsigned),
            "void" => Some(Self::Void),
            "bool" => Some(Self::Bool),
            "goto" => Some(Self::Goto),
            "return" => Some(Self::Return),
            _ => None,
        }
    }
}

/// Represents the set of recognizable tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Eq,    // '=='
    Gq,    // '>='
    Lq,    // '<='
    Shl,   // `>>`
    Shr,   // `<<`,
    Tilda, // `~`

    LPar, // `(`
    RPar, // `)`
    LSq,  // `[`
    RSq,  // `]`
    LCb,  // `{`
    RCb,  // `}`

    Plus,    // `+`
    Minus,   // `-`
    Star,    // `*`
    Slash,   // `/`
    Percent, // `%`
    Assign,  // `=`

    Gt,  // '>'
    Lt,  // '<'
    And, // '&'
    Xor, // '^'
    Or,  // '|'
    Not, // '!'

    Comma,     // ','
    Semicolon, // `;`
    Colon,     // `:`

    Keyword(Keyword),
    Ident(String), // [a-zA-Z][a-zA-Z0-9]*
    Number(u64),
}

impl Token {
    pub fn into_ident(self) -> Option<String> {
        match self {
            Self::Ident(n) => Some(n),
            _ => None,
        }
    }
}

/// Make a new ident.
fn ident<T: Into<String>>(s: T) -> Token {
    Token::Ident(s.into())
}

/// Macro for tokens that are constructable without possible errors.
macro_rules! primitive_tokens {
    ( $(($regex: literal, $token: expr)),* $(,)? ) => {{
        let v: Vec::<(&'static str, fn(&str) -> Result<Token>)> = vec![
            $(($regex, |_| Ok($token))),*
        ];
        v
    }};
}

lazy_static! {
    // Regexes are matched greedily from the first to the last.
    static ref TOKEN_REGEXS: Vec<(&'static str, fn(&str) -> Result<Token>)> = {
        // Non-primitive tokens.
        let mut regexs: Vec<(&'static str, fn(&str) -> Result<Token>)> = vec![
            (r"[+-]?[0-9]+", |a| parse_number(a).map(Token::Number)),
            (r"[a-zA-Z_][a-zA-Z0-9_]*", |a: &str| {
                Ok(
                    // Firstly try to extract keyword, then make an ident.
                    Keyword::from_str(&a)
                    .map(Token::Keyword)
                    .unwrap_or_else(|| Token::Ident(a.to_owned()) )
                )
            })
        ];
        // Primitive tokens.
        regexs.append(&mut primitive_tokens! {
            (r"==", Token::Eq),
            (r">=", Token::Gq),
            (r"<=", Token::Lq),
            (r"<<", Token::Shl),
            (r">>", Token::Shr),
            (r"~", Token::Tilda),
            (r"\(", Token::LPar),
            (r"\)", Token::RPar),
            (r"\[", Token::LSq),
            (r"\]", Token::RSq),
            (r"\{", Token::LCb),
            (r"\}", Token::RCb),
            (r"\+", Token::Plus),
            (r"\-", Token::Minus),
            (r"\*", Token::Star),
            (r"/", Token::Slash),
            (r"%", Token::Percent),
            (r"=", Token::Assign),
            (r">", Token::Gt),
            (r"<", Token::Lt),
            (r"&", Token::And),
            (r"\^", Token::Xor),
            (r"\|", Token::Or),
            (r"!", Token::Not),
            (r",", Token::Comma),
            (r";", Token::Semicolon),
            (r":", Token::Colon)
        });
        regexs
    };
}

/// Lexer splits an input stream into the tokens.
pub struct Lexer<'a> {
    input: &'a str,
    regexp: regex::Regex,
    /// Token constructors for specific match groups.
    cons: HashMap<usize, &'static fn(&str) -> Result<Token>>,
}

fn parse_number(s: &str) -> Result<u64> {
    s.parse().map_err(Error::ParseIntError)
}

impl<'a> Lexer<'a> {
    /// Make a new [`Lexer`].
    pub fn new(input: &'a str) -> Self {
        let mut cons = HashMap::new();
        let mut groups = String::with_capacity(512);

        // Collect regexs for tokens.
        for (i, (re, con)) in TOKEN_REGEXS.iter().enumerate() {
            write!(&mut groups, "|({})", re).unwrap();
            // Token groups started from 2, first two groups are reserved.
            cons.insert(i + 2, con);
        }

        // Match groups are:
        // * The whole matched.
        // * EOF.
        // * Token specific groups.
        let regexp = regex::Regex::new(&format!(r"^\s*(?:($){})", groups)).unwrap();
        Self {
            input,
            regexp,
            cons,
        }
    }

    /// Get next token from the input.
    /// If EOF is reached, returns [`None`].
    ///
    pub fn next(&mut self) -> Result<Option<Token>> {
        if self.input.is_empty() {
            // EOF.
            Ok(None)
        } else {
            match self.regexp.captures(&self.input) {
                Some(matched) => {
                    // Shift input.
                    // The whole match (0-group) always starts from the beginning.
                    self.input = &self.input[matched.get(0).unwrap().end()..];

                    Ok(if matched.get(1).is_some() {
                        // EOF matched.
                        None
                    } else {
                        for (group_num, cons) in self.cons.iter() {
                            if let Some(matched) = matched.get(*group_num) {
                                return cons(matched.as_str()).map(Some);
                            }
                        }
                        // If there is matching then some group is matched.
                        unreachable!()
                    })
                }
                None => Err(Error::UnexepectedTokens(self.input.to_owned())),
            }
        }
    }

    /// Split the input into the vector of tokens.
    pub fn lex(mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::with_capacity(128);
        loop {
            let nxt = self.next()?;
            if let Some(nxt) = nxt {
                tokens.push(nxt);
            } else {
                break;
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use {
        super::{Lexer, Result},
        crate::lexer::{ident, Keyword, Token::*},
    };

    #[test]
    fn test_lex() -> Result<()> {
        let input = "
void foo() {
    int a, b, c;
    a = 0;
    b=4; c=5;
    a=b+c;
}
";
        let tokens = Lexer::new(&input).lex()?;
        assert_eq!(
            tokens,
            vec![
                Keyword(Keyword::Void),
                ident("foo"),
                LPar,
                RPar,
                LCb,
                Keyword(Keyword::Int),
                ident("a"),
                Comma,
                ident("b"),
                Comma,
                ident("c"),
                Semicolon,
                ident("a"),
                Assign,
                Number(0),
                Semicolon,
                ident("b"),
                Assign,
                Number(4),
                Semicolon,
                ident("c"),
                Assign,
                Number(5),
                Semicolon,
                ident("a"),
                Assign,
                ident("b"),
                Plus,
                ident("c"),
                Semicolon,
                RCb,
            ]
        );

        Ok(())
    }
}
