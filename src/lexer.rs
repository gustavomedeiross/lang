use logos::{Logos, SpannedIter};
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("(")]
    LParens,
    #[token(")")]
    RParens,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("=")]
    Equals,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("fun")]
    Fun,

    // TODO: handle parse error gracefully
    #[regex("-?[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    IntegerLiteral(i64),

    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| lex.slice().to_owned())]
    StringLiteral(String),

    #[regex("[a-z][_0-9a-zA-Z]*", |lex| lex.slice().to_owned())]
    CamelCaseIdentifier(String),

    #[regex("[A-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_owned())]
    PascalCaseIdentifier(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidToken,
}

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token>,
}

// TODO: remove allow
#[allow(dead_code)]
impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token_or_err, span)| match token_or_err {
                Ok(token) => Ok((span.start, token, span.end)),
                Err(_err) => Err(LexicalError::InvalidToken),
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let code = "fun -> let ident = 1 in \"hello\"";

        let result = Token::lexer(code)
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        assert_eq!(
            result,
            &[
                Token::Fun,
                Token::Arrow,
                Token::Let,
                Token::CamelCaseIdentifier("ident".to_owned()),
                Token::Equals,
                Token::IntegerLiteral(1),
                Token::In,
                Token::StringLiteral("\"hello\"".to_owned()),
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let result = Token::lexer("Show")
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        assert_eq!(result, &[Token::PascalCaseIdentifier("Show".to_owned()),]);

        let result = Token::lexer("abc")
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        assert_eq!(result, &[Token::CamelCaseIdentifier("abc".to_owned()),]);
    }
}
