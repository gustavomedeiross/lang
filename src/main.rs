use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
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
    #[token("->")]
    Arrow,
    #[token("fun")]
    Fun,

    // TODO: handle parse error gracefully
    #[regex("-?[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    IntegerLiteral(i64),

    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| lex.slice().to_owned())]
    StringLiteral(String),

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_owned())]
    Identifier(String),
}


fn main() {
    let mut lex = Token::lexer("fun -> let ident = 1 in \"hello\"");

    assert_eq!(lex.next(), Some(Ok(Token::Fun)));
    assert_eq!(lex.next(), Some(Ok(Token::Arrow)));
    assert_eq!(lex.next(), Some(Ok(Token::Let)));
    assert_eq!(lex.next(), Some(Ok(Token::Identifier("ident".to_owned()))));
    assert_eq!(lex.next(), Some(Ok(Token::Equals)));
    assert_eq!(lex.next(), Some(Ok(Token::IntegerLiteral(1))));
    assert_eq!(lex.next(), Some(Ok(Token::In)));
    assert_eq!(lex.next(), Some(Ok(Token::StringLiteral("\"hello\"".to_owned()))));

    println!("Hello, world!");
}
