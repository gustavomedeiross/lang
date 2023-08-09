mod ast;
mod lexer;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub grammar);

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod parser_tests {
    use super::ast::{Literal, ParsedExpr};
    use super::*;

    fn parse(input: &str) -> ParsedExpr {
        let tokens = lexer::Lexer::new(input);
        let result = grammar::ExprParser::new()
            .parse(tokens)
            .expect("parsing failed");
        *result
    }

    #[test]
    fn test_1() {
        let result = parse("1");
        assert_eq!(result, ParsedExpr::Lit(Literal::Int(1)));
    }
}
