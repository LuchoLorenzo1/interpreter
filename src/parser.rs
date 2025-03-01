use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub statements: Vec<Statement>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            statements: vec![],
        }
    }

    pub fn parse_ast(&mut self) {
        self.statements.push(Statement::LetStatement(
            String::from("messi"),
            Expression::LiteralInteger(10),
        ))
    }
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(String, Expression),
    FunctionStatement,
    ReturnStatement,
}

#[derive(Debug)]
pub enum Expression {
    LiteralInteger(u32),
    LiteralString,
    Addition,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing_basic_statement() {
        let l = Lexer::new("let messi = 10;");
        let mut parser = Parser::new(l);
        parser.parse_ast();
        if let Some(Statement::LetStatement(a, b)) = parser.statements.first() {
            assert_eq!(a, "messi");
            if let Expression::LiteralInteger(c) = b {
                assert_eq!(*c, 10);
            }
        } else {
            assert!(false);
        }
    }
}
