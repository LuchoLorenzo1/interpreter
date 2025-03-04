use std::iter::Peekable;

use crate::{
    lexer::{Lexer, Token},
    parser_error::ParserError,
};

pub struct Parser<'a> {
    pub lexer: Peekable<Lexer<'a>>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(String, Expression),
    ReturnStatement,
    ExpressionStatement,
}

#[derive(Debug)]
pub enum Expression {
    LiteralInteger(u32),
    Identifier(String),
    MinusPrefix(Box<Expression>),
    Addition,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
            statements: vec![],
        }
    }

    pub fn parse_ast(&mut self) -> Result<(), ParserError> {
        use crate::lexer::Keyword;
        use crate::lexer::Token;

        let token = match self.lexer.next() {
            None => return Ok(()),
            Some(t) => t,
        };

        let st = match token {
            Token::Keyword(Keyword::Let) => self.parse_let_statement()?,
            // Token::Keyword(Keyword::Let) => Fn
            // Token::Keyword(Keyword::Let) => LetStatement
            _ => {
                return Err(ParserError::InvalidSyntax(
                    "Program must begin with a statement".to_string(),
                ));
            }
        };

        self.statements.push(st);

        Ok(())
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let token = match self.lexer.next() {
            None => {
                return Err(ParserError::InvalidSyntax(
                    "Invalid Let statement".to_string(),
                ));
            }
            Some(i) => i,
        };

        let identifier_name = match token {
            Token::Identifier(s) => s,
            _ => {
                return Err(ParserError::MissingToken(
                    "Identifier in let statement".to_string(),
                ));
            }
        };

        match self.lexer.next() {
            Some(Token::EqualSign) => true,
            _ => {
                return Err(ParserError::MissingToken(
                    "Equal sign in let statement".to_string(),
                ));
            }
        };

        let mut expressions: Vec<Token> = vec![];
        loop {
            let token = match self.lexer.next() {
                Some(i) => i,
                None => {
                    return Err(ParserError::MissingToken(
                        "Semicolon at the end of let statement".to_string(),
                    ));
                }
            };

            match token {
                Token::Semicolon => break,
                _ => {
                    expressions.push(token);
                }
            }
        }

        let expression: Expression = parse_expression(&expressions)?;

        let st = Statement::LetStatement(identifier_name, expression);

        Ok(st)
    }
}

pub fn parse_expression(tokens: &[Token]) -> Result<Expression, ParserError> {
    if tokens.len() < 1 {
        return Err(ParserError::InvalidSyntax("Empty expression".to_string()));
    }

    let expression = match tokens.iter().next() {
        Some(Token::Integer(i)) => Expression::LiteralInteger(*i),
        Some(Token::Identifier(a)) => Expression::Identifier(a.to_string()),
        Some(Token::MinusSign) => {
            let xp: Expression = parse_expression(&tokens[1..])?;
            Expression::MinusPrefix(Box::new(xp))
        }
        _ => return Err(ParserError::InvalidSyntax("Empty expression".to_string())),
    };

    Ok(expression)
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn parsing_basic_statement() -> Result<(), Box<dyn Error>> {
        let l = Lexer::new("let messi = 10;");
        let mut parser = Parser::new(l);
        parser.parse_ast()?;
        if let Some(Statement::LetStatement(a, b)) = parser.statements.first() {
            assert_eq!(a, "messi");
            if let Expression::LiteralInteger(c) = b {
                assert_eq!(*c, 10);
            }
        } else {
            assert!(false);
        }
        Ok(())
    }
}
