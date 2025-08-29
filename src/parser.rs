use std::iter::Peekable;

use crate::{
    lexer::{Lexer, Token},
    parser_error::ParserError,
};

pub struct Parser<'a> {
    pub lexer: Peekable<Lexer<'a>>,
    pub statements: Vec<Statement>,
}

/// Backus-Naur Form (BNF) for the grammar:
// expression     -> equality ;
// equality       -> comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           -> factor ( ( "-" | "+" ) factor )* ;
// factor         -> unary ( ( "/" | "*" ) unary )* ;
// unary          -> ( "!" | "-" ) unary | primary ;
// primary        -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

#[derive(Debug, PartialEq)]
pub enum Statement {
    // LetStatement(String, Expression),
    // ReturnStatement,
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Primary(Primary),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equality,
    Inequality,
    Addition,
}

#[derive(Debug, PartialEq)]
pub enum Primary {
    Integer(u32),
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
            statements: vec![],
        }
    }

    pub fn parse_ast(&mut self) -> Result<(), ParserError> {
        if let None = self.lexer.peek() {
            return Ok(());
        }
        let expr = self.expression()?;
        self.statements.push(Statement::Expression(expr));
        Ok(())
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        let primary_left = self.primary()?;

        let op = self.lexer.peek().and_then(|tok| match tok {
            Token::DoubleEqualSign => Some(Operator::Equality),
            Token::NotEqualSign => Some(Operator::Inequality),
            _ => None,
        });

        if let Some(operator) = op {
            self.lexer.next();
            let primary_right = self.primary()?;

            Ok(Expression::Binary(
                Box::new(primary_left),
                operator,
                Box::new(primary_right),
            ))
        } else {
            Ok(primary_left)
        }
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        if let None = self.lexer.peek() {
            return Err(ParserError::InvalidSyntax(String::from("Invalid syntax")));
        }

        let p = match self.lexer.next().unwrap() {
            Token::Integer(u) => Primary::Integer(u),
            t => Err(ParserError::InvalidSyntax(format!(
                "Unexpected token: {:?}",
                t
            )))?,
        };

        Ok(Expression::Primary(p))
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn parsing_equality_statement() -> Result<(), Box<dyn Error>> {
        let l = Lexer::new("1 == 1");
        let mut parser = Parser::new(l);
        parser.parse_ast()?;

        assert_eq!(parser.statements.len(), 1);
        println!("{:#?}", parser.statements);
        assert_eq!(
            parser.statements[0],
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::Integer(1))),
                Operator::Equality,
                Box::new(Expression::Primary(Primary::Integer(1))),
            )),
        );

        let l = Lexer::new("1 != 1");
        let mut parser = Parser::new(l);
        parser.parse_ast()?;

        assert_eq!(parser.statements.len(), 1);
        println!("{:#?}", parser.statements);
        assert_eq!(
            parser.statements[0],
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::Integer(1))),
                Operator::Inequality,
                Box::new(Expression::Primary(Primary::Integer(1))),
            )),
        );

        Ok(())
    }
}
