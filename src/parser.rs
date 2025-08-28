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

#[derive(Debug)]
pub enum Operator {
    EqualSign,
    Addition,
}

#[derive(Debug)]
pub enum Statement {
    // LetStatement(String, Expression),
    // ReturnStatement,
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Primary(Primary),
}

#[derive(Debug)]
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
        Ok(Expression::Binary(
            Box::new(Expression::Primary(self.primary()?)),
            Operator::Addition,
            Box::new(Expression::Primary(Primary::Integer(5))),
        ))
    }

    fn primary(&mut self) -> Result<Primary, ParserError> {
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

        Ok(p)
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    #[test]
    fn parsing_basic_statement() -> Result<(), Box<dyn Error>> {
        let l = Lexer::new("1");
        let mut parser = Parser::new(l);
        parser.parse_ast()?;

        assert_eq!(parser.statements.len(), 1);
        println!("{:#?}", parser.statements);

        // assert!(matches!(
        //     parser.statements[0],
        //     Statement::Expression(Expression::Primary(Primary::Integer(1)))
        // ));

        todo!();

        Ok(())
    }
}
