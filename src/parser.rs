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
    Unary(Operator, Box<Expression>),
    Primary(Primary),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equality,
    Inequality,
    Addition,
    LogicalNot,
    Negation,
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
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
        let comparison_left = self.comparison()?;

        let op = self.lexer.peek().and_then(|tok| match tok {
            Token::DoubleEqualSign => Some(Operator::Equality),
            Token::NotEqualSign => Some(Operator::Inequality),
            _ => None,
        });

        if let Some(operator) = op {
            self.lexer.next();
            let comparison_right = self.comparison()?;

            Ok(Expression::Binary(
                Box::new(comparison_left),
                operator,
                Box::new(comparison_right),
            ))
        } else {
            Ok(comparison_left)
        }
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        self.term()
    }

    fn term(&mut self) -> Result<Expression, ParserError> {
        self.factor()
    }

    fn factor(&mut self) -> Result<Expression, ParserError> {
        self.unary()
    }

    fn unary(&mut self) -> Result<Expression, ParserError> {
        let op = self.lexer.peek().and_then(|tok| match tok {
            Token::NotSign => Some(Operator::LogicalNot),
            Token::MinusSign => Some(Operator::Negation),
            _ => None,
        });

        if let Some(operator) = op {
            self.lexer.next();
            Ok(Expression::Unary(operator, Box::new(self.primary()?)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
        match self.lexer.peek() {
            None => return Err(ParserError::InvalidSyntax("Invalid syntax".into()))?,
            Some(Token::NotSign | Token::MinusSign) => {
                return self.expression();
            }
            Some(_) => {}
        };

        let p = match self.lexer.next().unwrap() {
            Token::Integer(u) => Expression::Primary(Primary::Integer(u)),
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

    #[test]
    fn parsing_unary_expression() -> Result<(), Box<dyn Error>> {
        let expression = vec!["!1", "-1", "!!1", "--1", "!-!-4"];
        let expected_results = vec![
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Primary(Primary::Integer(1))),
            )),
            Statement::Expression(Expression::Unary(
                Operator::Negation,
                Box::new(Expression::Primary(Primary::Integer(1))),
            )),
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Unary(
                    Operator::LogicalNot,
                    Box::new(Expression::Primary(Primary::Integer(1))),
                )),
            )),
            Statement::Expression(Expression::Unary(
                Operator::Negation,
                Box::new(Expression::Unary(
                    Operator::Negation,
                    Box::new(Expression::Primary(Primary::Integer(1))),
                )),
            )),
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Unary(
                    Operator::Negation,
                    Box::new(Expression::Unary(
                        Operator::LogicalNot,
                        Box::new(Expression::Unary(
                            Operator::Negation,
                            Box::new(Expression::Primary(Primary::Integer(4))),
                        )),
                    )),
                )),
            )),
        ];

        for (i, expr) in expression.iter().enumerate() {
            let l = Lexer::new(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast()?;

            assert_eq!(parser.statements.len(), 1);
            println!("{:#?}", parser.statements);
            assert_eq!(parser.statements[0], expected_results[i]);
        }

        Ok(())
    }
}
