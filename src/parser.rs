use std::iter::Peekable;

use crate::{
    lexer::{Keyword, Lexer, Token},
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
    /// Operators for Equality
    Equality,
    Inequality,

    /// Operators for Comparison
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,

    /// Operators for Term
    Addition,
    Subtraction,

    /// Operators for Factor
    Multiplication,
    Division,

    /// Operators for Unary
    LogicalNot,
    Negation,
}

#[derive(Debug, PartialEq)]
pub enum Primary {
    Integer(u32),
    String(String),
    Null,
    False,
    True,
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
        let mut comparison_left = self.comparison()?;

        while let Some(tok) = self.lexer.peek() {
            let op = match tok {
                Token::DoubleEqualSign => Some(Operator::Equality),
                Token::NotEqualSign => Some(Operator::Inequality),
                _ => break,
            };

            if let Some(operator) = op {
                self.lexer.next();

                let tmp = Expression::Binary(
                    Box::new(comparison_left),
                    operator,
                    Box::new(self.comparison()?),
                );

                comparison_left = tmp;
            }
        }

        Ok(comparison_left)
    }

    fn comparison(&mut self) -> Result<Expression, ParserError> {
        let mut term = self.term()?;

        while let Some(tok) = self.lexer.peek() {
            let op = match tok {
                Token::GreaterThan => Some(Operator::GreaterThan),
                Token::GreaterThanOrEqual => Some(Operator::GreaterThanOrEqual),
                Token::LessThan => Some(Operator::LessThan),
                Token::LessThanOrEqual => Some(Operator::LessThanOrEqual),
                _ => break,
            };

            if let Some(operator) = op {
                self.lexer.next();
                let right_term = self.term()?;

                let tmp = Expression::Binary(Box::new(term), operator, Box::new(right_term));
                term = tmp;
            };
        }

        Ok(term)
    }

    fn term(&mut self) -> Result<Expression, ParserError> {
        let mut factor = self.factor()?;

        while let Some(tok) = self.lexer.peek() {
            let op = match tok {
                Token::PlusSign => Some(Operator::Addition),
                Token::MinusSign => Some(Operator::Subtraction),
                _ => break,
            };

            if let Some(operator) = op {
                self.lexer.next();
                let tmp = Ok(Expression::Binary(
                    Box::new(factor),
                    operator,
                    Box::new(self.factor()?),
                ));

                factor = tmp?;
            }
        }

        Ok(factor)
    }

    fn factor(&mut self) -> Result<Expression, ParserError> {
        let mut unary = self.unary()?;

        while let Some(tok) = self.lexer.peek() {
            let op = match tok {
                Token::Asterisk => Some(Operator::Multiplication),
                Token::Slash => Some(Operator::Division),
                _ => break,
            };

            if let Some(operator) = op {
                self.lexer.next();
                let tmp = Ok(Expression::Binary(
                    Box::new(unary),
                    operator,
                    Box::new(self.unary()?),
                ));

                unary = tmp?;
            }
        }

        Ok(unary)
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
            Some(Token::OpenParenthesis) => {
                self.lexer.next();
                let right_expr = self.expression()?;

                let next_token = self.lexer.next();
                if let Some(Token::CloseParenthesis) = next_token {
                    return Ok(right_expr);
                } else {
                    return Err(ParserError::InvalidSyntax(format!(
                        "Expected closing parenthesis, found: {:?}",
                        next_token
                    )));
                }
            }
            Some(a) => {
                println!("Primary token: {:?}", a);
            }
        };

        let p = match self.lexer.next().unwrap() {
            Token::Integer(u) => Primary::Integer(u),
            Token::String(s) => Primary::String(s),
            Token::Keyword(Keyword::True) => Primary::True,
            Token::Keyword(Keyword::False) => Primary::False,
            Token::Keyword(Keyword::Null) => Primary::Null,
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

    fn int(i: u32) -> Box<Expression> {
        Box::new(Expression::Primary(Primary::Integer(i)))
    }

    fn bin(a: Box<Expression>, op: Operator, b: Box<Expression>) -> Box<Expression> {
        Box::new(Expression::Binary(a, op, b))
    }

    fn match_statements(
        expressions: Vec<&str>,
        expected_results: Vec<Statement>,
    ) -> Result<(), Box<dyn Error>> {
        for (i, expr) in expressions.iter().enumerate() {
            let l = Lexer::new(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast()?;

            assert_eq!(parser.statements.len(), 1);
            println!("{:#?}", parser.statements);
            assert_eq!(parser.statements[0], expected_results[i]);
        }

        Ok(())
    }

    #[test]
    fn parsing_equality_statement() -> Result<(), Box<dyn Error>> {
        let expression = vec!["1 == 1", "1 != 1"];
        let expected_results = vec![
            Statement::Expression(Expression::Binary(int(1), Operator::Equality, int(1))),
            Statement::Expression(Expression::Binary(int(1), Operator::Inequality, int(1))),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_unary_expression() -> Result<(), Box<dyn Error>> {
        let expression = vec!["!1", "-1", "!!1", "--1", "!-!-4"];
        let expected_results = vec![
            Statement::Expression(Expression::Unary(Operator::LogicalNot, int(1))),
            Statement::Expression(Expression::Unary(Operator::Negation, int(1))),
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Unary(Operator::LogicalNot, int(1))),
            )),
            Statement::Expression(Expression::Unary(
                Operator::Negation,
                Box::new(Expression::Unary(Operator::Negation, int(1))),
            )),
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Unary(
                    Operator::Negation,
                    Box::new(Expression::Unary(
                        Operator::LogicalNot,
                        Box::new(Expression::Unary(Operator::Negation, int(4))),
                    )),
                )),
            )),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_basic_comparisons() -> Result<(), Box<dyn Error>> {
        let expression = vec!["1 > 1", "1 >= 1", "1 < 1", "1 <= 1"];
        let expected_results = vec![
            Statement::Expression(Expression::Binary(int(1), Operator::GreaterThan, int(1))),
            Statement::Expression(Expression::Binary(
                int(1),
                Operator::GreaterThanOrEqual,
                int(1),
            )),
            Statement::Expression(Expression::Binary(int(1), Operator::LessThan, int(1))),
            Statement::Expression(Expression::Binary(
                int(1),
                Operator::LessThanOrEqual,
                int(1),
            )),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_basic_factor_and_terms() -> Result<(), Box<dyn Error>> {
        let expression = vec!["1 + 1", "1 - 1", "1 * 1", "1 / 1"];
        let expected_results = vec![
            Statement::Expression(Expression::Binary(int(1), Operator::Addition, int(1))),
            Statement::Expression(Expression::Binary(int(1), Operator::Subtraction, int(1))),
            Statement::Expression(Expression::Binary(int(1), Operator::Multiplication, int(1))),
            Statement::Expression(Expression::Binary(int(1), Operator::Division, int(1))),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_concatenating_expressions() -> Result<(), Box<dyn Error>> {
        let expression = vec![
            "1 == 1 == 1",
            "1 != 1 == 1",
            "1 + 1 + 5",
            "1-2+3-4+5-6+7-8",
            "!1 + !1 - 1 == 5+1",
            "5*5/5 >= 1 <= 5+5",
        ];
        let expected_results = vec![
            Statement::Expression(Expression::Binary(
                bin(int(1), Operator::Equality, int(1)),
                Operator::Equality,
                int(1),
            )),
            Statement::Expression(Expression::Binary(
                bin(int(1), Operator::Inequality, int(1)),
                Operator::Equality,
                int(1),
            )),
            Statement::Expression(Expression::Binary(
                bin(int(1), Operator::Addition, int(1)),
                Operator::Addition,
                int(5),
            )),
            Statement::Expression(Expression::Binary(
                bin(
                    bin(
                        bin(
                            bin(
                                bin(
                                    bin(int(1), Operator::Subtraction, int(2)),
                                    Operator::Addition,
                                    int(3),
                                ),
                                Operator::Subtraction,
                                int(4),
                            ),
                            Operator::Addition,
                            int(5),
                        ),
                        Operator::Subtraction,
                        int(6),
                    ),
                    Operator::Addition,
                    int(7),
                ),
                Operator::Subtraction,
                int(8),
            )),
            Statement::Expression(Expression::Binary(
                bin(
                    bin(
                        Box::new(Expression::Unary(Operator::LogicalNot, int(1))),
                        Operator::Addition,
                        Box::new(Expression::Unary(Operator::LogicalNot, int(1))),
                    ),
                    Operator::Subtraction,
                    int(1),
                ),
                Operator::Equality,
                bin(int(5), Operator::Addition, int(1)),
            )),
            Statement::Expression(Expression::Binary(
                bin(
                    bin(
                        bin(int(5), Operator::Multiplication, int(5)),
                        Operator::Division,
                        int(5),
                    ),
                    Operator::GreaterThanOrEqual,
                    int(1),
                ),
                Operator::LessThanOrEqual,
                bin(int(5), Operator::Addition, int(5)),
            )),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_parens_expressions() -> Result<(), Box<dyn Error>> {
        let expression = vec![
            "(1 + 1) * 3",
            "1 == (1 + 1 * (5/2))",
            "(1 + 2) * (3 + 4) - -(5 / (6 - 1))",
        ];

        let expected_results = vec![
            Statement::Expression(Expression::Binary(
                bin(int(1), Operator::Addition, int(1)),
                Operator::Multiplication,
                int(3),
            )),
            Statement::Expression(Expression::Binary(
                int(1),
                Operator::Equality,
                bin(
                    int(1),
                    Operator::Addition,
                    bin(
                        int(1),
                        Operator::Multiplication,
                        bin(int(5), Operator::Division, int(2)),
                    ),
                ),
            )),
            Statement::Expression(Expression::Binary(
                bin(
                    bin(int(1), Operator::Addition, int(2)),
                    Operator::Multiplication,
                    bin(int(3), Operator::Addition, int(4)),
                ),
                Operator::Subtraction,
                Box::new(Expression::Unary(
                    Operator::Negation,
                    bin(
                        int(5),
                        Operator::Division,
                        bin(int(6), Operator::Subtraction, int(1)),
                    ),
                )),
            )),
        ];

        match_statements(expression, expected_results)?;

        let l = Lexer::new("(()");
        let mut parser = Parser::new(l);
        let err = parser.parse_ast();
        assert!(err.is_err());

        Ok(())
    }

    #[test]
    fn parsing_bool_and_null_expressions() -> Result<(), Box<dyn Error>> {
        let expression = vec!["true", "false", "null", "true == false", "null != false"];
        let expected_results = vec![
            Statement::Expression(Expression::Primary(Primary::True)),
            Statement::Expression(Expression::Primary(Primary::False)),
            Statement::Expression(Expression::Primary(Primary::Null)),
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::True)),
                Operator::Equality,
                Box::new(Expression::Primary(Primary::False)),
            )),
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::Null)),
                Operator::Inequality,
                Box::new(Expression::Primary(Primary::False)),
            )),
        ];

        match_statements(expression, expected_results)
    }

    #[test]
    fn parsing_string_expressions() -> Result<(), Box<dyn Error>> {
        let expression = vec!["\"hello\" + \" world\"", "\"foo\" == \"bar\"", "!\"messi\""];
        let expected_results = vec![
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::String("hello".into()))),
                Operator::Addition,
                Box::new(Expression::Primary(Primary::String(" world".into()))),
            )),
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Primary(Primary::String("foo".into()))),
                Operator::Equality,
                Box::new(Expression::Primary(Primary::String("bar".into()))),
            )),
            Statement::Expression(Expression::Unary(
                Operator::LogicalNot,
                Box::new(Expression::Primary(Primary::String("messi".into()))),
            )),
        ];

        match_statements(expression, expected_results)
    }
}
