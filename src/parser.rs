use std::iter::Peekable;

use crate::{
    expression::Expression,
    lexer::{Keyword, Lexer, Token},
    parser_error::ParserError,
    perr,
};

pub struct Parser<I: Iterator<Item = char>> {
    pub lexer: Peekable<Lexer<I>>,
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
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
    Scope(Vec<Statement>),
    If(Expression, Vec<Statement>),
    While(Expression, Vec<Statement>),
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

    // Operators for Conditionals
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primary {
    Integer(i64),
    String(String),
    // Variable(String),
    Null,
    False,
    True,
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(lexer: Lexer<I>) -> Self {
        Parser {
            lexer: lexer.peekable(),
            statements: vec![],
        }
    }

    pub fn parse_ast(&mut self) -> Result<(), ParserError> {
        if self.lexer.peek().is_none() {
            return Ok(());
        }

        while let Some(t) = self.lexer.peek().cloned() {
            let statement = self.parse_next_statement(t)?;

            self.statements.push(statement);

            match self.lexer.next() {
                Some(Token::NewLine) | None => {}
                Some(other) => perr!(unexpected other, "Expecting end of statement.")?,
            }

            while let Some(Token::NewLine) = self.lexer.peek() {
                self.lexer.next();
            }
        }

        Ok(())
    }

    fn parse_next_statement(&mut self, t: Token) -> Result<Statement, ParserError> {
        Ok(match t {
            Token::Keyword(Keyword::Let) => self.parse_let_statement()?,
            Token::OpenBrace => self.parse_scope()?,
            Token::Keyword(Keyword::If) => {
                self.lexer.next();
                let condition = self.expression()?;
                let scope = self.parse_scope()?;
                if let Statement::Scope(statements) = scope {
                    Statement::If(condition, statements)
                } else {
                    perr!(syntax "Expected a scope in if statement.")?
                }
            }
            Token::Keyword(Keyword::While) => {
                self.lexer.next();
                let condition = self.expression()?;
                let scope = self.parse_scope()?;
                if let Statement::Scope(statements) = scope {
                    Statement::While(condition, statements)
                } else {
                    perr!(syntax "Expected a scope in while statement.")?
                }
            }
            Token::Keyword(Keyword::Return) => Statement::Return(self.expression()?),
            _ => Statement::Expression(self.expression()?),
        })
    }

    fn parse_scope(&mut self) -> Result<Statement, ParserError> {
        self.lexer.next();

        let mut statements = vec![];

        if self.lexer.peek().is_none() {
            return Ok(Statement::Scope(statements));
        }

        while let Some(t) = self.lexer.peek().cloned() {
            let statement = self.parse_next_statement(t)?;

            statements.push(statement);

            while let Some(Token::NewLine) = self.lexer.peek() {
                self.lexer.next();
            }

            match self.lexer.peek() {
                Some(Token::CloseBrace) => {
                    self.lexer.next();
                    return Ok(Statement::Scope(statements));
                }
                None => perr!(syntax "Expecting closing bracket")?,
                _ => {}
            }
        }

        Ok(Statement::Scope(vec![]))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.lexer.next();

        let var_name = match self.lexer.next() {
            Some(Token::Identifier(s)) => s,
            Some(k) => perr!(unexpected k, "Expected identifier in let statement.")?,
            None => {
                perr!(syntax "Unexpected end of input. Expecting identifier in let statement.")?
            }
        };

        match self.lexer.next() {
            Some(Token::EqualSign) => {}
            Some(k) => perr!(unexpected k, "Expected `=` after `let {var_name}`.")?,
            None => perr!(syntax "Unexpected end of input. Expecting `=` after `let {var_name}`.")?,
        };

        let expr = self.expression()?;

        Ok(Statement::Let(var_name, expr))
    }

    fn expression(&mut self) -> Result<Expression, ParserError> {
        self.conditional()
    }

    fn conditional(&mut self) -> Result<Expression, ParserError> {
        let mut equality_left = self.equality()?;

        while let Some(tok) = self.lexer.peek() {
            let op = match tok {
                Token::DoublePipe => Some(Operator::Or),
                Token::DoubleAmpersand => Some(Operator::And),
                _ => break,
            };

            if let Some(operator) = op {
                self.lexer.next();

                let tmp = Expression::Binary(
                    Box::new(equality_left),
                    operator,
                    Box::new(self.equality()?),
                );

                equality_left = tmp;
            }
        }

        Ok(equality_left)
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
            None => perr!(syntax "Unexpected end of input. Expecting expression.")?,
            Some(Token::NotSign | Token::MinusSign) => return self.expression(),
            Some(Token::OpenParenthesis) => {
                self.lexer.next();
                let right_expr = self.expression()?;

                let next_token = self.lexer.next();
                if let Some(Token::CloseParenthesis) = next_token {
                    return Ok(right_expr);
                } else {
                    return perr!(unexpected next_token.unwrap_or(Token::NewLine), "Expected closing parenthesis.")?;
                }
            }
            Some(_) => {}
        };

        let p = match self.lexer.next().unwrap() {
            Token::Integer(u) => Primary::Integer(u),
            Token::String(s) => Primary::String(s),
            Token::Keyword(Keyword::True) => Primary::True,
            Token::Keyword(Keyword::False) => Primary::False,
            Token::Keyword(Keyword::Null) => Primary::Null,
            Token::Identifier(s) => return Ok(Expression::Variable(s)),
            t => perr!(unexpected t, "Expected primary expression.")?,
        };

        Ok(Expression::Primary(p))
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;

    fn int(i: i64) -> Box<Expression> {
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
            let l = Lexer::new_from_str(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast()?;

            assert_eq!(parser.statements.len(), 1);
            println!("{:#?}", parser.statements);
            assert_eq!(parser.statements[0], expected_results[i]);
        }

        Ok(())
    }

    fn match_programs(
        programs: Vec<&str>,
        expected_results: Vec<Vec<Statement>>,
    ) -> Result<(), Box<dyn Error>> {
        for (i, expr) in programs.iter().enumerate() {
            println!("Parsing program: {}", expr);

            let l = Lexer::new_from_str(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast()?;

            assert_eq!(parser.statements.len(), expected_results[i].len());

            for (j, s) in parser.statements.into_iter().enumerate() {
                assert_eq!(s, expected_results[i][j]);
            }
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

        let l = Lexer::new_from_str("(()");
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

    #[test]
    fn parsing_let_statements() -> Result<(), Box<dyn Error>> {
        let expression = vec!["let x = 5"];
        let expected_results = vec![Statement::Let(
            "x".into(),
            Expression::Primary(Primary::Integer(5)),
        )];
        match_statements(expression, expected_results)?;

        let programs = vec![
            "let name = \"John Doe\"\nlet a=b",
            "let a = !true;;let b = false\n\n\nlet c = null",
        ];
        let expected_results = vec![
            vec![
                Statement::Let(
                    "name".into(),
                    Expression::Primary(Primary::String("John Doe".into())),
                ),
                Statement::Let("a".into(), Expression::Variable("b".into())),
            ],
            vec![
                Statement::Let(
                    "a".into(),
                    Expression::Unary(
                        Operator::LogicalNot,
                        Box::new(Expression::Primary(Primary::True)),
                    ),
                ),
                Statement::Let("b".into(), Expression::Primary(Primary::False)),
                Statement::Let("c".into(), Expression::Primary(Primary::Null)),
            ],
        ];

        match_programs(programs, expected_results)
    }

    #[test]
    fn test_scopes() -> Result<(), Box<dyn Error>> {
        let programs = vec![
            "{ let x = 5\n let y = 10 }",
            "{ let x=5;\n\n\n let y=10;;;\n\n\n }",
            "{ { { 1 + 1; let a = 1;;;} let b = 1 } true == true }",
        ];
        let expected_results = vec![
            vec![Statement::Scope(vec![
                Statement::Let("x".into(), Expression::Primary(Primary::Integer(5))),
                Statement::Let("y".into(), Expression::Primary(Primary::Integer(10))),
            ])],
            vec![Statement::Scope(vec![
                Statement::Let("x".into(), Expression::Primary(Primary::Integer(5))),
                Statement::Let("y".into(), Expression::Primary(Primary::Integer(10))),
            ])],
            vec![Statement::Scope(vec![
                Statement::Scope(vec![
                    Statement::Scope(vec![
                        Statement::Expression(Expression::Binary(
                            int(1),
                            Operator::Addition,
                            int(1),
                        )),
                        Statement::Let("a".into(), Expression::Primary(Primary::Integer(1))),
                    ]),
                    Statement::Let("b".into(), Expression::Primary(Primary::Integer(1))),
                ]),
                Statement::Expression(Expression::Binary(
                    Box::new(Expression::Primary(Primary::True)),
                    Operator::Equality,
                    Box::new(Expression::Primary(Primary::True)),
                )),
            ])],
        ];

        match_programs(programs, expected_results)
    }

    #[test]
    fn scopes_should_fail() {
        let programs = vec![
            "{ let x = 5",
            "let y = 10 }",
            "{ { 1 + 1; let a = 1;",
            "}",
            "}}}",
            "{ \n\n\n\n }\n\n}",
        ];
        for program in programs {
            let l = Lexer::new_from_str(program);
            let mut parser = Parser::new(l);
            let err = parser.parse_ast();
            assert!(err.is_err());
        }
    }

    #[test]
    fn testing_if_and_while_statements() -> Result<(), Box<dyn Error>> {
        let programs = vec![
            "if true { let x = 5 }",
            "if false { let y = 10 }",
            "while true { let a = 1 }",
            "while false { let b = 2 }",
            "if true { if false { let x = 5 } }",
            "while true { while false { let y = 10 } }",
            "if true { while false { let a = 1 } }",
            "while false { if true { let b = 2 } }",
        ];

        let expected_results = vec![
            vec![Statement::If(
                Expression::Primary(Primary::True),
                vec![Statement::Let(
                    "x".into(),
                    Expression::Primary(Primary::Integer(5)),
                )],
            )],
            vec![Statement::If(
                Expression::Primary(Primary::False),
                vec![Statement::Let(
                    "y".into(),
                    Expression::Primary(Primary::Integer(10)),
                )],
            )],
            vec![Statement::While(
                Expression::Primary(Primary::True),
                vec![Statement::Let(
                    "a".into(),
                    Expression::Primary(Primary::Integer(1)),
                )],
            )],
            vec![Statement::While(
                Expression::Primary(Primary::False),
                vec![Statement::Let(
                    "b".into(),
                    Expression::Primary(Primary::Integer(2)),
                )],
            )],
            vec![Statement::If(
                Expression::Primary(Primary::True),
                vec![Statement::If(
                    Expression::Primary(Primary::False),
                    vec![Statement::Let(
                        "x".into(),
                        Expression::Primary(Primary::Integer(5)),
                    )],
                )],
            )],
            vec![Statement::While(
                Expression::Primary(Primary::True),
                vec![Statement::While(
                    Expression::Primary(Primary::False),
                    vec![Statement::Let(
                        "y".into(),
                        Expression::Primary(Primary::Integer(10)),
                    )],
                )],
            )],
            vec![Statement::If(
                Expression::Primary(Primary::True),
                vec![Statement::While(
                    Expression::Primary(Primary::False),
                    vec![Statement::Let(
                        "a".into(),
                        Expression::Primary(Primary::Integer(1)),
                    )],
                )],
            )],
            vec![Statement::While(
                Expression::Primary(Primary::False),
                vec![Statement::If(
                    Expression::Primary(Primary::True),
                    vec![Statement::Let(
                        "b".into(),
                        Expression::Primary(Primary::Integer(2)),
                    )],
                )],
            )],
        ];

        match_programs(programs, expected_results)
    }
}
