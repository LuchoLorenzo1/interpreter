use crate::parser::Operator;
use crate::parser::Primary;
use crate::parser::Primary::*;
use crate::parser_error::ParserError;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Primary(Primary),
}

impl Expression {
    pub fn exec(&self) -> Result<Primary, ParserError> {
        let res = match self {
            Expression::Binary(a, operator, b) => {
                let left_resolved = a.exec()?;
                let right_resolved = b.exec()?;

                if let (Integer(x), Integer(y)) = (left_resolved, right_resolved) {
                    operation_between_integers(&x, &operator, &y)?
                } else {
                    Primary::Null
                }
            }
            Expression::Primary(p) => p.clone(),
            _ => Null,
        };

        Ok(res)
    }
}

fn operation_between_integers(
    left: &u32,
    operator: &Operator,
    right: &u32,
) -> Result<Primary, ParserError> {
    let a = match operator {
        Operator::Addition => Integer(left + right),
        Operator::Subtraction => Integer(left - right),
        Operator::Multiplication => Integer(left * right),
        Operator::Division => Integer(left / right),
        Operator::Equality => {
            if left == right {
                True
            } else {
                False
            }
        }
        Operator::Inequality => {
            if left != right {
                True
            } else {
                False
            }
        }
        Operator::GreaterThan => {
            if left > right {
                True
            } else {
                False
            }
        }
        Operator::LessThan => {
            if left < right {
                True
            } else {
                False
            }
        }
        Operator::GreaterThanOrEqual => {
            if left >= right {
                True
            } else {
                False
            }
        }
        Operator::LessThanOrEqual => {
            if left <= right {
                True
            } else {
                False
            }
        }
        Operator::Negation => todo!(),
        Operator::LogicalNot => todo!(),
    };

    Ok(a)
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        lexer::Lexer,
        parser::{Parser, Statement},
    };

    use super::*;

    fn match_statements(
        expressions: Vec<&str>,
        expected_results: Vec<Primary>,
    ) -> Result<(), Box<dyn Error>> {
        for (i, expr) in expressions.iter().enumerate() {
            let l = Lexer::new_from_str(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast()?;

            assert_eq!(parser.statements.len(), 1);
            println!("{:#?}", parser.statements);

            let result = match &parser.statements[0] {
                Statement::Expression(e) => e.exec()?,
                _ => Primary::Null,
            };

            assert_eq!(result, expected_results[i]);
        }

        Ok(())
    }

    #[test]
    fn test_basic_arithmetic() {
        let expressions = vec!["1 + 2", "5 - 3", "4 * 2", "8 / 4"];
        let expected_results = vec![
            Primary::Integer(3),
            Primary::Integer(2),
            Primary::Integer(8),
            Primary::Integer(2),
        ];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_multiple_arithmetic_expressions() {
        let expressions = vec!["1 + 2 * 3", "10 + 2 - 3", "2 * 3 * 4", "100 / 5 / 2"];
        let expected_results = vec![
            Primary::Integer(7),
            Primary::Integer(9),
            Primary::Integer(24),
            Primary::Integer(10),
        ];
        match_statements(expressions, expected_results).unwrap();
    }
}
