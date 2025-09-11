// use std::mem::discriminant;

use crate::parser::Operator;
use crate::parser::Primary;
use crate::parser_error::ParserError;
use crate::perr;

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
                if let Operator::And = operator {
                    let left_resolved = a.exec()?;
                    if let Primary::Null | Primary::False = left_resolved {
                        return Ok(left_resolved);
                    }
                    return Ok(b.exec()?);
                }

                if let Operator::Or = operator {
                    let left_resolved = a.exec()?;
                    if let Primary::Null | Primary::False = left_resolved {
                        return Ok(b.exec()?);
                    } else {
                        return Ok(left_resolved);
                    }
                }

                let left_resolved = a.exec()?;
                let right_resolved = b.exec()?;

                // if discriminant(&left_resolved) != discriminant(&right_resolved) {
                //     return perr!(syntax "Cannot operate between different types");
                // }

                match (left_resolved, right_resolved) {
                    (Primary::Integer(x), Primary::Integer(y)) => {
                        operation_between_integers(&x, &operator, &y)?
                    }
                    (Primary::True, Primary::True) | (Primary::False, Primary::False) => {
                        match operator {
                            Operator::Equality => Primary::True,
                            Operator::Inequality => Primary::False,
                            _ => return perr!(syntax "Invalid binary operation"),
                        }
                    }
                    (Primary::True, Primary::False) | (Primary::False, Primary::True) => {
                        match operator {
                            Operator::Equality => Primary::False,
                            Operator::Inequality => Primary::True,
                            _ => return perr!(syntax "Invalid binary operation"),
                        }
                    }
                    (Primary::String(s1), Primary::String(s2)) => match operator {
                        Operator::Equality => {
                            if s1 == s2 {
                                Primary::True
                            } else {
                                Primary::False
                            }
                        }
                        Operator::Inequality => {
                            if s1 != s2 {
                                Primary::True
                            } else {
                                Primary::False
                            }
                        }
                        Operator::Addition => Primary::String(format!("{}{}", s1, s2)),
                        _ => return perr!(syntax "Invalid binary operation"),
                    },
                    (Primary::Null, Primary::Null) => match operator {
                        Operator::Equality => Primary::True,
                        Operator::Inequality => Primary::False,
                        _ => return perr!(syntax "Invalid binary operation"),
                    },
                    (_, _) => {
                        if operator == &Operator::Equality {
                            Primary::False
                        } else if operator == &Operator::Inequality {
                            Primary::True
                        } else {
                            perr!(syntax "Invalid binary operation")?
                        }
                    }
                }
            }
            Expression::Unary(operator, expr) => {
                let resolved = expr.exec()?;
                match (operator, resolved) {
                    (Operator::Negation, Primary::Integer(x)) => Primary::Integer(-x),
                    (Operator::LogicalNot, Primary::True) => Primary::False,
                    (Operator::LogicalNot, Primary::False) => Primary::True,
                    (Operator::LogicalNot, Primary::Null) => Primary::True,
                    (Operator::LogicalNot, Primary::String(s)) => {
                        if s.is_empty() {
                            Primary::True
                        } else {
                            Primary::False
                        }
                    }
                    (Operator::LogicalNot, Primary::Integer(i)) => {
                        if i == 0 {
                            Primary::True
                        } else {
                            Primary::False
                        }
                    }
                    _ => return perr!(syntax "Invalid unary operation"),
                }
            }
            Expression::Primary(p) => p.clone(),
        };

        Ok(res)
    }
}

fn operation_between_integers(
    left: &i64,
    operator: &Operator,
    right: &i64,
) -> Result<Primary, ParserError> {
    let a = match operator {
        Operator::Addition => Primary::Integer(left + right),
        Operator::Subtraction => Primary::Integer(left - right),
        Operator::Multiplication => Primary::Integer(left * right),
        Operator::Division => Primary::Integer(left / right),
        Operator::Equality => {
            if left == right {
                Primary::True
            } else {
                Primary::False
            }
        }
        Operator::Inequality => {
            if left != right {
                Primary::True
            } else {
                Primary::False
            }
        }
        Operator::GreaterThan => {
            if left > right {
                Primary::True
            } else {
                Primary::False
            }
        }
        Operator::LessThan => {
            if left < right {
                Primary::True
            } else {
                Primary::False
            }
        }
        Operator::GreaterThanOrEqual => {
            if left >= right {
                Primary::True
            } else {
                Primary::False
            }
        }
        Operator::LessThanOrEqual => {
            if left <= right {
                Primary::True
            } else {
                Primary::False
            }
        }
        _ => perr!(syntax "invalid operation between integers")?,
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

    #[test]
    fn test_unary_operations() {
        let expressions = vec![
            "-5",
            "--9",
            "!true",
            "!false",
            "!0",
            "!1",
            r#"!""#,
            r#"! "hello" "#,
        ];
        let expected_results = vec![
            Primary::Integer(-5),
            Primary::Integer(9),
            Primary::False,
            Primary::True,
            Primary::True,
            Primary::False,
            Primary::True,
            Primary::False,
        ];
        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_true_comparison_operations() {
        let expressions = vec![
            "5 == 5",
            "5 != 3",
            "4 > 2",
            "2 < 4",
            "3 >= 3",
            "2 <= 3",
            r#""hello" == "hello""#,
            r#""hello" != "world""#,
            "true == true",
            "false != true",
            "null == null",
            "null != 5",
            r#""test" != null"#,
            "10 != null",
        ];

        let expected_results = vec![Primary::True; expressions.len()];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_false_comparison_operations() {
        let expressions = vec![
            "5 == 3",
            "5 != 5",
            "2 > 4",
            "4 < 2",
            "3 >= 5",
            "4 <= 2",
            r#""hello" == "world""#,
            r#""hello" != "hello""#,
            "true == false",
            "false != false",
            "null == 5",
            "null != null",
            r#""test" == null"#,
            "10 == null",
        ];

        let expected_results = vec![Primary::False; expressions.len()];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_string_concatenation() {
        let expressions = vec![r#""Hello " + "world""#, r#""Foo" + "Bar""#];
        let expected_results = vec![
            Primary::String("Hello world".to_string()),
            Primary::String("FooBar".to_string()),
        ];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_mixed_type_comparisons() {
        let expressions = vec![
            r#"5 == "5""#,
            r#"true == 1"#,
            r#"false == 0"#,
            r#"null == false"#,
            r#""hello" == true"#,
        ];
        let expected_results = vec![Primary::False; expressions.len()];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_mixed_type_operations() {
        let expressions = vec![
            r#"5 + "5""#,
            r#"true + 1"#,
            r#"false + 0"#,
            r#"null + false"#,
            r#""hello" + true"#,
            r#""hello" * true"#,
            r#"5 * "2""#,
            r#"null * 5"#,
            r#"null + 5"#,
        ];

        for (_, expr) in expressions.iter().enumerate() {
            let l = Lexer::new_from_str(expr);
            let mut parser = Parser::new(l);
            parser.parse_ast().unwrap();

            assert_eq!(parser.statements.len(), 1);
            println!("{:#?}", parser.statements);

            match &parser.statements[0] {
                Statement::Expression(e) => {
                    assert!(e.exec().is_err())
                }
                _ => {}
            };
        }
    }

    #[test]
    fn test_conditional_operations_with_booleans() {
        let expressions = vec![
            "true || true",
            "false || false",
            "false || true",
            "true || false",
            "false && false",
            "false && true",
            "true && false",
            "true && true",
        ];

        let expected_results = vec![
            Primary::True,
            Primary::False,
            Primary::True,
            Primary::True,
            Primary::False,
            Primary::False,
            Primary::False,
            Primary::True,
        ];

        match_statements(expressions, expected_results).unwrap();
    }

    #[test]
    fn test_conditional_operations_with_others() {
        let expressions = vec![
            "true || 5",
            "false || 8",
            "true || 0",
            "false || 0",
            "5 && 9",
            "0 && 9",
        ];

        let expected_results = vec![
            Primary::True,
            Primary::Integer(8),
            Primary::True,
            Primary::Integer(0),
            Primary::Integer(9),
            Primary::Integer(9),
        ];

        match_statements(expressions, expected_results).unwrap();
    }
}
