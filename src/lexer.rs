#![allow(dead_code, unused_variables)]

use std::{iter::Peekable, str::Chars};

#[derive(PartialEq, Debug)]
pub enum Keyword {
    Let,
    Fn,
    Return,
    False,
    True,
    Null,
}

#[derive(PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Integer(u32),
    Illegal(char),
    String(String),
    NewLine,
    EqualSign,
    DoubleEqualSign,
    NotEqualSign,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    PlusSign,
    MinusSign,
    Asterisk,
    Slash,
    Comma,
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Quote,
    NotSign,
}

pub struct Lexer<I: Iterator<Item = char>> {
    chars: Peekable<I>,
}

impl<'a> Lexer<Chars<'a>> {
    pub fn new_from_str(string: &'a str) -> Self {
        return Lexer {
            chars: string.chars().peekable(),
        };
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(i: I) -> Self {
        return Lexer {
            chars: i.peekable(),
        };
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let char = match self.chars.next() {
            Some(c) => c,
            None => return None,
        };

        Some(match char {
            '\n' | ';' => Token::NewLine,
            ',' => Token::Comma,
            '+' => Token::PlusSign,
            '-' => Token::MinusSign,
            '(' => Token::OpenParenthesis,
            ')' => Token::CloseParenthesis,
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '"' => {
                let mut word: String = String::new();
                while let Some(i) = self.chars.peek() {
                    match i {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | ' ' => {
                            word.push(*i);
                            self.chars.next();
                        }
                        '"' => {
                            self.chars.next();
                            break;
                        }
                        _ => break,
                    }
                }
                Token::String(word)
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '!' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::NotEqualSign
                } else {
                    Token::NotSign
                }
            }
            '=' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::DoubleEqualSign
                } else {
                    Token::EqualSign
                }
            }
            '>' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '<' => {
                if let Some('=') = self.chars.peek() {
                    self.chars.next();
                    Token::LessThanOrEqual
                } else {
                    Token::LessThan
                }
            }
            ' ' | '\t' => {
                while let Some(i) = self.chars.peek() {
                    match i {
                        ' ' | '\t' => {
                            self.chars.next();
                        }
                        _ => break,
                    }
                }
                if let None = self.chars.peek() {
                    return None;
                }
                self.next()?
            }
            '0'..='9' => {
                let mut number: String = String::from(char);
                while let Some(i) = self.chars.peek() {
                    match i {
                        '0'..='9' => {
                            number.push(*i);
                            self.chars.next();
                        }
                        _ => break,
                    }
                }
                Token::Integer(u32::from_str_radix(&number, 10).unwrap_or(0))
            }
            'a'..='z' | 'A'..='Z' => {
                let mut word: String = String::from(char);
                while let Some(i) = self.chars.peek() {
                    match i {
                        'a'..='z' | 'A'..='Z' => {
                            word.push(*i);
                            self.chars.next();
                        }
                        _ => break,
                    }
                }

                match word.as_str() {
                    "let" => Token::Keyword(Keyword::Let),
                    "return" => Token::Keyword(Keyword::Return),
                    "fn" => Token::Keyword(Keyword::Fn),
                    "false" => Token::Keyword(Keyword::False),
                    "true" => Token::Keyword(Keyword::True),
                    "null" => Token::Keyword(Keyword::Null),
                    _ => Token::Identifier(word),
                }
            }
            _ => Token::Illegal(char),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_signs() {
        let s = String::from("+ = ; , -");
        let mut l = Lexer::new_from_str(&s);
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::MinusSign);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn lexing_numbers() {
        let s = String::from("1 2; 3; 44,,558");
        let mut l = Lexer::new_from_str(&s);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next().unwrap(), Token::Integer(3));
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next().unwrap(), Token::Integer(44));
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::Comma);
        assert_eq!(l.next().unwrap(), Token::Integer(558));
        assert_eq!(l.next(), None);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn multiple_whitespaces() {
        let s = String::from("1 \t\t  2   \t\t  55");
        let mut l = Lexer::new_from_str(&s);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::Integer(55));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn lexing_basic_statement() {
        let s = String::from("let a = 71-1+1;");
        let mut l = Lexer::new_from_str(&s);
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next().unwrap(), Token::Identifier(String::from("a")));
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(71));
        assert_eq!(l.next().unwrap(), Token::MinusSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_illegal_chars() {
        let s = String::from("1?2%3$");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::Illegal('?'));
        assert_eq!(l.next().unwrap(), Token::Integer(2));
        assert_eq!(l.next().unwrap(), Token::Illegal('%'));
        assert_eq!(l.next().unwrap(), Token::Integer(3));
        assert_eq!(l.next().unwrap(), Token::Illegal('$'));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_basic_string() {
        let s = String::from("\"messi\"");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::String(String::from("messi")));
    }

    #[test]
    fn test_multiple_strings() {
        let s = String::from("\"messi\"+(\"ronaldo\")");
        let mut l = Lexer::new_from_str(&s).into_iter();

        assert_eq!(l.next().unwrap(), Token::String(String::from("messi")));
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::OpenParenthesis);
        assert_eq!(l.next().unwrap(), Token::String(String::from("ronaldo")));
        assert_eq!(l.next().unwrap(), Token::CloseParenthesis);
    }

    #[test]
    fn test_string() {
        let s = String::from("let xyz = \"abcdefghijklmnopqrstuvwxyz\";");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next().unwrap(), Token::Identifier(String::from("xyz")));
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(
            l.next().unwrap(),
            Token::String(String::from("abcdefghijklmnopqrstuvwxyz"))
        );
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_double_equals() {
        let s = String::from("1 == 1;");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::DoubleEqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_unequal() {
        let s = String::from("1 != 1; !===!=");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::NewLine);
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next().unwrap(), Token::DoubleEqualSign);
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next(), None);
    }

    #[test]
    fn test_bool_and_null() {
        let s = String::from("let false = true != false == null + 1 - \"null\"");
        let mut l = Lexer::new_from_str(&s).into_iter();
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::False));
        assert_eq!(l.next().unwrap(), Token::EqualSign);
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::True));
        assert_eq!(l.next().unwrap(), Token::NotEqualSign);
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::False));
        assert_eq!(l.next().unwrap(), Token::DoubleEqualSign);
        assert_eq!(l.next().unwrap(), Token::Keyword(Keyword::Null));
        assert_eq!(l.next().unwrap(), Token::PlusSign);
        assert_eq!(l.next().unwrap(), Token::Integer(1));
        assert_eq!(l.next().unwrap(), Token::MinusSign);
        assert_eq!(l.next().unwrap(), Token::String(String::from("null")));
        assert_eq!(l.next(), None);
    }
}
