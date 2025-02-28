#![allow(dead_code, unused_variables)]

#[derive(PartialEq, Debug)]
enum Keyword {
    Let,
    Fn,
    Return,
}

#[derive(PartialEq, Debug)]
enum Token {
    Identifier(String),
    Keyword(Keyword),
    Integer(usize),
    Illegal(char),
    EqualSign,
    PlusSign,
    MinusSign,
    Comma,
    Semicolon,
    EOF,
}

struct Lexer {
    string: String,
    curr_index: usize,
    curr_reading: usize,
}

impl Lexer {
    fn new(string: String) -> Lexer {
        return Lexer {
            string,
            curr_index: 0,
            curr_reading: 0,
        };
    }

    fn next_token(&mut self) -> Token {
        if self.curr_index >= self.string.len() {
            return Token::EOF;
        }

        let mut chars = self.string.chars();
        let char = chars.nth(self.curr_index).unwrap();

        self.curr_index += 1;
        match char {
            ',' => Token::Comma,
            '=' => Token::EqualSign,
            ';' => Token::Semicolon,
            '+' => Token::PlusSign,
            '-' => Token::MinusSign,
            ' ' | '\t' => {
                while let Some(i) = chars.next() {
                    match i {
                        ' ' | '\t' => self.curr_index += 1,
                        _ => break,
                    }
                }
                let t = self.next_token();
                t
            }
            '0'..'9' => {
                let mut number: String = String::from(char);
                while let Some(i) = chars.next() {
                    match i {
                        '0'..'9' => {
                            number.push(i);
                            self.curr_index += 1;
                        }
                        _ => break,
                    }
                }
                Token::Integer(usize::from_str_radix(&number, 10).unwrap_or(0))
            }
            'a'..'z' | 'A'..'Z' => {
                let mut word: String = String::from(char);
                while let Some(i) = chars.next() {
                    match i {
                        'a'..'z' | 'A'..'Z' => {
                            word.push(i);
                            self.curr_index += 1;
                        }
                        _ => break,
                    }
                }

                match word.as_str() {
                    "let" => Token::Keyword(Keyword::Let),
                    "return" => Token::Keyword(Keyword::Return),
                    "fn" => Token::Keyword(Keyword::Fn),
                    _ => Token::Identifier(word),
                }
            }
            _ => Token::Illegal(char),
        }
    }
}

fn main() {
    let s = String::from("let a = 0;");
    let mut lexer = Lexer::new(s);
    let token: Token = lexer.next_token();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexing_signs() {
        let s = String::from("+ = ; , -");
        let mut l = Lexer::new(s);
        assert_eq!(l.next_token(), Token::PlusSign);
        assert_eq!(l.next_token(), Token::EqualSign);
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::Comma);
        assert_eq!(l.next_token(), Token::MinusSign);
        assert_eq!(l.next_token(), Token::EOF);
    }

    #[test]
    fn lexing_numbers() {
        let s = String::from("1 2; 3; 44,,558");
        let mut l = Lexer::new(s);
        assert_eq!(l.next_token(), Token::Integer(1));
        assert_eq!(l.next_token(), Token::Integer(2));
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::Integer(3));
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::Integer(44));
        assert_eq!(l.next_token(), Token::Comma);
        assert_eq!(l.next_token(), Token::Comma);
        assert_eq!(l.next_token(), Token::Integer(558));
        assert_eq!(l.next_token(), Token::EOF);
    }

    #[test]
    fn multiple_whitespaces() {
        let s = String::from("1 \t\t  2   \t\t  55");
        let mut l = Lexer::new(s);
        assert_eq!(l.next_token(), Token::Integer(1));
        assert_eq!(l.next_token(), Token::Integer(2));
        assert_eq!(l.next_token(), Token::Integer(55));
        assert_eq!(l.next_token(), Token::EOF);
    }

    #[test]
    fn lexing_basic_statement() {
        let s = String::from("let a = 71-1+1;");
        let mut l = Lexer::new(s);
        assert_eq!(l.next_token(), Token::Keyword(Keyword::Let));
        assert_eq!(l.next_token(), Token::Identifier(String::from("a")));
        assert_eq!(l.next_token(), Token::EqualSign);
        assert_eq!(l.next_token(), Token::Integer(71));
        assert_eq!(l.next_token(), Token::MinusSign);
        assert_eq!(l.next_token(), Token::Integer(1));
        assert_eq!(l.next_token(), Token::PlusSign);
        assert_eq!(l.next_token(), Token::Integer(1));
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::EOF);
        assert_eq!(l.next_token(), Token::EOF);
    }

    #[test]
    fn test_illegal_chars() {
        let s = String::from("1?2!3$");
        let mut l = Lexer::new(s);
        assert_eq!(l.next_token(), Token::Integer(1));
        assert_eq!(l.next_token(), Token::Illegal('?'));
        assert_eq!(l.next_token(), Token::Integer(2));
        assert_eq!(l.next_token(), Token::Illegal('!'));
        assert_eq!(l.next_token(), Token::Integer(3));
        assert_eq!(l.next_token(), Token::Illegal('$'));
        assert_eq!(l.next_token(), Token::EOF);
        assert_eq!(l.next_token(), Token::EOF);
        assert_eq!(l.next_token(), Token::EOF);
    }
}
