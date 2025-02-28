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
    Integer(u32),
    Illegal,
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
            return Token::Illegal;
        }

        dbg!(&self.string);
        let char = &self.string.chars().nth(self.curr_index).unwrap();
        let token = match char {
            ',' => Token::Comma,
            '=' => Token::EqualSign,
            ';' => Token::Semicolon,
            '+' => Token::PlusSign,
            '-' => Token::MinusSign,
            ' ' => {
                self.curr_index += 1;
                self.next_token()
            }
            _ => Token::Illegal,
        };
        self.curr_index += 1;
        token
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
        let mut lexer = Lexer::new(s);
        let mut token: Token = lexer.next_token();
        assert_eq!(token, Token::PlusSign);

        token = lexer.next_token();
        assert_eq!(token, Token::EqualSign);

        token = lexer.next_token();
        assert_eq!(token, Token::Semicolon);

        token = lexer.next_token();
        assert_eq!(token, Token::Comma);

        token = lexer.next_token();
        assert_eq!(token, Token::MinusSign);
    }

    #[test]
    fn lexing_basic_text() {
        let s = String::from("let a = 0;");
        let mut lexer = Lexer::new(s);
        let mut token: Token = lexer.next_token();
        assert_eq!(token, Token::Keyword(Keyword::Let));

        token = lexer.next_token();
        assert_eq!(token, Token::Identifier(String::from("a")));
    }
}
