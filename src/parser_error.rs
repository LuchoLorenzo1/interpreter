use crate::lexer::Token;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, String),
    MissingToken(Token, String),
    InvalidSyntax(String),
}

impl ParserError {
    pub fn unexpected(token: Token, msg: impl Into<String>) -> Self {
        ParserError::UnexpectedToken(token, msg.into())
    }

    pub fn missing(token: Token, msg: impl Into<String>) -> Self {
        ParserError::MissingToken(token, msg.into())
    }

    pub fn syntax(msg: impl Into<String>) -> Self {
        ParserError::InvalidSyntax(msg.into())
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(t, msg) => {
                write!(f, "Unexpected token {t:?}")?;
                if !msg.is_empty() {
                    write!(f, ": {msg}")?;
                }
                Ok(())
            }
            ParserError::MissingToken(t, msg) => {
                write!(f, "Missing token {t:?}")?;
                if !msg.is_empty() {
                    write!(f, ": {msg}")?;
                }
                Ok(())
            }
            ParserError::InvalidSyntax(msg) => {
                if msg.is_empty() {
                    write!(f, "Syntax error")
                } else {
                    write!(f, "Syntax error: {msg}")
                }
            }
        }
    }
}

impl std::error::Error for ParserError {}

#[macro_export]
macro_rules! perr {
    (syntax $($arg:tt)*) => {
        Err(ParserError::syntax(format!($($arg)*)))
    };
    (unexpected $tok:expr, $($arg:tt)*) => {
        Err(ParserError::unexpected($tok, format!($($arg)*)))
    };
    (missing $tok:expr, $($arg:tt)*) => {
        Err(ParserError::missing($tok, format!($($arg)*)))
    };
}
