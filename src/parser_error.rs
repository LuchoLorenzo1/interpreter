#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(String),
    MissingToken(String),
    InvalidSyntax(String),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(msg) => write!(f, "Unexpected token: {}", msg),
            ParserError::MissingToken(msg) => write!(f, "Missing expected token: {}", msg),
            ParserError::InvalidSyntax(msg) => write!(f, "Syntax error: {}", msg),
        }
    }
}

impl std::error::Error for ParserError {}
