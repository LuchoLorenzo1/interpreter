use std::collections::HashMap;

use crate::{
    parser::{Primary, Statement},
    parser_error::ParserError,
    perr,
};

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Primary>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }

    fn set(&mut self, var: String, exec: Primary) {
        self.variables.insert(var, exec);
    }

    pub(crate) fn get(&self, v: &String) -> Primary {
        if let Some(p) = self.variables.get(v) {
            p.clone()
        } else {
            Primary::Null
        }
    }
}

pub fn execute_statement(statement: Statement, scope: &mut Scope) -> Result<Primary, ParserError> {
    match statement {
        Statement::Expression(expr) => expr.exec(&scope),
        Statement::Let(var, expr) => {
            let value = expr.exec(&scope)?;
            scope.set(var, value.clone());
            Ok(Primary::Null)
        }
        _ => perr!(syntax "Unsupported statement")?,
    }
}
