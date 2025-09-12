use std::{cell::RefCell, collections::HashMap};

use crate::{
    parser::{Primary, Statement},
    parser_error::ParserError,
    perr,
};

#[derive(Debug)]
pub struct Scope<'a> {
    variables: RefCell<HashMap<String, Primary>>,
    prev_scope: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Scope {
            variables: RefCell::new(HashMap::new()),
            prev_scope: None,
        }
    }

    fn set(&self, var: String, exec: Primary) {
        let mut vars = self.variables.borrow_mut();
        vars.insert(var, exec);
    }

    pub fn get(&self, v: &String) -> Primary {
        let vars = self.variables.borrow();
        if let Some(p) = vars.get(v) {
            p.clone()
        } else {
            if let Some(_s) = self.prev_scope {
                _s.get(v)
            } else {
                Primary::Null
            }
        }
    }

    fn from_scope(scope: &'a Scope) -> Self {
        Scope {
            variables: scope.variables.clone(),
            prev_scope: Some(scope),
        }
    }
}

pub fn execute_statement(statement: Statement, scope: &Scope) -> Result<Primary, ParserError> {
    match statement {
        Statement::Expression(expr) => expr.exec(&scope),
        Statement::Let(var, expr) => {
            let value = expr.exec(&scope)?;
            scope.set(var, value.clone());

            Ok(Primary::Null)
        }
        Statement::If(conditional, stataments) => {
            let value = conditional.exec(&scope)?;

            let _scope = Scope::from_scope(scope);
            if let Primary::True = value {
                for i in stataments {
                    execute_statement(i, &_scope)?;
                }
            }

            Ok(Primary::Null)
        }
        _ => perr!(syntax "Unsupported statement")?,
    }
}
