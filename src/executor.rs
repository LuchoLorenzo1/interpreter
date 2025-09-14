use std::{cell::RefCell, collections::HashMap};

use crate::{
    parser::{Primary, Statement},
    parser_error::ParserError,
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

    pub fn set(&self, var: &String, new_p: Primary) -> Option<()> {
        let mut vars = self.variables.borrow_mut();

        if let Some(p) = vars.get_mut(var) {
            *p = new_p;
            return Some(());
        }

        if let Some(prev_scope) = self.prev_scope {
            prev_scope.set(var, new_p);
            return Some(());
        }

        None
    }

    pub fn define(&self, var: String, exec: Primary) {
        let mut vars = self.variables.borrow_mut();
        vars.insert(var, exec);
    }

    pub fn get(&self, v: &String) -> Primary {
        let vars = self.variables.borrow();
        if let Some(p) = vars.get(v) {
            p.clone()
        } else {
            if let Some(prev_scope) = self.prev_scope {
                prev_scope.get(v)
            } else {
                Primary::Null
            }
        }
    }

    fn from_scope(scope: &'a Scope) -> Self {
        Scope {
            variables: RefCell::new(HashMap::new()),
            prev_scope: Some(scope),
        }
    }
}

pub fn execute(statements: Vec<Statement>) -> Result<Primary, ParserError> {
    let scope = Scope::new();

    for statement in statements {
        if let (p, true) = execute_statement(&statement, &scope)? {
            return Ok(p);
        }
    }

    Ok(Primary::Null)
}

/// Returns (Primary, bool) where bool indicates if it's a return statement
pub fn execute_statement(
    statement: &Statement,
    scope: &Scope,
) -> Result<(Primary, bool), ParserError> {
    match statement {
        Statement::Expression(expr) => {
            let p = expr.exec(&scope)?;
            Ok((p, false))
        }
        Statement::Let(var, expr) => {
            let value = expr.exec(&scope)?;
            scope.define(var.clone(), value.clone());
            Ok((Primary::Null, false))
        }
        Statement::Scope(statements) => {
            let _scope = Scope::from_scope(scope);
            for s in statements {
                if let (p, true) = execute_statement(&s, &_scope)? {
                    return Ok((p, true));
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::If(conditional, statements) => {
            let value = conditional.exec(&scope)?;
            let _scope = Scope::from_scope(scope);
            if let Primary::True = value {
                for s in statements {
                    if let (p, true) = execute_statement(&s, &_scope)? {
                        return Ok((p, true));
                    }
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::While(conditional, statements) => {
            while let Primary::True = conditional.exec(&scope)? {
                let _scope = Scope::from_scope(scope);
                for s in statements {
                    if let (p, true) = execute_statement(&s, &_scope)? {
                        return Ok((p, true));
                    }
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::Function(_params, _statements) => Ok((Primary::Null, false)),
        Statement::Return(expr) => Ok((expr.exec(&scope)?, true)),
    }
}
