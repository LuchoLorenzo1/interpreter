use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    parser::{Primary, Statement},
    parser_error::ParserError,
};

#[derive(Debug)]
pub struct Scope {
    variables: RefCell<HashMap<String, Record>>,
    prev_scope: Option<Rc<Scope>>,
}

#[derive(Debug)]
pub enum Record {
    Primary(Primary),
    Function(Rc<Vec<String>>, Rc<Vec<Statement>>),
}

impl Scope {
    pub fn new() -> Rc<Self> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            prev_scope: None,
        })
    }

    pub fn from_scope(parent: Rc<Scope>) -> Rc<Self> {
        Rc::new(Scope {
            variables: RefCell::new(HashMap::new()),
            prev_scope: Some(parent),
        })
    }

    pub fn set(&self, var: &String, new_p: Primary) -> Option<()> {
        let mut vars = self.variables.borrow_mut();

        if let Some(p) = vars.get_mut(var) {
            *p = Record::Primary(new_p);
            return Some(());
        }

        if let Some(ref parent) = self.prev_scope {
            parent.set(var, new_p);
            return Some(());
        }

        None
    }

    pub fn define(&self, var: String, exec: Primary) {
        self.variables
            .borrow_mut()
            .insert(var, Record::Primary(exec));
    }

    pub fn get(&self, v: &String) -> Primary {
        if let Some(Record::Primary(p)) = self.variables.borrow().get(v) {
            p.clone()
        } else if let Some(ref parent) = self.prev_scope {
            parent.get(v)
        } else {
            Primary::Null
        }
    }

    pub fn define_fn(&self, name: String, params: Rc<Vec<String>>, statements: Rc<Vec<Statement>>) {
        self.variables
            .borrow_mut()
            .insert(name, Record::Function(params, statements));
    }

    pub fn get_fn(&self, name: &String) -> Option<(Rc<Vec<String>>, Rc<Vec<Statement>>)> {
        if let Some(Record::Function(p, statements)) = self.variables.borrow().get(name) {
            Some((p.clone(), statements.clone()))
        } else if let Some(ref parent) = self.prev_scope {
            parent.get_fn(name)
        } else {
            None
        }
    }
}

pub fn execute(statements: Vec<Statement>) -> Result<Primary, ParserError> {
    let scope = Scope::new();

    for statement in statements.iter() {
        if let (p, true) = execute_statement(statement, scope.clone())? {
            return Ok(p);
        }
    }

    Ok(Primary::Null)
}

pub fn execute_statement(
    statement: &Statement,
    scope: Rc<Scope>,
) -> Result<(Primary, bool), ParserError> {
    match statement {
        Statement::Expression(expr) => {
            let p = expr.exec(scope.clone())?;
            Ok((p, false))
        }
        Statement::Let(var, expr) => {
            let value = expr.exec(scope.clone())?;
            scope.define(var.clone(), value.clone());
            Ok((Primary::Null, false))
        }
        Statement::Scope(statements) => {
            let child = Scope::from_scope(scope.clone());
            for s in statements.iter() {
                if let (p, true) = execute_statement(s, child.clone())? {
                    return Ok((p, true));
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::If(conditional, statements) => {
            let value = conditional.exec(scope.clone())?;
            if let Primary::True = value {
                let child = Scope::from_scope(scope.clone());
                for s in statements.iter() {
                    if let (p, true) = execute_statement(s, child.clone())? {
                        return Ok((p, true));
                    }
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::While(conditional, statements) => {
            while let Primary::True = conditional.exec(scope.clone())? {
                let child = Scope::from_scope(scope.clone());
                for s in statements.iter() {
                    if let (p, true) = execute_statement(s, child.clone())? {
                        return Ok((p, true));
                    }
                }
            }
            Ok((Primary::Null, false))
        }
        Statement::Function(name, params, statements) => {
            scope.define_fn(name.clone(), params.clone(), statements.clone());
            Ok((Primary::Null, false))
        }
        Statement::Return(expr) => Ok((expr.exec(scope.clone())?, true)),
    }
}
