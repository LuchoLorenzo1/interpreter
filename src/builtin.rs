use std::rc::Rc;

use crate::{
    executor::Scope, expression::Expression, parser::Primary, parser_error::ParserError, perr,
};

pub fn call_builtin_function(
    name: &str,
    params: &Vec<Box<Expression>>,
    scope: Rc<Scope>,
) -> Result<Primary, ParserError> {
    match name {
        "print" => {
            if params.len() != 1 {
                return perr!(syntax "print only receives one parameter")?;
            }
            let p = params[0].exec(scope.clone())?;
            println!("{}", p);

            Ok(Primary::Null)
        }
        _ => perr!(syntax "function not defined")?,
    }
}
