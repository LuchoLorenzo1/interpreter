use std::io::{self, Write};

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{Clear, ClearType, disable_raw_mode, enable_raw_mode};

use crate::executor::{Scope, execute_statement};

pub fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut history: Vec<String> = vec![];
    let mut history_index: Option<usize> = None;

    let scope = Scope::new();

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    let mut stderr = io::stderr();

    loop {
        write!(stdout, "\r")?;
        write!(stderr, "\r")?;
        print!(">> ");
        stdout.flush()?;

        let mut line = String::new();
        loop {
            if let Event::Key(KeyEvent {
                code, modifiers, ..
            }) = event::read()?
            {
                match (code, modifiers) {
                    (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                        print!("\n\rexit");
                        disable_raw_mode()?;
                        return Ok(());
                    }
                    (KeyCode::Char('d'), KeyModifiers::CONTROL) => {
                        print!("\r\nexit");
                        write!(stdout, "\r")?;
                        disable_raw_mode()?;

                        return Ok(());
                    }
                    (KeyCode::Enter, _) => {
                        break;
                    }
                    (KeyCode::Up, _) => {
                        if !history.is_empty() {
                            let idx = history_index.unwrap_or(history.len());
                            if idx > 0 {
                                history_index = Some(idx - 1);
                                line = history[history_index.unwrap()].clone();

                                execute!(
                                    stdout,
                                    crossterm::cursor::MoveToColumn(0),
                                    Clear(ClearType::CurrentLine),
                                )?;
                                write!(stdout, ">> {}", line)?;
                                stdout.flush()?;
                            }
                        }
                    }
                    (KeyCode::Down, _) => {
                        if let Some(idx) = history_index {
                            if idx + 1 < history.len() {
                                history_index = Some(idx + 1);
                                line = history[idx + 1].clone();
                            } else {
                                history_index = None;
                                line.clear();
                            }

                            execute!(
                                stdout,
                                crossterm::cursor::MoveToColumn(0),
                                Clear(ClearType::CurrentLine),
                            )?;
                            write!(stdout, ">> {}", line)?;
                            stdout.flush()?;
                        }
                    }
                    (KeyCode::Left, _) => {}
                    (KeyCode::Right, _) => {}
                    (KeyCode::Char(c), _) => {
                        line.push(c);
                        write!(stdout, "{}", c)?;
                        stdout.flush()?;
                    }
                    (KeyCode::Backspace, _) => {
                        if !line.is_empty() {
                            line.pop();
                            execute!(
                                stdout,
                                crossterm::cursor::MoveToColumn(0),
                                crossterm::terminal::Clear(
                                    crossterm::terminal::ClearType::CurrentLine
                                ),
                            )?;
                            write!(stdout, ">> {}", line)?;
                            stdout.flush()?;
                        }
                    }
                    _ => {}
                }
            }
        }

        write!(stdout, "\r\n")?;
        stdout.flush()?;

        history_index = None;

        if !line.trim().is_empty() {
            history.push(line.clone());
        }

        let line = line.trim_end();
        if line == "exit" {
            break;
        }

        if line == "clear" {
            execute!(
                stdout,
                Clear(ClearType::All),
                crossterm::cursor::MoveTo(0, 0)
            )?;
            continue;
        }

        let lexer = crate::lexer::Lexer::new_from_str(line);
        let mut parser = crate::parser::Parser::new(lexer);

        if let Err(e) = parser.parse_ast() {
            println!("Parse error: {}", e);
            continue;
        }

        disable_raw_mode()?;
        for s in parser.statements.iter() {
            let (p, b) = execute_statement(s, scope.clone())?;
            println!("{:?}", p);
            if b {
                return Ok(());
            }
        }
        enable_raw_mode()?;
    }

    disable_raw_mode()?;

    Ok(())
}
