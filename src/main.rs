mod lexer;
mod error;
mod parser;
// mod semantic;
mod analyzer;

use lexer::*;
use parser::*;
use analyzer::*;
use error::*;

fn main() {
    // load source code to compile
    let source_code = {
        let args: Vec<String> = std::env::args().collect();
        let path = match args.get(1) {
            Some(value) => value,
            None => {
                println!("No file specified.");
                return;
            }
        };

        let with_comments = match std::fs::read_to_string(path) {
            Ok(mut value) => {
                value.push('\n');
                value
            },
            Err(_) => {
                println!("Unable to read file.");
                return;
            }
        };
        // with_comments.push('\n');
        
        
        let mut buffer: Vec<char> = Vec::new();

        // remove comments
        let mut in_comment = false;
        let mut in_string = false;
        let char_count = with_comments.chars().count();
        for (i, c) in with_comments[..char_count - 1].chars().enumerate() {
            let wide = &with_comments[i..i + 2];

            // println!("{}", wide);
            
            // ignore "//" in string literals
            if !in_comment {
                // if entering string
                if !in_string {
                    if c == '"' {
                        in_string = true;
                    }
                    // if exiting string
                } else if c == '"' {
                    in_string = false
                }
            }

            if wide == "//" && !in_string {
                in_comment = true;
            }

            if in_comment && !in_string {
                if c == '\n' {
                    in_comment = false;
                    buffer.push('\n');
                }
                continue;
            }

            buffer.push(c);
        }
        
        let mut without_comments = buffer.iter().collect::<String>();
        without_comments.push('\n');
        without_comments.push('\n');
        without_comments
    };
        
    // used for displaying lines where an error occurs
    let source_code_lines = source_code.lines().collect::<Vec<&str>>();

    // println!("{}", source_code);
    let lexemes = match source_code.lex() {
        Ok(lexemes) => lexemes,
        Err(error) => {
            match error.kind {
                LexerError::UnclosedString => println!("error: unclosed string literal"),
                LexerError::MismatchedBrackets => println!("error: mismatched brackets"),
                LexerError::InvalidSymbol { symbol_name } => println!("error: invalid symbol `{}`", symbol_name),
            }
            
            for line in error.lines.iter() {
                println!("   --> main.bling");
                println!("{:>6} | {}", line, source_code_lines.get(line - 1).unwrap());
            }
            println!("");
            return;
        }
    };
    
    // println!("\nLEXER OUTPUT: ");
    // for (i, lexeme) in lexemes.iter().enumerate() {
    //     println!("{}: {:?}", i, lexeme);
    // }

    let mut parser = Parser::new(lexemes.as_slice());
    let mut syntax_tree = match parser.parse_statement() {
        Ok(statement) => statement,
        Err(error) => {
            match error.kind {
                ParserError::SyntaxError {expected, found} => println!("error: expected {}, found {}", expected, found),
            }
            
            for line in error.lines.iter() {
                println!("   --> main.bling");
                println!("{:>6} | {}", line, source_code_lines.get(line - 1).unwrap());
            }
            println!("");
            return;
        }
    };

    let mut analyzer = Analyzer::new();
    match analyzer.analyze(&mut syntax_tree) {
        Ok(()) => {},
        Err(errors) => {
            for error in errors {
                match error.kind {
                    SemanticError::UndeclaredSymbol { symbol_name } => println!("error: undeclared symbol `{}`", symbol_name),
                    SemanticError::RedeclaredSymbol { symbol_name } => println!("error: redeclaration of symbol `{}`", symbol_name),
                    SemanticError::TypeMismatch { expected, found } => println!("error: expected type `{}`, found `{}`", expected, found),
                    SemanticError::InvalidOperation { operation, data_types } => println!("error: cannot perform operation on types `{}` and `{}`", data_types[0].to_string(), data_types[1].to_string()),
                    SemanticError::InvalidArgumentCount { expected, found } => println!("error: expected {} arguments, found {}", expected, found),
                    SemanticError::NotFunction { symbol_name } => println!("error: `{}` is not a function", symbol_name),
                    SemanticError::NotProcedure { symbol_name } => println!("error: `{}` is not a procedure", symbol_name),
                    SemanticError::ConstantReassignment { symbol_name } =>println!("error: cannot reassign `{}` because it is a constant", symbol_name)
                }

                println!("   --> main.bling");
                    
                for line in error.lines.iter() {
                    println!("{:>6} | {}", line, source_code_lines.get(line - 1).unwrap());
                }
                println!("");
            }
            return;
        }
    }

    dbg!(analyzer);

    // println!("\nAFTER SEMANTIC ANALYSIS AND TYPE INFERENCE:");
    // println!("{:#?}", syntax_tree);
        
    // println!("{}", asd);
}
