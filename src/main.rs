mod lexer;
mod error;
mod parser;
use lexer::*;
use parser::*;

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
            println!("error: {}", error.message);
            for line in error.lines.iter() {
                println!("   --> main.bling");
                println!("{:>6} | {}", line, source_code_lines.get(line - 1).unwrap());
            }
            return;
        }
    };
    println!("\nLEXER OUTPUT: ");
    for (i, lexeme) in lexemes.iter().enumerate() {
        println!("{}: {:?}", i, lexeme);
    }

    let mut parser = Parser::new(lexemes.as_slice());
    let syntax_tree = match parser.parse_statement() {
        Ok(statement) => statement,
        Err(error) => {
            println!("error: {}", error.message);
            println!("   --> main.bling");
            for line in error.lines.iter() {
                println!("{:>6} | {}", line, source_code_lines.get(line - 1).unwrap());
            }
            return;
        }
    };

    println!("\nPARSER OUTPUT:");
    println!("{:#?}", syntax_tree);
    
    // println!("{}", asd);
}
