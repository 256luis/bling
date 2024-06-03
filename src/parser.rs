use crate::{lexer::*, error::*};

#[derive(Debug)]
pub enum Type {
    Int, Float, String, UserDefined(String),

    Proc {
        param_types: Vec<Type>
    },

    Func {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}
#[derive(Debug)]
pub struct IdentifierAndType {
    identifier: String,
    data_type: Type
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f32),

    Binary {
        operand_1: Box<Expression>,
        operand_2: Box<Expression>,
    },

    ProcedureDefinition {
        param_list: Vec<IdentifierAndType>,
        body: Box<Statement>
    }
}

#[derive(Debug)]
pub enum Statement {
    Compound(Vec<Statement>),

    DeclareConstant {
        identifier: String,
        data_type: Option<Type>,
        value: Expression,
    },

    DeclareVariable {
        identifier: String,
        data_type: Option<Type>,
        value: Option<Expression>,
    }
}

fn parse_type(lexemes: &[Lexeme]) -> Result<Type, Error> {
    println!("parse type\n{:#?}\n", lexemes);
    
    let lexeme_kinds: Vec<LexemeKind> = lexemes.iter().map(|x| x.kind.clone()).collect();
    match lexeme_kinds.as_slice() {
        [LexemeKind::Identifier(type_name)] => {
            match type_name.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "string" => Ok(Type::String),
                _ => Ok(Type::UserDefined(type_name.to_string())),
            }
        },
        
        [LexemeKind::Proc | LexemeKind::Func, ..] => {
            let is_func = lexemes[0].kind == LexemeKind::Func;
            
            // func type grammar
            // 'func' '(' [ TYPE { ',' TYPE } ] ')' '->' TYPE

            // proc type grammar
            // 'proc' '(' [ TYPE { ',' TYPE } ] ')' 

            let mut lexemes_iter = lexemes.iter();            
            let mut prev_lexeme = lexemes_iter.next().unwrap();
            let mut current_lexeme = lexemes_iter.next();

            // '(' --------------------
            match current_lexeme {
                Some(Lexeme{kind: LexemeKind::LeftParen, ..}) => {},
                
                Some(lexeme) => return Err(Error {
                    message: format!("expected `(`, found `{}`", lexeme.kind.to_string()),
                    lines: vec![lexeme.line]
                }),
                
                None => return Err(Error {
                    message: String::from("expected `(`, found nothing"),
                    lines: vec![prev_lexeme.line]
                }),
            }

            // TYPE_LIST ----------------
            prev_lexeme = current_lexeme.unwrap();
            current_lexeme = lexemes_iter.next();

            // get all lexemes between the parens and separate them by comma while still
            // respecting parens
            let mut paren_counter = 1; // 1 because we skipped the first left paren
            let mut type_list_lexemes: Vec<Vec<Lexeme>> = Vec::new();
            let mut buffer: Vec<Lexeme> = Vec::new();
            loop {
                // i think current_lexeme will never be none because the lexer ensures that
                // parens are always correct
                // todo: remove?
                // dbg!(&current_lexeme);
                if current_lexeme.is_none() {
                    return Err(Error {
                        message: String::from("procedure type must take the form `proc(type, ...)`"),
                        lines: vec![prev_lexeme.line],
                    })                    
                }

                let lexeme = current_lexeme.unwrap();
                prev_lexeme = current_lexeme.unwrap();
                current_lexeme = lexemes_iter.next();
                
                if lexeme.kind == LexemeKind::LeftParen {
                    paren_counter += 1;
                } else if lexeme.kind == LexemeKind::RightParen {
                    paren_counter -= 1;
                    if paren_counter == 0 {
                        break;
                    }
                }

                if lexeme.kind == LexemeKind::Comma && paren_counter == 1 {
                    type_list_lexemes.push(buffer.clone());
                    buffer.clear();
                    continue;
                }

                buffer.push(lexeme.clone());
            }

            // for the last type that isnt followed by a comma
            if !buffer.is_empty() {
                type_list_lexemes.push(buffer.clone());
                buffer.clear();
            }                    

            // parse each type in the type list
            let param_types =  {
                let mut result: Vec<Type> = Vec::new();
                for type_lexemes in type_list_lexemes {
                    let param_type = parse_type(&type_lexemes)?;
                    result.push(param_type);
                }

                result
            };

            // dbg!(&param_types);

            if is_func {
                // -> -------------------
                match current_lexeme {
                    Some(Lexeme{kind: LexemeKind::Arrow, ..}) => {},
                    Some(lexeme) => return Err(Error {
                        message: format!("expected `->`, found `{}`", lexeme.kind.to_string()),
                        lines: vec![lexeme.line]
                    }),
                    None => return Err(Error {
                        message: String::from("expected `->`, found nothing"),
                        lines: vec![prev_lexeme.line]
                    })
                }

                // return type -----------------                
                let return_type = {
                    let remaining: Vec<_> = lexemes_iter.cloned().collect();
                    Box::new(parse_type(remaining.as_slice())?)
                };

                Ok(Type::Func {
                    param_types,
                    return_type
                })
            } else { 
                Ok(Type::Proc {
                    param_types
                })                    
            }
        },

        _ => Err(Error {
            message: format!("expected type_name, found `{}`", lexemes[0].kind.to_string()),
            lines: lexemes.iter().map(|x| x.line).collect()
        })
    }
    
    //todo!()
}

fn parse_expression(lexemes: &[Lexeme]) -> Result<Expression, Error> {
    println!("parse expression\n{:#?}\n", lexemes);

    let lexeme_kinds: Vec<LexemeKind> = lexemes.iter().map(|x| x.kind.clone()).collect();
    match lexeme_kinds.as_slice() {
        [LexemeKind::Identifier(identifier)] => Ok(Expression::Identifier(identifier.to_owned())),
        [LexemeKind::StringLiteral(string_literal)] => Ok(Expression::StringLiteral(string_literal.to_owned())),
        [LexemeKind::NumberLiteral(number)] => {
            if number.contains(".") {
                Ok(Expression::FloatLiteral(number.parse::<f32>().unwrap()))
            } else {
                Ok(Expression::IntegerLiteral(number.parse::<i32>().unwrap())) 
            }
        },

        [LexemeKind::Proc, ..] => {
            // procedure definition grammar
            // 'proc' '(' [ IDENTIFIER ':' TYPE { ',' IDENTIFIER ':' TYPE } ] ')'
            
            todo!();
        }
        
        _ => Err(Error {
            message: String::from("not yet implemented"),
            lines: lexemes.iter().map(|x| x.line).collect()
        })
    }
}

fn parse_const_let(lexemes_iter: &mut impl Iterator<Item = Lexeme>) -> Result<Statement, Error> {
    // println!("parse let\n{:#?}\n", lexemes);
    
    // constant declaration grammar
    // 'const' IDENTIFIER [ ':' TYPE ] '=' EXPRESSION ';'
    
    // variable declaration grammar
    // 'let' IDENTIFIER ( ':' TYPE ) | ( '=' EXPRESSION ) ';'
    
    let mut prev_lexeme = lexemes_iter.next().unwrap();
    let mut current_lexeme = lexemes_iter.next();
    
    let is_const = prev_lexeme.kind == LexemeKind::Const;
    
    // identifier
    let identifier = match &current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
            identifier.to_owned()
        },

        Some(lexeme) => return Err(Error {
            message: format!("expected identifier, found `{}`", lexeme.kind.to_string()),
            lines: vec![lexeme.line]
        }),
        
        None => return Err(Error {
            message: String::from("expected identifier, found nothing"),
            lines: vec![prev_lexeme.line]
        }),
    };

    prev_lexeme = current_lexeme.unwrap();
    current_lexeme = lexemes_iter.next();

    // check if type is explicit or to be inferred
    let is_type_explicit = match current_lexeme {
        Some(Lexeme{kind: LexemeKind::Colon, ..}) => true,
        Some(Lexeme{kind: LexemeKind::Equals, ..}) => false,
        
        Some(lexeme) => return Err(Error {
            message: format!("expected `:` or `=`, found `{}`", lexeme.kind.to_string()),
            lines: vec![lexeme.line]
        }),
        
        None => return Err(Error {
            message: String::from("expected `:` or `=`, found nothing"),
            lines: vec![prev_lexeme.line]
        }),
    };

    prev_lexeme = current_lexeme.unwrap();
    current_lexeme = lexemes_iter.next();            

    // panic!();
    
    let data_type: Option<Type> = if is_type_explicit {
        let mut type_lexemes: Vec<Lexeme> = Vec::new();
        loop {
            dbg!(&current_lexeme);
            match &current_lexeme {
                Some(lexeme @ Lexeme{kind: LexemeKind::Equals | LexemeKind::Semicolon, ..}) => {
                    if is_const && lexeme.kind == LexemeKind::Semicolon {
                        return Err(Error {
                            message: String::from("constant declaration must have a value known at compile-time"),
                            lines: vec![prev_lexeme.line]
                        });
                    }
                    
                    prev_lexeme = current_lexeme.unwrap();
                    current_lexeme = lexemes_iter.next();            
                    break
                },
                Some(lexeme) => type_lexemes.push(lexeme.to_owned()),
                None => {
                    return Err(Error {
                        message: String::from("constant declaration must take the form `const identifier: type = expression;` or `const identifier = expression`"),
                        lines: vec![prev_lexeme.line]
                    });
                }
                
            }

            prev_lexeme = current_lexeme.unwrap();
            current_lexeme = lexemes_iter.next();            
        }
        Some(parse_type(&type_lexemes)?)
    } else {
        None
    };
    
    // this will only be true if is_const is false
    if current_lexeme.is_none() {
        // so no need to check is_const
        return Ok(Statement::DeclareVariable {
            identifier,
            data_type,
            value: None
        })
    }
    
    // by this point, the only thing left is the expression (to be parsed) and the semicolon
    // after it

    let value = {
        // get all lexemes from after equals until semicolon
        let mut expression_lexemes: Vec<Lexeme> = Vec::new();
        loop {
            match &current_lexeme {
                Some(Lexeme{kind: LexemeKind::Semicolon, ..}) => break,
                Some(lexeme) => expression_lexemes.push(lexeme.clone()),
                None => return Err(Error {
                    message: String::from("expected expression, found nothing"),
                    lines: vec![prev_lexeme.line]
                })
            }
            prev_lexeme = current_lexeme.unwrap();
            current_lexeme = lexemes_iter.next();
        }

        parse_expression(expression_lexemes.as_slice())?
    };

    if is_const {
        Ok(Statement::DeclareConstant {
            identifier,
            data_type,
            value
        })
    } else {
        Ok(Statement::DeclareVariable {
            identifier,
            data_type,
            value: Some(value)
        })
    }
    
}

fn parse_compound(lexemes_iter: &mut impl Iterator<Item = Lexeme>) -> Result<Statement, Error> {
    lexemes_iter.next(); // skip the first left brace
    let mut current_lexeme = lexemes_iter.next();

    let inner_lexemes = {
        let mut inner_lexemes: Vec<Lexeme> = Vec::new();
        let mut brace_counter = 1;
        loop {
            // dbg!(&current_lexeme);
            
            match &current_lexeme {
                Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => brace_counter += 1,
                Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => brace_counter -= 1,
                Some(_) => {},
                None => unreachable!(),
            }

            if brace_counter == 0 {
                break;
            }

            inner_lexemes.push(current_lexeme.clone().unwrap());

            current_lexeme = lexemes_iter.next();
        }

        inner_lexemes
    };
    
    // separate inner lexemes by semicolon (respecting the brackets)
    let separated_lexemes = {
        let mut brace_counter = 0;
        let mut separated_lexemes: Vec<Vec<Lexeme>> = Vec::new();
        let mut buffer: Vec<Lexeme> = Vec::new();
        for lexeme in inner_lexemes {
            if lexeme.kind == LexemeKind::LeftBrace {
                brace_counter += 1;
            } else if lexeme.kind == LexemeKind::RightBrace {
                brace_counter -= 1;
            }
            
            buffer.push(lexeme.clone());

            if lexeme.kind == LexemeKind::Semicolon && brace_counter == 0 {
                separated_lexemes.push(buffer.clone());
                buffer.clear();
            }
        }
        
        // for the last expression that doesnt have semicolon
        if !buffer.is_empty() {
            separated_lexemes.push(buffer.clone());
            buffer.clear();
        }                    

        separated_lexemes
    };

    let mut inner_statements: Vec<Statement> = Vec::new();
    for statement_lexemes in separated_lexemes {
        inner_statements.push(parse(&statement_lexemes)?);
    }

    Ok(Statement::Compound(inner_statements))
}

pub fn parse(lexemes: &[Lexeme]) -> Result<Statement, Error> {
    let lexeme_kinds: Vec<LexemeKind> = lexemes.iter().map(|x| x.kind.clone()).collect();
    let mut lexemes_iter = lexemes.iter().cloned();
    match lexeme_kinds.as_slice() {
        [LexemeKind::LeftBrace, ..] => {
            println!("parse compound\n{:#?}\n", lexemes);
            parse_compound(&mut lexemes_iter)
        },
        
        [LexemeKind::Const | LexemeKind::Let, ..] =>{
            println!("parse const/let\n{:#?}\n", lexemes);
            parse_const_let(&mut lexemes_iter)
        },
            
        _ => {
            println!("not yet implemented\n{:#?}\n", lexemes);
            todo!()
        },
    }
}
