use crate::{lexer::*, error::*};

struct Parser {
    lexemes_iter: Box<dyn Iterator<Item = Lexeme>>,
    prev_lexeme: Lexeme,
    current_lexeme: Option<Lexeme>,
    next_lexeme: Option<Lexeme>
}

impl Parser {
    fn new<'a>(lexemes: &[Lexeme]) -> Parser {
        let mut lexemes_iter = Vec::from(lexemes).into_iter();
        let next_lexeme = lexemes_iter.next();
        
        Parser {
            // lexemes_iter: Box::new(Vec::from(lexemes).into_iter()),
            lexemes_iter: Box::new(lexemes_iter.clone()),
            prev_lexeme: Lexeme{kind: LexemeKind::LeftBrace, line: 1},
            current_lexeme: None,
            next_lexeme,
        }
    }

    fn advance(&mut self) {
        if let Some(lexeme) = &self.current_lexeme {
            self.prev_lexeme = lexeme.clone()
        }
        
        self.current_lexeme = self.next_lexeme.clone();
        self.next_lexeme = self.lexemes_iter.next();

        dbg!(&self.current_lexeme);
    }

    
}

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
pub enum BinaryOperation {
    Add, Subtract, Multiply, Divide, Modulo,
    EqualTo, NotEqualTo, LessEqualTo, GreaterEqualTo, LessThan, GreaterThan,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f32),

    Binary {
        operation: BinaryOperation,
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
    },
}

const INFIX_LEXEMES: [LexemeKind; 10] = [
    LexemeKind::DoubleEquals, LexemeKind::NotEquals, LexemeKind::LessEquals,
    LexemeKind::GreaterEquals, LexemeKind::Less, LexemeKind::Greater,

    LexemeKind::Plus, LexemeKind::Minus, LexemeKind::Star, LexemeKind::Slash
];

fn parse_type(parser: &mut Parser) -> Result<Type, Error> {
    //println!("parse type\n{:#?}\n", lexemes_iter);
    match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
            match identifier.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "string" => Ok(Type::String),
                _ => Ok(Type::UserDefined(identifier.to_string())),
            }            
        },

        Some(Lexeme{kind: LexemeKind::Proc | LexemeKind::Func, ..}) => {
            todo!()
        },
        
        Some(lexeme) => Err(Error {
            message: format!("expected type, found `{}`", lexeme.kind.to_string()),
            lines: vec![lexeme.line]
        }),

        None => Err(Error {
            message: String::from("expected type, found nothing"),
            lines: vec![1]
        })
    }
    
    // panic!();
    // match lexeme_kinds.as_slice() {
    //     [LexemeKind::Identifier(type_name)] => {
    //         match type_name.as_str() {
    //             "int" => Ok(Type::Int),
    //             "float" => Ok(Type::Float),
    //             "string" => Ok(Type::String),
    //             _ => Ok(Type::UserDefined(type_name.to_string())),
    //         }
    //     },
        
    //     [LexemeKind::Proc | LexemeKind::Func, ..] => {
    //         let is_func = lexemes[0].kind == LexemeKind::Func;
            
    //         // func type grammar
    //         // 'func' '(' [ TYPE { ',' TYPE } ] ')' '->' TYPE

    //         // proc type grammar
    //         // 'proc' '(' [ TYPE { ',' TYPE } ] ')' 

    //         let mut lexemes_iter = lexemes.iter();            
    //         let mut prev_lexeme = lexemes_iter.next().unwrap();
    //         let mut current_lexeme = lexemes_iter.next();

    //         // '(' --------------------
    //         match current_lexeme {
    //             Some(Lexeme{kind: LexemeKind::LeftParen, ..}) => {},
                
    //             Some(lexeme) => return Err(Error {
    //                 message: format!("expected `(`, found `{}`", lexeme.kind.to_string()),
    //                 lines: vec![lexeme.line]
    //             }),
                
    //             None => return Err(Error {
    //                 message: String::from("expected `(`, found nothing"),
    //                 lines: vec![prev_lexeme.line]
    //             }),
    //         }

    //         // TYPE_LIST ----------------
    //         prev_lexeme = current_lexeme.unwrap();
    //         current_lexeme = lexemes_iter.next();

    //         // get all lexemes between the parens and separate them by comma while still
    //         // respecting parens
    //         let mut paren_counter = 1; // 1 because we skipped the first left paren
    //         let mut type_list_lexemes: Vec<Vec<Lexeme>> = Vec::new();
    //         let mut buffer: Vec<Lexeme> = Vec::new();
    //         loop {
    //             // i think current_lexeme will never be none because the lexer ensures that
    //             // parens are always correct
    //             // todo: remove?
    //             // dbg!(&current_lexeme);
    //             if current_lexeme.is_none() {
    //                 return Err(Error {
    //                     message: String::from("procedure type must take the form `proc(type, ...)`"),
    //                     lines: vec![prev_lexeme.line],
    //                 })                    
    //             }

    //             let lexeme = current_lexeme.unwrap();
    //             prev_lexeme = current_lexeme.unwrap();
    //             current_lexeme = lexemes_iter.next();
                
    //             if lexeme.kind == LexemeKind::LeftParen {
    //                 paren_counter += 1;
    //             } else if lexeme.kind == LexemeKind::RightParen {
    //                 paren_counter -= 1;
    //                 if paren_counter == 0 {
    //                     break;
    //                 }
    //             }

    //             if lexeme.kind == LexemeKind::Comma && paren_counter == 1 {
    //                 type_list_lexemes.push(buffer.clone());
    //                 buffer.clear();
    //                 continue;
    //             }

    //             buffer.push(lexeme.clone());
    //         }

    //         // for the last type that isnt followed by a comma
    //         if !buffer.is_empty() {
    //             type_list_lexemes.push(buffer.clone());
    //             buffer.clear();
    //         }                    

    //         // parse each type in the type list
    //         let param_types =  {
    //             let mut result: Vec<Type> = Vec::new();
    //             for type_lexemes in type_list_lexemes {
    //                 let param_type = parse_type(&type_lexemes)?;
    //                 result.push(param_type);
    //             }

    //             result
    //         };

    //         // dbg!(&param_types);

    //         if is_func {
    //             // -> -------------------
    //             match current_lexeme {
    //                 Some(Lexeme{kind: LexemeKind::Arrow, ..}) => {},
    //                 Some(lexeme) => return Err(Error {
    //                     message: format!("expected `->`, found `{}`", lexeme.kind.to_string()),
    //                     lines: vec![lexeme.line]
    //                 }),
    //                 None => return Err(Error {
    //                     message: String::from("expected `->`, found nothing"),
    //                     lines: vec![prev_lexeme.line]
    //                 })
    //             }

    //             // return type -----------------                
    //             let return_type = {
    //                 let remaining: Vec<_> = lexemes_iter.cloned().collect();
    //                 Box::new(parse_type(remaining.as_slice())?)
    //             };

    //             Ok(Type::Func {
    //                 param_types,
    //                 return_type
    //             })
    //         } else { 
    //             Ok(Type::Proc {
    //                 param_types
    //             })                    
    //         }
    //     },

    //     _ => Err(Error {
    //         message: format!("expected type_name, found `{}`", lexemes[0].kind.to_string()),
    //         lines: lexemes.iter().map(|x| x.line).collect()
    //     })
    // }
    
    // todo!()
}

fn parse_expression(parser: &mut Parser) -> Result<Expression, Error> {
    // println!("parse expression\n{:#?}\n", lexemes);

    fn parse_term(lexeme: &Lexeme) -> Result<Expression, Error> {
        match &lexeme.kind {
            LexemeKind::Identifier(identifier) => Ok(Expression::Identifier(identifier.to_owned())),
            LexemeKind::StringLiteral(string) => Ok(Expression::StringLiteral(string.to_owned())),
            LexemeKind::NumberLiteral(number) => {
                if number.contains(".") {
                    Ok(Expression::FloatLiteral(number.parse::<f32>().unwrap()))
                } else {
                    Ok(Expression::IntegerLiteral(number.parse::<i32>().unwrap()))
                }
            },
            
            _ => Err(Error {
                message: format!("expected term, found `{}`", lexeme.kind.to_string()),
                lines: vec![lexeme.line]
            })
        }
    }

    fn get_operation(lexeme: &Lexeme) -> Result<BinaryOperation, Error> {
        match lexeme.kind {
            LexemeKind::DoubleEquals  => Ok(BinaryOperation::EqualTo),
            LexemeKind::NotEquals     => Ok(BinaryOperation::NotEqualTo),
            LexemeKind::LessEquals    => Ok(BinaryOperation::LessEqualTo),
            LexemeKind::GreaterEquals => Ok(BinaryOperation::GreaterEqualTo),
            LexemeKind::Plus          => Ok(BinaryOperation::Add),
            LexemeKind::Minus         => Ok(BinaryOperation::Subtract),
            LexemeKind::Star          => Ok(BinaryOperation::Multiply),
            LexemeKind::Slash         => Ok(BinaryOperation::Divide),
            LexemeKind::Percent       => Ok(BinaryOperation::Modulo),
            _ => Err(Error {
                message: format!("expected binary operator, found `{}`", lexeme.kind.to_string()),
                lines: vec![lexeme.line]
            })            
        }
    }
    
    match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(_) | LexemeKind::StringLiteral(_) | LexemeKind::NumberLiteral(_), ..}) => {
            if parser.next_lexeme.is_none() {
                Err(Error {
                    message: String::from("expected `;` or an infix operator, found nothing"),
                    lines: vec![parser.current_lexeme.clone().unwrap().line]
                })
            } else {
                let next_lexeme = parser.next_lexeme.clone().unwrap();
                if next_lexeme.kind == LexemeKind::Semicolon {
                    parse_term(&parser.current_lexeme.clone().unwrap())
                } else {                    
                    if INFIX_LEXEMES.contains(&next_lexeme.kind) {
                        let left_operand = parse_term(&parser.current_lexeme.clone().unwrap())?;

                        // infix operator
                        parser.advance();
                        let operation = get_operation(&parser.current_lexeme.clone().unwrap())?;
                        
                        // right operand
                        parser.advance();
                        let right_operand = parse_expression(parser)?;

                        Ok(Expression::Binary {
                            operation,
                            operand_1: Box::new(left_operand),
                            operand_2: Box::new(right_operand),
                        })
                        
                        // todo!()
                    } else {
                        Err(Error {
                            message: format!("expected `;` or infix operator, found `{}`", next_lexeme.kind.to_string()),
                            lines: vec![parser.current_lexeme.clone().unwrap().line, next_lexeme.line]
                        })
                    }
                }
            }
            
        },
        
        _ => todo!()
    }
}

fn parse_const_let(parser: &mut Parser) -> Result<Statement, Error> {
    // constant declaration grammar
    // 'const' IDENTIFIER [ ':' TYPE ] '=' EXPRESSION ';'
    
    // variable declaration grammar
    // 'let' IDENTIFIER ( ':' TYPE ) | ( '=' EXPRESSION ) ';'

    parser.advance();
    let is_const = parser.prev_lexeme.kind == LexemeKind::Const;
    
    // identifier
    let identifier = match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
            identifier.to_owned()
        },

        Some(lexeme) => return Err(Error {
            message: format!("expected identifier, found `{}`", lexeme.kind.to_string()),
            lines: vec![lexeme.line]
        }),
        
        None => return Err(Error {
            message: String::from("expected identifier, found nothing"),
            lines: vec![parser.prev_lexeme.line]
        }),
    };

    parser.advance();
    
    // check if type is explicit or to be inferred
    let is_type_explicit = match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Colon, ..}) => true,
        Some(Lexeme{kind: LexemeKind::Equals, ..}) => false,
        
        Some(lexeme) => return Err(Error {
            message: format!("expected `:` or `=`, found `{}`", lexeme.kind.to_string()),
            lines: vec![lexeme.line]
        }),
        
        None => return Err(Error {
            message: String::from("expected `:` or `=`, found nothing"),
            lines: vec![parser.prev_lexeme.line]
        }),
    };

    let data_type = if is_type_explicit {
        parser.advance();
        Some(parse_type(parser)?)
    } else {
        None
    };

    // either ';' or '='
    let value = match &parser.current_lexeme {        
        Some(Lexeme{kind: LexemeKind::Equals, ..}) => {
            parser.advance();
            Some(parse_expression(parser)?)
        },

        Some(Lexeme{kind: LexemeKind::Semicolon, ..}) => if is_const {
            return Err(Error {
                message: String::from("constant declaration must have a value known at compile-time"),
                lines: vec![parser.prev_lexeme.line]               
            });
        } else {
            None
        },

        Some(lexeme) => {
            let expected_symbol = if is_const { "`=`" } else { "`;` or '='" };
            return Err(Error {
                message: format!("expected {}, found `{}`", expected_symbol, lexeme.kind.to_string()),
                lines: vec![parser.prev_lexeme.line]               
            })
        },
        
        None => {
            let expected_symbol = if is_const { "`=`" } else { "`;` or '='" };
            return Err(Error {
                message: format!("expected {}, found nothing", expected_symbol),
                lines: vec![parser.prev_lexeme.line]               
            })
        }, 
    };

    if is_const {
        Ok(Statement::DeclareConstant {
            identifier,
            data_type,
            value: value.unwrap()
        })
    } else {
        Ok(Statement::DeclareVariable {
            identifier,
            data_type,
            value
        })
    }    
}

// fn parse_compound(parser: Parser) -> Result<Statement, Error> {
//     lexemes_iter.next(); // skip the first left brace
//     let mut current_lexeme = lexemes_iter.next();

//     let inner_lexemes = {
//         let mut inner_lexemes: Vec<Lexeme> = Vec::new();
//         let mut brace_counter = 1;
//         loop {
//             // dbg!(&current_lexeme);
            
//             match &current_lexeme {
//                 Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => brace_counter += 1,
//                 Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => brace_counter -= 1,
//                 Some(_) => {},
//                 None => unreachable!(),
//             }

//             if brace_counter == 0 {
//                 break;
//             }

//             inner_lexemes.push(current_lexeme.clone().unwrap());

//             current_lexeme = lexemes_iter.next();
//         }

//         inner_lexemes
//     };
    
//     // separate inner lexemes by semicolon (respecting the brackets)
//     let separated_lexemes = {
//         let mut brace_counter = 0;
//         let mut separated_lexemes: Vec<Vec<Lexeme>> = Vec::new();
//         let mut buffer: Vec<Lexeme> = Vec::new();
//         for lexeme in inner_lexemes {
//             if lexeme.kind == LexemeKind::LeftBrace {
//                 brace_counter += 1;
//             } else if lexeme.kind == LexemeKind::RightBrace {
//                 brace_counter -= 1;
//             }
            
//             buffer.push(lexeme.clone());

//             if lexeme.kind == LexemeKind::Semicolon && brace_counter == 0 {
//                 separated_lexemes.push(buffer.clone());
//                 buffer.clear();
//             }
//         }
        
//         // for the last expression that doesnt have semicolon
//         if !buffer.is_empty() {
//             separated_lexemes.push(buffer.clone());
//             buffer.clear();
//         }                    

//         separated_lexemes
//     };

//     let mut inner_statements: Vec<Statement> = Vec::new();
//     for statement_lexemes in separated_lexemes {
//         inner_statements.push(parse(&statement_lexemes)?);
//     }

//     Ok(Statement::Compound(inner_statements))
// }

pub fn parse(lexemes: &[Lexeme]) -> Result<Statement, Error> {
    let mut parser = Parser::new(lexemes);
    parser.advance();
    
    // let lexeme_kinds: Vec<LexemeKind> = lexemes.iter().map(|x| x.kind.clone()).collect();
    // let mut lexemes_iter = lexemes.iter().cloned();
    match parser.current_lexeme {
        // Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
        //     println!("parse compound\n{:#?}\n", lexemes);
        //     parse_compound(parser)
        // },
        
        Some(Lexeme{kind: LexemeKind::Const | LexemeKind::Let, ..}) =>{
            println!("parse const/let\n{:#?}\n", lexemes);
            parse_const_let(&mut parser)
        },
            
        _ => {
            println!("not yet implemented\n{:#?}\n", lexemes);
            todo!()
        },
    }
}
