use crate::{error::*, lexer::*};

pub struct Parser {
    lexemes_iter: Box<dyn Iterator<Item = Lexeme>>,
    prev_lexeme: Lexeme,
    current_lexeme: Option<Lexeme>,
    next_lexeme: Option<Lexeme>
}

impl Parser {
    pub fn new(lexemes: &[Lexeme]) -> Parser {
        let mut lexemes_iter = Vec::from(lexemes).into_iter();
        let next_lexeme = lexemes_iter.next();
        
        let mut parser = Parser {
            // lexemes_iter: Box::new(Vec::from(lexemes).into_iter()),
            lexemes_iter: Box::new(lexemes_iter.clone()),
            prev_lexeme: Lexeme{kind: LexemeKind::LeftBrace, line: 1},
            current_lexeme: None,
            next_lexeme,
        };

        parser.advance();
        parser
    }

    fn advance(&mut self) {
        if let Some(lexeme) = &self.current_lexeme {
            self.prev_lexeme = lexeme.clone()
        }
        
        self.current_lexeme = self.next_lexeme.clone();
        self.next_lexeme = self.lexemes_iter.next();

        dbg!(&self.current_lexeme);
    }

    fn expect(&self, expected_lexeme_kinds: &[LexemeKind]) -> Result<(), Error> {
        let mut expected_lexemes_strings: Vec<String> = Vec::new();
        for lexeme_kind in expected_lexeme_kinds {
            match lexeme_kind {
                LexemeKind::Identifier(_) => expected_lexemes_strings.push(String::from("identifier")),
                LexemeKind::StringLiteral(_) => expected_lexemes_strings.push(String::from("string literal")),
                LexemeKind::NumberLiteral(_) => expected_lexemes_strings.push(String::from("number literal")),
                _ => expected_lexemes_strings.push(format!{"`{}`", lexeme_kind.to_string()})
            }
        }

        let expected_lexemes_list_message = expected_lexemes_strings.join(" or ");
        
        match &self.current_lexeme {
            None | Some(Lexeme{kind: LexemeKind::RightBrace, line: 0}) => Err(Error {
                message: format!("expected {}, found nothing", expected_lexemes_list_message),
                lines: vec![self.prev_lexeme.line]
            }),
            
            Some(lexeme) => {
                match lexeme.kind {
                    LexemeKind::Identifier(_) => if expected_lexeme_kinds.contains(&LexemeKind::Identifier(String::new())) {
                        return Ok(());
                    },

                    LexemeKind::StringLiteral(_) => if expected_lexeme_kinds.contains(&LexemeKind::StringLiteral(String::new())) {
                        return Ok(());
                    },
                    
                    LexemeKind::NumberLiteral(_) => if expected_lexeme_kinds.contains(&LexemeKind::NumberLiteral(String::new())) {
                        return Ok(());
                    },

                    _ => if expected_lexeme_kinds.contains(&lexeme.kind) {
                        return Ok(());
                    }
                }

                Err(Error {
                    message: format!("expected {}, found `{}`",
                        expected_lexemes_list_message, lexeme.kind.to_string()),
                    lines: vec![lexeme.line]
                })
            }
        }
        
        // let mut expected_list_message = format!("`{}`", expected_lexeme_kinds[0].to_string());
        // for lexeme_kind in &expected_lexeme_kinds[1..] {
        //     expected_list_message = format!("{expected_list_message} or `{}`", lexeme_kind.to_string());
        // }

        // match &self.current_lexeme {
        //     None | Some(Lexeme{kind: LexemeKind::RightBrace, line: 0}) => Err(Error {
        //         message: format!("expected {expected_list_message}, found nothing"),
        //         lines: vec![self.prev_lexeme.line]
        //     }),

        //     Some(lexeme) => {
        //         if expected_lexeme_kinds.contains(&lexeme.kind) {
        //             Ok(())
        //         } else {
        //             Err(Error {
        //                 message: format!("expected {expected_list_message}, found `{}`", lexeme.kind.to_string()),
        //                 lines: vec![self.prev_lexeme.line]
        //             })
        //         }
        //     },
        // }
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

impl BinaryOperation {
    fn from_lexeme(lexeme: &Lexeme) -> Result<Self, Error> {
        match lexeme.kind {
            LexemeKind::DoubleEquals  => Ok(BinaryOperation::EqualTo),
            LexemeKind::NotEquals     => Ok(BinaryOperation::NotEqualTo),
            LexemeKind::LessEquals    => Ok(BinaryOperation::LessEqualTo),
            LexemeKind::GreaterEquals => Ok(BinaryOperation::GreaterEqualTo),
            LexemeKind::Less          => Ok(BinaryOperation::LessThan),
            LexemeKind::Greater       => Ok(BinaryOperation::GreaterThan),
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

    Assign {
        identifier: String,
        value: Expression,
    }
}

const INFIX_LEXEMES: [LexemeKind; 10] = [
    LexemeKind::DoubleEquals, LexemeKind::NotEquals, LexemeKind::LessEquals,
    LexemeKind::GreaterEquals, LexemeKind::Less, LexemeKind::Greater,

    LexemeKind::Plus, LexemeKind::Minus, LexemeKind::Star, LexemeKind::Slash
];

fn parse_type(parser: &mut Parser) -> Result<Type, Error> {
    //println!("parse type\n{:#?}\n", lexemes_iter);
    parser.expect(&[
        LexemeKind::Identifier(String::new()),
        LexemeKind::Proc,
        LexemeKind::Func,
    ])?;

    match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
            match identifier.as_str() {
                "int" => Ok(Type::Int),
                "float" => Ok(Type::Float),
                "string" => Ok(Type::String),
                _ => Ok(Type::UserDefined(identifier.to_string())),
            }            
        },

        Some(lexeme @ Lexeme{kind: LexemeKind::Proc | LexemeKind::Func, ..}) => {
            let is_func = lexeme.kind == LexemeKind::Func;

            // // func type grammar
            // // 'func' '(' [ TYPE { ',' TYPE } ] ')' '->' TYPE
            
            // // proc type grammar
            // // 'proc' '(' [ TYPE { ',' TYPE } ] ')' 

            // parser.advance();
            // parser.expect(&[LexemeKind::LeftParen])?;

            // parser.advance();
            // let param_types = {
                
            // };
            
            todo!();
        },

        _ => unreachable!()
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

    // checks if a lexeme is a valid lexeme in an expression
    fn is_expression_lexeme(lexeme: &Lexeme) -> bool {
        match lexeme.kind {
            LexemeKind::Identifier(_) |
            LexemeKind::StringLiteral(_) |
            LexemeKind::NumberLiteral(_) |
            LexemeKind::DoubleEquals |
            LexemeKind::NotEquals |
            LexemeKind::LessEquals |
            LexemeKind::GreaterEquals |
            LexemeKind::Less |
            LexemeKind::Greater |
            LexemeKind::Plus | 
            LexemeKind::Minus | 
            LexemeKind::Star | 
            LexemeKind::Slash | 
            LexemeKind::Percent => true,
            _ => false
        }
    }

    parser.expect(&[
        LexemeKind::Identifier(String::new()),
        LexemeKind::NumberLiteral(String::new()),
        LexemeKind::StringLiteral(String::new()),
        LexemeKind::LeftParen,
    ])?;

    
    
    let operand_1 = match &parser.current_lexeme {
        Some(lexeme @ Lexeme{kind: LexemeKind::Identifier(_) | LexemeKind::StringLiteral(_) | LexemeKind::NumberLiteral(_), ..}) => {
            parse_term(lexeme)?
        },

        Some(Lexeme{kind: LexemeKind::LeftParen, .. }) => {
            parser.advance();
            let expression = parse_expression(parser)?;

            parser.advance();
            parser.expect(&[LexemeKind::RightParen])?;
      
            expression
        }

        _ => unreachable!()
    };

    if let Ok(operation) = BinaryOperation::from_lexeme(parser.next_lexeme.as_ref().unwrap()) {
        parser.advance(); // skip opereation

        parser.advance();
        let operand_2 = parse_expression(parser)?;
        
        Ok(Expression::Binary {
            operation,
            operand_1: Box::new(operand_1),
            operand_2: Box::new(operand_2),
        })
    } else {
        Ok(operand_1)
    }    
}

fn parse_declare(parser: &mut Parser) -> Result<Statement, Error> {
    println!("parse_const_let");

    // constant declaration grammar
    // 'const' IDENTIFIER [ ':' TYPE ] '=' EXPRESSION ';'
    
    // variable declaration grammar
    // 'let' IDENTIFIER ( ':' TYPE ) | ( '=' EXPRESSION ) ';'

    let is_const = parser.current_lexeme.as_ref().unwrap().kind == LexemeKind::Const;
    
    parser.advance();
    parser.expect(&[LexemeKind::Identifier(String::new())])?;

    let identifier = match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => identifier.to_owned(),
        _ => unreachable!()
    };
    
    // let identifier = match &parser.current_lexeme {
    //     Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
    //         identifier.to_owned()
    //     },

    //     Some(lexeme) => return Err(Error {
    //         message: format!("expected identifier, found `{}`", lexeme.kind.to_string()),
    //         lines: vec![lexeme.line]
    //     }),
        
    //     None => return Err(Error {
    //         message: String::from("expected identifier, found nothing"),
    //         lines: vec![parser.prev_lexeme.line]
    //     }),
    // };

    parser.advance();
    parser.expect(&[LexemeKind::Colon, LexemeKind::Equals])?;
    
    // check if type is explicit or to be inferred
    let is_type_explicit = match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Colon, ..}) => true,
        Some(Lexeme{kind: LexemeKind::Equals, ..}) => false,
        _ => unreachable!()
    };
    
    // current lexeme should be type (if is_type_explicit == true)
    // or the start of an expression (if is_type_explicit == false)
    
    let data_type = if is_type_explicit {
        parser.advance();
        let result = Some(parse_type(parser)?);

        // go to semicolon or equals after type
        parser.advance();
        result
    } else {
        None
    };
    
    // either ';' or '='
    if is_const {
        parser.expect(&[LexemeKind::Equals])?;
    } else {
        parser.expect(&[LexemeKind::Equals, LexemeKind::Semicolon])?;
    }
    
    let value = match &parser.current_lexeme {        
        Some(Lexeme{kind: LexemeKind::Equals, ..}) => {
            parser.advance();
            let result = Some(parse_expression(parser)?);

            // go to semicolon after expression
            parser.advance();
            parser.expect(&[LexemeKind::Semicolon])?;
            
            result
        },
        
        Some(Lexeme{kind: LexemeKind::Semicolon, ..}) => {
            // parser.advance();
            None
        },
        
        _ => unreachable!()
    
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

fn parse_compound(parser: &mut Parser) -> Result<Statement, Error> { 
    let mut statements: Vec<Statement> = Vec::new();

    loop {
        parser.advance();
        match &parser.current_lexeme {
            Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => {
                break;
            }
            Some(_) => statements.push(parse(parser)?),
            None => unreachable!(),
        }
    }

    Ok(Statement::Compound(statements))
}

pub fn parse_assign(parser: &mut Parser) -> Result<Statement, Error> {
    // assignment grammar
    // IDENTIFIER '=' EXPRESSION ';'
    let identifier = match &parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => identifier.to_owned(),
        _ => unreachable!()
    };
    
    parser.advance();
    parser.expect(&[LexemeKind::Equals])?;

    // go to  start of expression;
    parser.advance();
    let value = parse_expression(parser)?;

    // go to semicolon after expression
    parser.advance();
        
    Ok(Statement::Assign {
        identifier,
        value
    })
}

pub fn parse(parser: &mut Parser) -> Result<Statement, Error> {
    // let mut parser = Parser::new(lexemes);
    // parser.advance();
    println!("parse");
    
    // let lexeme_kinds: Vec<LexemeKind> = lexemes.iter().map(|x| x.kind.clone()).collect();
    // let mut lexemes_iter = lexemes.iter().cloned();
    match parser.current_lexeme {
        Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
            parse_compound(parser)
        },
        
        Some(Lexeme{kind: LexemeKind::Const | LexemeKind::Let, ..}) => {
            parse_declare(parser)
        },

        Some(Lexeme{kind: LexemeKind::Identifier(_), ..}) => {
            parse_assign(parser)
        },
        
        _ => {
            // println!("not yet implemented\n{:#?}\n", lexemes);
            todo!()
        },
    }
}
