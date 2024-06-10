use crate::{error::*, lexer::*};

#[derive(Debug)]
pub enum Type {
    Int, Float, String, UserDefined(String),

    Procedure {
        param_types: Vec<Type>
    },

    Function {
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
        args: Vec<IdentifierAndType>,
        body: Box<Expression>
    },

    FunctionDefinition {
        args: Vec<IdentifierAndType>,
        return_type: Type,
        body: Box<Expression>
    },

    Compound {
        statements: Vec<Statement>,
    //    expression: Box<Expression>
    },
}

#[derive(Debug)]
pub enum Statement {
    Compound(Vec<Statement>),
    Return(Option<Expression>),
    
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
    },

    If {
        condition: Expression,
        on_true: Box<Statement>,
        on_false: Option<Box<Statement>>
    },

    ProcedureCall(Vec<Expression>),
}

pub struct Parser {
    lexemes_iter: Box<dyn Iterator<Item = Lexeme>>,
    prev_lexeme: Lexeme,
    current_lexeme: Option<Lexeme>,
    next_lexeme: Option<Lexeme>
}

impl Parser {
    const STATEMENT_START_LEXEMES: [LexemeKind; 6] = [
        LexemeKind::LeftBrace,
        LexemeKind::Const,
        LexemeKind::Let,
        LexemeKind::Identifier(String::new()),
        LexemeKind::Return,
        LexemeKind::If,
    ];

    const EXPRESSION_START_LEXEMES: [LexemeKind; 7] = [
        // for binary operations or single values
        LexemeKind::Identifier(String::new()),
        LexemeKind::NumberLiteral(String::new()),
        LexemeKind::StringLiteral(String::new()),
        LexemeKind::LeftParen,

        // for function and procedure definition
        LexemeKind::Func,
        LexemeKind::Proc,

        // for compound expressions
        LexemeKind::LeftBrace,  
    ];
    
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

    fn expect(&self, lexeme_kinds: &[LexemeKind]) -> Result<(), Error> {
        let mut expected_lexemes_strings: Vec<String> = Vec::new();
        for lexeme_kind in lexeme_kinds {
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
                    LexemeKind::Identifier(_) => if lexeme_kinds.contains(&LexemeKind::Identifier(String::new())) {
                        return Ok(());
                    },

                    LexemeKind::StringLiteral(_) => if lexeme_kinds.contains(&LexemeKind::StringLiteral(String::new())) {
                        return Ok(());
                    },
                    
                    LexemeKind::NumberLiteral(_) => if lexeme_kinds.contains(&LexemeKind::NumberLiteral(String::new())) {
                        return Ok(());
                    },

                    _ => if lexeme_kinds.contains(&lexeme.kind) {
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
    }
    
    pub fn parse_statement(&mut self) -> Result<Statement, Error> {
        println!("parse_statement");
        
        self.expect(&Self::STATEMENT_START_LEXEMES)?;
        
        match self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
                self.parse_statement_compound()
            },
        
            Some(Lexeme{kind: LexemeKind::Const | LexemeKind::Let, ..}) => {
                self.parse_statement_declare()
            },

            Some(Lexeme{kind: LexemeKind::Identifier(_), ..}) => {
                // todo: add checks for next lexeme
                
                match &self.next_lexeme {
                    Some(Lexeme{kind: LexemeKind::Equals, ..}) => self.parse_statement_assign(),
                    Some(Lexeme{kind: LexemeKind::LeftParen, ..}) => self.parse_statement_procedure_call(),
                    _ => todo!()
                }
                
            },

            Some(Lexeme{kind: LexemeKind::Return, ..}) => {
                self.parse_statement_return()
            },
        
            Some(Lexeme{kind: LexemeKind::If, ..}) => {
                self.parse_statement_if()
            },
            
            _ => unreachable!()
        }
    }

    fn parse_statement_procedure_call(&mut self) -> Result<Statement, Error> {
        // procedure call grammar
        // IDENTIFIER '(' [ EXPRESSION { ',' EXPRESSION } ] ')' ';'

        let identifier = self.current_lexeme.as_ref().unwrap();

        self.advance();
        self.expect(&Self::EXPRESSION_START_LEXEMES).or(
            self.expect(&[LexemeKind::LeftParen]) // explicitly putting this here
        )?;

        // dbg!(&self.current_lexeme);
        // panic!();

        self.advance();
        let mut args: Vec<Expression> = Vec::new();
        match &self.current_lexeme {
            // no args
            Some(Lexeme{kind: LexemeKind::RightParen, ..}) => {},
            Some(_) => {
                args.push(self.parse_expression()?);

                loop {
                    self.advance();
                    self.expect(&[
                        LexemeKind::RightParen,
                        LexemeKind::Comma
                    ])?;
                    
                    match &self.current_lexeme {
                        Some(Lexeme{kind: LexemeKind::RightParen, ..}) => break,
                        Some(Lexeme{kind: LexemeKind::Comma, ..}) => {
                            self.advance();
                            args.push(self.parse_expression()?);
                        },

                        _ => unreachable!()
                    }
                }
            },

            _ => unreachable!()
        }

        self.advance();
        self.expect(&[LexemeKind::Semicolon])?;
        
        Ok(Statement::ProcedureCall(args))
    }
    fn parse_statement_if(&mut self) -> Result<Statement, Error> {
        // if statement grammar
        // 'if' EXPRESSION STATEMENT [ 'else' STATEMENT ]

        self.advance();
        let condition = self.parse_expression()?;

        self.advance();
        let on_true = Box::new(self.parse_statement()?);
        
        let on_false = match &self.next_lexeme {
            Some(Lexeme{kind: LexemeKind::Else, ..}) => {
                self.advance(); // skip the else
                self.advance();
                Some(Box::new(self.parse_statement()?))        
            },

            _ => None
        };

        Ok(Statement::If {
            condition,
            on_true,
            on_false
        })
        
        // todo!()
    }
    
    fn parse_statement_return(&mut self) -> Result<Statement, Error> {
        // return grammar
        // 'return' [ EXPRESSION ] ';'
                
        self.advance();
        let expression = self.parse_expression().ok();

        if expression.is_some() {
            self.advance();
        }
                
        self.expect(&[LexemeKind::Semicolon])?;
                
        Ok(Statement::Return(expression))        
    }
    
    fn parse_statement_assign(&mut self) -> Result<Statement, Error> {
        // assignment grammar
        // IDENTIFIER '=' EXPRESSION ';'
        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => identifier.to_owned(),
            _ => unreachable!()
        };
    
        self.advance();
        self.expect(&[LexemeKind::Equals])?;

        // go to  start of expression;
        self.advance();
        let value = self.parse_expression()?;

        // go to semicolon after expression
        self.advance();
        self.expect(&[LexemeKind::Semicolon])?;
    
        Ok(Statement::Assign {
            identifier,
            value
        })
    }
    
    fn parse_statement_compound(&mut self) -> Result<Statement, Error> { 
        let mut statements: Vec<Statement> = Vec::new();

        loop {
            self.advance();
            match &self.current_lexeme {
                Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => {
                    break;
                },
                
                Some(_) => statements.push(self.parse_statement()?),
                None => unreachable!(),
            }
        }

        Ok(Statement::Compound(statements))
    }

    fn parse_statement_declare(&mut self) -> Result<Statement, Error> {
        println!("parse_const_let");

        // constant declaration grammar
        // 'const' IDENTIFIER [ ':' TYPE ] '=' EXPRESSION ';'
    
        // variable declaration grammar
        // 'let' IDENTIFIER ( ':' TYPE ) | ( '=' EXPRESSION ) ';'

        let is_const = self.current_lexeme.as_ref().unwrap().kind == LexemeKind::Const;
    
        self.advance();
        self.expect(&[LexemeKind::Identifier(String::new())])?;

        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => identifier.to_owned(),
            _ => unreachable!()
        };
    
        self.advance();
        self.expect(&[LexemeKind::Colon, LexemeKind::Equals])?;
    
        // check if type is explicit or to be inferred
        let is_type_explicit = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Colon, ..}) => true,
            Some(Lexeme{kind: LexemeKind::Equals, ..}) => false,
            _ => unreachable!()
        };
    
        // current lexeme should be type (if is_type_explicit == true)
        // or the start of an expression (if is_type_explicit == false)
    
        let data_type = if is_type_explicit {
            self.advance();
            let result = Some(self.parse_type()?);

            // go to semicolon or equals after type
            self.advance();
            result
        } else {
            None
        };
    
        // either ';' or '='
        if is_const {
            self.expect(&[LexemeKind::Equals])?;
        } else {
            self.expect(&[LexemeKind::Equals, LexemeKind::Semicolon])?;
        }
    
        let value = match &self.current_lexeme {        
            Some(Lexeme{kind: LexemeKind::Equals, ..}) => {
                self.advance();
                let result = Some(self.parse_expression()?);

                // go to semicolon after expression
                self.advance();
                self.expect(&[LexemeKind::Semicolon])?;
            
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
    
    fn parse_expression(&mut self) -> Result<Expression, Error> {
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
        
        self.expect(&Self::EXPRESSION_START_LEXEMES)?;

        match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(_) | LexemeKind::NumberLiteral(_) | LexemeKind::StringLiteral(_) | LexemeKind::LeftParen, ..}) => {        
                let operand_1 = match &self.current_lexeme {
                    Some(lexeme @ Lexeme{kind: LexemeKind::Identifier(_) | LexemeKind::StringLiteral(_) | LexemeKind::NumberLiteral(_), ..}) => {
                        parse_term(lexeme)?
                    },

                    Some(Lexeme{kind: LexemeKind::LeftParen, .. }) => {
                        self.advance();
                        let expression = self.parse_expression()?;

                        self.advance();
                        self.expect(&[LexemeKind::RightParen])?;
      
                        expression
                    }

                    _ => unreachable!()
                };

                if let Ok(operation) = BinaryOperation::from_lexeme(self.next_lexeme.as_ref().unwrap()) {
                    self.advance(); // skip opereation

                    self.advance();
                    let operand_2 = self.parse_expression()?;
        
                    Ok(Expression::Binary {
                        operation,
                        operand_1: Box::new(operand_1),
                        operand_2: Box::new(operand_2),
                    })
                } else {
                    Ok(operand_1)
                }    
            },
            
            Some(Lexeme{kind: lexeme_kind @ (LexemeKind::Func | LexemeKind::Proc), ..}) => {
                let is_proc = *lexeme_kind == LexemeKind::Proc;

                // procedure definition grammar
                // 'proc' '(' [ IDENTIFIER ':' TYPE { ',' IDENTIFIER ':' TYPE } ] ')' STATEMENT

                // function definition grammar
                // 'func' '(' [ IDENTIFIER ':' TYPE { ',' IDENTIFIER ':' TYPE } ] '->' TYPE STATEMENT

                self.advance();
                self.expect(&[LexemeKind::LeftParen])?;

                let mut args: Vec<IdentifierAndType> = Vec::new();

                self.advance();
                self.expect(&[
                    LexemeKind::RightParen,
                    LexemeKind::Identifier(String::new())
                ])?;

                // parse the first argument (if there are any)
                match &self.current_lexeme {
                    Some(Lexeme{kind: LexemeKind::RightParen, ..}) => {},
                    Some(Lexeme{kind: LexemeKind::Identifier(_), ..}) => {
                        args.push(self.parse_identifier_and_type()?);

                        // parse the rest
                        loop {
                            self.advance();
                            self.expect(&[
                                LexemeKind::RightParen,
                                LexemeKind::Comma
                            ])?;

                            match &self.current_lexeme {
                                Some(Lexeme{kind: LexemeKind::RightParen, ..}) => break,
                                Some(Lexeme{kind: LexemeKind::Comma, ..}) => {
                                    self.advance();
                                    args.push(self.parse_identifier_and_type()?);
                                },

                                _ => unreachable!()
                            }
                        }
                    }

                    _ => unreachable!()
                }

                // right now we are at right paren

                self.advance();
                let return_type = if !is_proc {
                    self.expect(&[LexemeKind::Arrow])?;

                    self.advance();
                    let result = Some(self.parse_type()?);

                    self.advance();

                    result
                } else {
                    None
                };

                let body = Box::new(self.parse_expression()?);

                if is_proc {
                    Ok(Expression::ProcedureDefinition {
                        args,
                        body
                    })
                } else {
                    Ok(Expression::FunctionDefinition {
                        args,
                        return_type: return_type.unwrap(),
                        body
                    })                    
                }
            },

            Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
                // todo (maybe) allow implicit returns?

                println!("here");
                
                let mut statements: Vec<Statement> = Vec::new();

                loop {
                    self.advance();
                    match &self.current_lexeme {
                        Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => {
                            break;
                        },
                
                        Some(_) => statements.push(self.parse_statement()?),
                        None => unreachable!(),
                    }
                }

                Ok(Expression::Compound{statements})
            },
            
            _ => unreachable!()
        }
    }
    
    fn parse_type(&mut self) -> Result<Type, Error> {
        //println!("parse type\n{:#?}\n", lexemes_iter);
        self.expect(&[
            LexemeKind::Identifier(String::new()),
            LexemeKind::Proc,
            LexemeKind::Func,
        ])?;

        match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => {
                match identifier.as_str() {
                    "int" => Ok(Type::Int),
                    "float" => Ok(Type::Float),
                    "string" => Ok(Type::String),
                    _ => Ok(Type::UserDefined(identifier.to_string())),
                }            
            },

            Some(lexeme @ Lexeme{kind: LexemeKind::Proc | LexemeKind::Func, ..}) => {
                let is_proc = lexeme.kind == LexemeKind::Proc;

                // // func type grammar
                // // 'func' '(' [ TYPE { ',' TYPE } ] ')' '->' TYPE
            
                // // proc type grammar
                // // 'proc' '(' [ TYPE { ',' TYPE } ] ')' 

                self.advance();
                self.expect(&[LexemeKind::LeftParen])?;

                // parse the first type (if there are any)
                let mut param_types: Vec<Type> = Vec::new();            
            
                self.advance();
                self.expect(&[
                    LexemeKind::Identifier(String::new()),
                    LexemeKind::Proc,
                    LexemeKind::Func,
                    LexemeKind::RightParen
                ])?;

                match &self.current_lexeme {
                    Some(Lexeme{kind: LexemeKind::RightParen, ..}) => {},
                    Some(_) => {
                        param_types.push(self.parse_type()?);

                        // parse the rest
                        loop {
                            self.advance();
                            self.expect(&[
                                LexemeKind::RightParen,
                                LexemeKind::Comma
                            ])?;

                            match &self.current_lexeme {
                                Some(Lexeme{kind: LexemeKind::RightParen, ..}) => break,
                                Some(Lexeme{kind: LexemeKind::Comma, ..}) => {
                                    self.advance();
                                    param_types.push(self.parse_type()?);
                                },

                                _ => unreachable!()
                            }
                        }
                    },

                    _ => unreachable!()
                }

                // right now we are at right paren
                
                if is_proc {
                    return Ok(Type::Procedure {
                        param_types
                    });
                }

                self.advance();
                self.expect(&[LexemeKind::Arrow])?;

                self.advance();
                let return_type = Box::new(self.parse_type()?);

                Ok(Type::Function {
                    param_types,
                    return_type
                })
            },

            _ => unreachable!()
        }
    }

    fn parse_identifier_and_type(&mut self) -> Result<IdentifierAndType, Error> {
        self.expect(&[LexemeKind::Identifier(String::new())])?;

        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), ..}) => identifier.to_owned(),
            _ => unreachable!()
        };

        self.advance();
        self.expect(&[LexemeKind::Colon])?;
        
        self.advance();
        let data_type = self.parse_type()?;

        Ok(IdentifierAndType {
            identifier,
            data_type
        })
    }

}
