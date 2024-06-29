use crate::{error::*, lexer::*};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol {
    pub name: String,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Integer, Float, String, Boolean, UserDefined(Symbol),

    Procedure {
        param_types: Vec<Type>
    },

    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Integer => String::from("int"),
            Type::Float => String::from("float"),
            Type::String => String::from("string"),
            Type::Boolean => String::from("bool"),
            Type::UserDefined(symbol) => symbol.name.to_owned(),
            
            Type::Procedure { param_types } => {
                format!("proc({})", param_types
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "))
            },

            Type::Function { param_types, return_type } => {
                format!("func({}) -> {}", param_types
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                    return_type.to_string())
            },
        }
    }
}

#[derive(Debug)]
pub struct IdentifierAndType {
    pub identifier: Symbol,
    pub data_type: Type
}

#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub enum BinaryOperation {
    Add, Subtract, Multiply, Divide, Modulo,
    EqualTo, NotEqualTo, LessEqualTo, GreaterEqualTo, LessThan, GreaterThan,
}

impl BinaryOperation {
    fn from_lexeme(lexeme: &Lexeme) -> Result<Self, Error<ParserError>> {
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
                kind: ParserError::SyntaxError {
                    expected: String::from("binary operator"),
                    found: format!("`{}`", lexeme.kind.to_string())
                },
                // message: format!("expected binary operator, found `{}`", lexeme.kind.to_string()),
                lines: vec![lexeme.line]
            })   
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Symbol),
    StringLiteral(String),
    IntegerLiteral(i32),
    FloatLiteral(f32),
    True,
    False,
    
    Binary {
        operation: BinaryOperation,
        operand_1: Box<ASTNode<Expression>>,
        operand_2: Box<ASTNode<Expression>>,
    },

    ProcedureDefinition {
        args: Vec<IdentifierAndType>,
        body: Box<ASTNode<Statement>>
    },

    FunctionDefinition {
        args: Vec<IdentifierAndType>,
        return_type: Type,
        body: Box<ASTNode<Statement>>
    },

    FunctionCall {
        identifier: Symbol,
        args: Vec<ASTNode<Expression>>
    },
}

#[derive(Debug)]
pub enum Statement {
    Compound(Vec<ASTNode<Statement>>),
    Return(Option<ASTNode<Expression>>),
    
    DeclareConstant {
        identifier: Symbol,
        data_type: Option<Type>,
        value: ASTNode<Expression>,
    },

    DeclareVariable {
        identifier: Symbol,
        data_type: Option<Type>,
        value: Option<ASTNode<Expression>>,
    },

    Assign {
        identifier: Symbol,
        value: ASTNode<Expression>,
    },

    If {
        condition: ASTNode<Expression>,
        on_true: Box<ASTNode<Statement>>,
        on_false: Option<Box<ASTNode<Statement>>>
    },

    ProcedureCall{
        identifier: Symbol,
        args: Vec<ASTNode<Expression>>
    },
}

#[derive(Debug)]
pub struct ASTNode<T> {
    pub kind: T,
    pub lines: Vec<usize>
}

pub struct Parser {
    lexemes_iter: Box<dyn Iterator<Item = Lexeme>>,
    prev_lexeme: Lexeme,
    current_lexeme: Option<Lexeme>,
    next_lexeme: Option<Lexeme>,
    current_line: usize,
}

impl Parser {
    const TYPE_LEXEMES: [LexemeKind; 7] = [
        LexemeKind::Int,
        LexemeKind::Float,
        LexemeKind::String,
        LexemeKind::Bool,
        LexemeKind::Identifier(String::new()),
        LexemeKind::Proc,
        LexemeKind::Func,
    ];
    
    const STATEMENT_START_LEXEMES: [LexemeKind; 6] = [
        LexemeKind::LeftBrace,
        LexemeKind::Const,
        LexemeKind::Let,
        LexemeKind::Identifier(String::new()),
        LexemeKind::Return,
        LexemeKind::If,
    ];

    const EXPRESSION_START_LEXEMES: [LexemeKind; 8] = [
        // for binary operations or single values
        LexemeKind::Identifier(String::new()),
        LexemeKind::NumberLiteral(String::new()),
        LexemeKind::StringLiteral(String::new()),
        LexemeKind::True,
        LexemeKind::False,
        LexemeKind::LeftParen,
        
        
        // for function and procedure definition
        LexemeKind::Func,
        LexemeKind::Proc,

        // for compound expressions
        // LexemeKind::LeftBrace,  
    ];
    
    pub fn new(lexemes: &[Lexeme]) -> Parser {
        let mut lexemes_iter = Vec::from(lexemes).into_iter();
        let next_lexeme = lexemes_iter.next();
        let current_line = 0usize;
        
        let mut parser = Parser {
            // lexemes_iter: Box::new(Vec::from(lexemes).into_iter()),
            lexemes_iter: Box::new(lexemes_iter.clone()),
            prev_lexeme: Lexeme{kind: LexemeKind::LeftBrace, line: 1},
            current_lexeme: None,
            next_lexeme,
            current_line
        };

        parser.advance();
        parser
    }

    fn advance(&mut self) {
        if let Some(lexeme) = &self.current_lexeme {
            self.prev_lexeme = lexeme.clone();
        }
        
        self.current_lexeme = self.next_lexeme.clone();
        self.next_lexeme = self.lexemes_iter.next();

        // is there a better way to do this
        self.current_line = if self.current_lexeme.is_some() {
            self.current_lexeme.clone().unwrap().line
        } else {
            0
        };
        
        
        // dbg!(&self.current_lexeme, self.current_line);
    }

    fn expect(&self, lexeme_kinds: &[LexemeKind]) -> Result<(), Error<ParserError>> {
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
                kind: ParserError::SyntaxError {
                    expected: expected_lexemes_list_message,
                    found: String::from("nothing")
                },
                // message: format!("expected {}, found nothing", expected_lexemes_list_message),
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
                    kind: ParserError::SyntaxError {
                        expected: expected_lexemes_list_message,
                        found: format!("`{}`", lexeme.kind.to_string())
                    },
                    // message: format!("expected {}, found `{}`", "A", "A"),
                        //expected_lexemes_list_message, lexeme.kind.to_string()),
                    lines: vec![lexeme.line]
                })
            }
        }
    }


    fn expect_next(&self, lexeme_kinds: &[LexemeKind]) -> Result<(), Error<ParserError>> {
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
        
        match &self.next_lexeme {
            None | Some(Lexeme{kind: LexemeKind::RightBrace, line: 0}) => Err(Error {
                kind: ParserError::SyntaxError {
                    expected: expected_lexemes_list_message,
                    found: String::from("nothing")
                },
                // message: format!("expected {}, found nothing", expected_lexemes_list_message),
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
                    kind: ParserError::SyntaxError {
                        expected: expected_lexemes_list_message,
                        found: format!("`{}`", lexeme.kind.to_string())
                    },
                    // message: format!("expected {}, found `{}`", "A", "A"),
                        //expected_lexemes_list_message, lexeme.kind.to_string()),
                    lines: vec![lexeme.line]
                })
            }
        }
    }    
    
    pub fn parse_statement(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // println!("parse_statement");
        
        self.expect(&Self::STATEMENT_START_LEXEMES)?;
        
        match self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
                self.parse_statement_compound()
            },
        
            Some(Lexeme{kind: LexemeKind::Const | LexemeKind::Let, ..}) => {
                self.parse_statement_declare()
            },

            Some(Lexeme{kind: LexemeKind::Identifier(_), ..}) => {
                // todo: add expects for next lexeme

                self.expect_next(&[
                    LexemeKind::Equals,
                    LexemeKind::LeftParen
                ])?;

                match &self.next_lexeme {
                    Some(Lexeme{kind: LexemeKind::Equals, ..}) => self.parse_statement_assign(),
                    Some(Lexeme{kind: LexemeKind::LeftParen, ..}) => self.parse_statement_procedure_call(),
                    _ => unreachable!()
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

    fn parse_statement_procedure_call(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // procedure call grammar
        // IDENTIFIER '(' [ EXPRESSION { ',' EXPRESSION } ] ')' ';'

        // println!("procedure_call");

        let line_start = self.current_line;
        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => {
                Symbol{
                    name: identifier.to_owned(),
                    line: *line
                }
            },
            _ => unreachable!()
        };

        self.advance();
        self.expect(&[LexemeKind::LeftParen])?;
        
        self.advance();
        self.expect(&Self::EXPRESSION_START_LEXEMES).or(
            self.expect(&[LexemeKind::RightParen]) // explicitly putting this here
        )?;
        
        let mut args: Vec<ASTNode<Expression>> = Vec::new();
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
        
        Ok(ASTNode {
            kind: Statement::ProcedureCall {
                identifier,
                args  
            },
            lines: (line_start..=self.current_line).collect()
        })
    }
    fn parse_statement_if(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // if statement grammar
        // 'if' EXPRESSION STATEMENT [ 'else' STATEMENT ]
        let line_start = self.current_line;
        
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

        Ok(ASTNode{
            kind: Statement::If {
                condition,
                on_true,
                on_false
            },
            lines: (line_start..=self.current_line).collect()
        })
        
        // todo!()
    }
    
    fn parse_statement_return(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // return grammar
        // 'return' [ EXPRESSION ] ';'

        let line_start = self.current_line;
        
        self.advance();
        let expression = self.parse_expression().ok();

        if expression.is_some() {
            self.advance();
        }
                
        self.expect(&[LexemeKind::Semicolon])?;
                
        Ok(ASTNode {
            kind: Statement::Return(expression),
            lines: (line_start..=self.current_line).collect()
        })        
    }
    
    fn parse_statement_assign(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // assignment grammar
        // IDENTIFIER '=' EXPRESSION ';'

        let line_start = self.current_line;

        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => Symbol {
                name: identifier.to_owned(),
                line: *line
        },
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
    
        Ok(ASTNode {
            kind: Statement::Assign {
                identifier,
                value
            },
            lines: (line_start..=self.current_line).collect()
        })
    }
    
    fn parse_statement_compound(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> { 
        let line_start = self.current_line;
        
        let mut statements: Vec<ASTNode<Statement>> = Vec::new();

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

        Ok(ASTNode {
            kind: Statement::Compound(statements),
            lines: (line_start..=self.current_line).collect()
        })
    }

    fn parse_statement_declare(&mut self) -> Result<ASTNode<Statement>, Error<ParserError>> {
        // println!("parse_const_let");

        // constant declaration grammar
        // 'const' IDENTIFIER [ ':' TYPE ] '=' EXPRESSION ';'
    
        // variable declaration grammar
        // 'let' IDENTIFIER ( ':' TYPE ) | ( '=' EXPRESSION ) ';'
        
        let line_start = self.current_line;
        
        let is_const = self.current_lexeme.as_ref().unwrap().kind == LexemeKind::Const;
    
        self.advance();
        self.expect(&[LexemeKind::Identifier(String::new())])?;
        
        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => Symbol {
                name: identifier.to_owned(),
                line: *line
            },
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

        let statement_kind = if is_const {
            Statement::DeclareConstant {
                identifier,
                data_type,
                value: value.unwrap()
            }
        } else {
            Statement::DeclareVariable {
                identifier,
                data_type,
                value
            }
        };

        Ok(ASTNode {
            kind: statement_kind,
            lines: (line_start..=self.current_line).collect()
        })
    }

    fn parse_expression_function_definition(&mut self) -> Result<ASTNode<Expression>, Error<ParserError>> {
        let line_start = self.current_line;
        
        let is_proc = if let Some(Lexeme{kind: LexemeKind::Proc, ..}) = self.current_lexeme {
            true
        } else {
            false
        };

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

        let body = Box::new(self.parse_statement()?);

        let expression = if is_proc {
            Expression::ProcedureDefinition {
                args,
                body
            }
        } else {
            Expression::FunctionDefinition {
                args,
                return_type: return_type.unwrap(),
                body
            }
        };

        Ok(ASTNode {
            kind: expression,
            lines: (line_start..=self.current_line).collect()
        })
    }

    // fn parse_expression_compound(&mut self) -> Result<ASTNode<Expression>, Error<ParserError>> {
    //     // todo (maybe) allow implicit returns?

    //     let line_start = self.current_line;
        
    //     let mut statements: Vec<ASTNode<Statement>> = Vec::new();

    //     loop {
    //         self.advance();
    //         match &self.current_lexeme {
    //             Some(Lexeme{kind: LexemeKind::RightBrace, ..}) => {
    //                 break;
    //             },
                
    //             Some(_) => statements.push(self.parse_statement()?),
    //             None => unreachable!(),
    //         }
    //     }

    //     Ok(ASTNode {
    //         kind: Expression::Compound{
    //             statements
    //         },
    //         lines: (line_start..=self.current_line).collect()
    //     })
    // }

    fn parse_expression_math(&mut self) -> Result<ASTNode<Expression>, Error<ParserError>> {
        let line_start = self.current_line;
        let operand_1 = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => {
                if let Some(Lexeme{kind: LexemeKind::LeftParen, ..}) = self.next_lexeme {
                    self.parse_value_function_call()?
                } else {   
                    ASTNode {
                        kind: Expression::Identifier(Symbol {
                            name: identifier.to_owned(),
                            line: *line
                        }),
                        lines: (line_start..=self.current_line).collect()
                    }
                }
            },
                    
            Some(Lexeme{kind: LexemeKind::StringLiteral(string), ..}) => {
                ASTNode {
                    kind: Expression::StringLiteral(string.to_owned()),
                    lines: (line_start..=self.current_line).collect()
                }
            },
                    
            Some(Lexeme{kind: LexemeKind::NumberLiteral(number), ..}) => {
                if number.contains(".") {
                    ASTNode {
                        kind: Expression::FloatLiteral(number.parse::<f32>().unwrap()),
                        lines: (line_start..=self.current_line).collect()
                    }
                } else {
                    ASTNode {
                        kind: Expression::IntegerLiteral(number.parse::<i32>().unwrap()),
                        lines: (line_start..=self.current_line).collect()
                    }
                }
            },

            Some(Lexeme{kind: LexemeKind::True, ..}) => {
                ASTNode {
                    kind: Expression::True,
                    lines: (line_start..=self.current_line).collect()
                }
            }
            
            Some(Lexeme{kind: LexemeKind::False, ..}) => {
                ASTNode {
                    kind: Expression::False,
                    lines: (line_start..=self.current_line).collect()
                }
            }
            
            Some(Lexeme{kind: LexemeKind::LeftParen, .. }) => {
                self.advance();
                let expression = self.parse_expression()?;

                self.advance();
                self.expect(&[LexemeKind::RightParen])?;
      
                expression
            }

            _ => unreachable!()
        };

        // determining operand_2 (can be None)
        if let Ok(operation) = BinaryOperation::from_lexeme(self.next_lexeme.as_ref().unwrap()) {
            self.advance(); // skip opereation

            self.advance();
            let operand_2 = self.parse_expression()?;
            
            Ok(ASTNode {
                kind: Expression::Binary {
                    operation,
                    operand_1: Box::new(operand_1),
                    operand_2: Box::new(operand_2),
                },
                lines: (line_start..=self.current_line).collect()
            })
        } else {
            Ok(operand_1)
        }    
    }

    fn parse_value_function_call(&mut self) -> Result<ASTNode<Expression>, Error<ParserError>> {
        // function call grammar
        // IDENTIFIER '(' [ EXPRESSION { ',' EXPRESSION } ] ')' ';'

        let line_start = self.current_line;
        
        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => Symbol {
                name: identifier.to_owned(),
                line: *line
        },
            _ => unreachable!()
        };

        self.advance();
        self.expect(&[LexemeKind::LeftParen])?;
        
        self.advance();
        self.expect(&Self::EXPRESSION_START_LEXEMES).or(
            self.expect(&[LexemeKind::RightParen]) // explicitly putting this here
        )?;
        
        let mut args: Vec<ASTNode<Expression>> = Vec::new();
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
        
        Ok(ASTNode {
            kind: Expression::FunctionCall {
                identifier,
                args
            },
            lines: (line_start..=self.current_line).collect()
        })
    }
    
    fn parse_expression(&mut self) -> Result<ASTNode<Expression>, Error<ParserError>> {        
        self.expect(&Self::EXPRESSION_START_LEXEMES)?;

        match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(_) |
                 LexemeKind::NumberLiteral(_) |
                 LexemeKind::StringLiteral(_) |
                 LexemeKind::True | 
                 LexemeKind::False |
                 LexemeKind::LeftParen, ..}) => {
                self.parse_expression_math()
            },

            Some(Lexeme{kind: LexemeKind::Func | LexemeKind::Proc, ..}) => {
                self.parse_expression_function_definition()
            },

            // Some(Lexeme{kind: LexemeKind::LeftBrace, ..}) => {
            //     self.parse_expression_compound()
            // },
            
            _ => unreachable!()
        }
    }
    
    fn parse_type(&mut self) -> Result<Type, Error<ParserError>> {
        //println!("parse type\n{:#?}\n", lexemes_iter);
        
        self.expect(&Self::TYPE_LEXEMES)?;

        match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Int, ..}) => Ok(Type::Integer),
            Some(Lexeme{kind: LexemeKind::Float, ..}) => Ok(Type::Float),
            Some(Lexeme{kind: LexemeKind::String, ..}) => Ok(Type::String),
            Some(Lexeme{kind: LexemeKind::Bool, ..}) => Ok(Type::Boolean),
            
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => {
                Ok(Type::UserDefined(Symbol{
                    name: identifier.to_string(),
                    line: *line
                }))
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
                self.expect(&Self::TYPE_LEXEMES)
                    .or(self.expect(&[LexemeKind::RightParen]))?;

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

    fn parse_identifier_and_type(&mut self) -> Result<IdentifierAndType, Error<ParserError>> {
        self.expect(&[LexemeKind::Identifier(String::new())])?;

        let identifier = match &self.current_lexeme {
            Some(Lexeme{kind: LexemeKind::Identifier(identifier), line}) => Symbol{
                name: identifier.to_owned(),
                line: *line
            },
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
