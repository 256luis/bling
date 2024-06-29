use std::{collections::HashMap, iter};

use crate::{error::*, parser::*};

#[derive(Debug, Clone)]
enum SymbolKind {
    Variable(Type),
    Constant(Type),
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    kind: SymbolKind,
    line: usize,
}

#[derive(Debug)]
pub struct Analyzer {
    symbol_table: HashMap<String, SymbolInfo>,
    symbol_stack: Vec<String>,
    pub errors: Vec<Error<SemanticError>>
}

macro_rules! throw_error {
    ($self:expr, $error:expr) => {
        $self.errors.push($error);
        return Err(());
    };
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            symbol_stack: Vec::new(),
            errors: Vec::new(),
        }
    }
    
    pub fn analyze(&mut self, ast: &mut ASTNode<Statement>) -> Result<(), Vec<Error<SemanticError>>> {
        _ = self.check_statement(ast, None);

        for error in self.errors.iter_mut() {
            error.lines.sort();
            error.lines.dedup();
        }
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    // the `expected_type` param is only for checking return types on return statements
    fn check_statement(&mut self, statement_node: &mut ASTNode<Statement>, expected_return_type: Option<Type>) -> Result<(), ()> {
        match &mut statement_node.kind {
            // TODO: check possible return types of compound statements
            Statement::Compound(statements) => {
                let error_count_before_check = self.errors.len();
                let symbol_count_before = self.symbol_stack.len();
                
                for s in statements {
                    _ = self.check_statement(s, expected_return_type.clone());
                }

                let symbol_count_after = self.symbol_stack.len();
                let difference = symbol_count_after - symbol_count_before;
                
                for _ in 0..difference {
                    let top = self.symbol_stack.pop().unwrap();
                    self.symbol_table.remove(&top);
                }
                
                if self.errors.len() > error_count_before_check {
                    Err(())
                } else {
                    Ok(())
                }
            },
            
            Statement::DeclareVariable{ identifier, data_type, value} => {
                self.check_declaration_variable(identifier, data_type, value, &statement_node.lines)
            },
            
            Statement::DeclareConstant{ identifier, data_type, value} => {
                self.check_declaration_constant(identifier, data_type, value, &statement_node.lines)
            },

            Statement::Return(expression) => {
                let found_return_type = if let Some(e) = expression {
                    Some(self.check_expression_and_infer_type(e)?)
                } else {
                    None
                };

                if found_return_type != expected_return_type {
                    let found_return_type_string = if let Some(t) = found_return_type {
                        t.to_string()
                    } else {
                        String::from("nothing")
                    };

                    let expected_return_type_string = if let Some(t) = expected_return_type {
                        t.to_string()
                    } else {
                        String::from("nothing")
                    };

                    throw_error!(self, Error {
                        kind: SemanticError::TypeMismatch {
                            expected: expected_return_type_string,
                            found: found_return_type_string,
                        },
                        lines: statement_node.lines.clone()
                    });
                }
                
                Ok(())
            }

            Statement::Assign { identifier, value } => {
                let existing = self.symbol_must_be_existing(&identifier)?;
                let found_type = self.check_expression_and_infer_type(value)?;

                match existing.kind {
                    SymbolKind::Variable(expected_type) => {
                        if found_type != expected_type {
                            throw_error!(self, Error {
                                kind: SemanticError::TypeMismatch {
                                    expected: expected_type.to_string(),
                                    found: found_type.to_string(),
                                },
                                lines: value.lines.clone()
                            });
                        }
                    },

                    SymbolKind::Constant(_) => {
                        throw_error!(self, Error {
                            kind: SemanticError::ConstantReassignment {
                                symbol_name: identifier.name.clone()
                            },
                            lines: statement_node.lines.clone()
                        });
                    },
                }

                Ok(())
            },
            
            Statement::If { condition, on_true, on_false } => {
                // condition expression must evaluate to bool
                let condition_type = self.check_expression_and_infer_type(condition)?;
                if condition_type != Type::Boolean {
                    throw_error!(self, Error {
                        kind: SemanticError::TypeMismatch {
                            expected: Type::Boolean.to_string(),
                            found: condition_type.to_string()
                        },
                        lines: condition.lines.clone()
                    });
                }

                self.check_statement(on_true, None)?;
                if let Some(statement) = on_false {
                    self.check_statement(statement, None)?;
                }

                Ok(())
            },
            
            Statement::ProcedureCall { identifier, args } => {
                let procedure_symbol = self.symbol_must_be_existing(&identifier)?;

                match procedure_symbol.kind {
                    SymbolKind::Variable(Type::Procedure { param_types }) |
                    SymbolKind::Constant(Type::Procedure { param_types}) => {
                        // check arg count
                        if args.len() != param_types.len() {
                            let args_lines: Vec<usize> = args
                                .iter()
                                .map(|x| x.lines.clone())
                                .flatten()
                                .collect();

                            throw_error!(self, Error {
                                kind: SemanticError::InvalidArgumentCount {
                                    expected: param_types.len(),
                                    found: args.len()
                                },
                                lines: args_lines
                            });
                        }
                        
                        // check if given arg types match param types in function definition 
                        let mut args_types: Vec<Type> = Vec::new();
                        for arg in args.iter_mut() {
                            args_types.push(self.check_expression_and_infer_type(arg)?);
                        }
                        
                        let expected_found_pairs = iter::zip(param_types, args_types);
                        for (i, pair) in expected_found_pairs.enumerate() {
                            // 0 = expected
                            // 1 = found

                            if pair.0 != pair.1 {
                                throw_error!(self, Error {
                                    kind: SemanticError::TypeMismatch {
                                        expected: pair.0.to_string(),
                                        found: pair.1.to_string(),
                                    },
                                    lines: args[i].lines.clone()
                                });
                            }
                        }

                        Ok(())
                    },

                    _ => {
                        throw_error!(self, Error {
                            kind: SemanticError::NotProcedure {
                                symbol_name: identifier.name.clone()
                            },
                            lines: vec![identifier.line.clone()]
                        });
                    }
                }
            },
        }
    }

    // assumes that statement is either declare constant or declare variable
    fn check_declaration_variable(&mut self, identifier: &mut Symbol, data_type: &mut Option<Type>, value: &mut Option<ASTNode<Expression>>, statement_lines: &[usize]) -> Result<(), ()> {
        self.symbol_must_be_new(identifier)?;

        // get type of value
        let found_data_type = if let Some(expression_node) = value {
            Some(self.check_expression_and_infer_type(expression_node)?)
        } else {
            None
        };

        // if data type is explicit, check if found data type is same with explicit data type
        *data_type = match (&found_data_type, &data_type) {
            (Some(found), Some(expected)) => {
                self.check_type(expected)?;
                                    
                if found != expected {
                    throw_error!(self, Error {
                        kind: SemanticError::TypeMismatch {
                            expected: expected.to_string(),
                            found: found.to_string(),
                        },
                        lines: statement_lines.to_vec()
                    });
                }

                Some(found.clone())
            },

            (Some(found), None) => Some(found.clone()),
            (None, Some(explicit)) => Some(explicit.clone()),

            // this case is unreachable because the grammar enforces it
            // if this case gets reached, its a parser bug
            (None, None) => unreachable!()
        };
        
        self.push_symbol(
            identifier.name.clone(),
            SymbolInfo {
                kind: SymbolKind::Variable(data_type.clone().unwrap()),
                line: identifier.line
            }        
        );
        
        Ok(())
    }

    fn check_declaration_constant(&mut self, identifier: &mut Symbol, data_type: &mut Option<Type>, value: &mut ASTNode<Expression>, statement_lines: &[usize]) -> Result<(), ()> {
        self.symbol_must_be_new(identifier)?;
                            
        let found_data_type = self.check_expression_and_infer_type(value)?;

        // if data type is explicit, check if found data type is same with explicit data type
        *data_type = if let Some(t) = data_type {
            self.check_type(t)?;

            if &found_data_type != t {
                throw_error!(self, Error {
                    kind: SemanticError::TypeMismatch {
                        expected: t.to_string(),
                        found: found_data_type.to_string(),
                    },
                    lines: statement_lines.to_vec()
                });
            }

            Some(found_data_type)
        } else {
            Some(found_data_type)
        };

        self.push_symbol(
            identifier.name.clone(),
            SymbolInfo{
                kind: SymbolKind::Constant(data_type.clone().unwrap()),
                line: identifier.line
            }
        );
        
        Ok(())
    }
    
    fn check_expression_and_infer_type(&mut self, expression_node: &mut ASTNode<Expression>) -> Result<Type, ()> {
        fn is_operation_allowed(operation: &BinaryOperation, type_1: &Type, type_2: &Type) -> bool {
            match operation {
                BinaryOperation::Add |
                BinaryOperation::Subtract |
                BinaryOperation::Multiply |
                BinaryOperation::Divide |
                BinaryOperation::LessEqualTo |
                BinaryOperation::GreaterEqualTo |
                BinaryOperation::LessThan |
                BinaryOperation::GreaterThan => {
                    match (type_1, type_2) {
                        (Type::Integer | Type::Float, Type::Integer | Type::Float) => true,
                        _ => false
                    }
                },

                BinaryOperation::EqualTo | BinaryOperation::NotEqualTo => {
                    match (type_1, type_2) {
                        (Type::Integer | Type::Float, Type::Integer | Type::Float) => true,
                        (Type::String, Type::String) => true,
                        (Type::Boolean, Type::Boolean) => true,
                        _ => false
                    }
                },
                
                BinaryOperation::Modulo => {
                    match (type_1, type_2) {
                        (Type::Integer, Type::Integer) => true,
                        _ => false
                    }
                }
            }
        }

        match &mut expression_node.kind {
            Expression::StringLiteral(_) => Ok(Type::String),
            Expression::IntegerLiteral(_) => Ok(Type::Integer),
            Expression::FloatLiteral(_) => Ok(Type::Float),
            Expression::True | Expression::False => Ok(Type::Boolean),
            
            Expression::Identifier(identifier) => {
                self.symbol_must_be_existing(identifier)?;

                match &self.symbol_table.get(&identifier.name).unwrap().kind {
                    SymbolKind::Variable(t) | SymbolKind::Constant(t) => Ok(t.clone())
                }
            },

            Expression::Binary { operation, operand_1, operand_2 } => {
                let type_1 = self.check_expression_and_infer_type(operand_1)?;
                let type_2 = self.check_expression_and_infer_type(operand_2)?;

                if is_operation_allowed(operation, &type_1, &type_2) {
                    // operation tells us if type is bool or float/int
                    match operation {
                        BinaryOperation::EqualTo |
                        BinaryOperation::NotEqualTo |
                        BinaryOperation::LessEqualTo |
                        BinaryOperation::GreaterEqualTo |
                        BinaryOperation::LessThan |
                        BinaryOperation::GreaterThan => Ok(Type::Boolean),

                        BinaryOperation::Add |
                        BinaryOperation::Subtract |
                        BinaryOperation::Multiply |
                        BinaryOperation::Divide |
                        BinaryOperation::Modulo => {
                            // if types of both operands are the same, return that type
                            if type_1 == type_2 {
                                return Ok(type_1);
                            }

                            // else, must be int/float, return float
                            match (type_1, type_2) {
                                (Type::Integer | Type::Float, Type::Integer | Type::Float) => Ok(Type::Float),

                                // if types or not int/float, programmer error
                                _ => unreachable!()
                            }
                        }
                    }

                } else {
                    throw_error!(self, Error {
                        kind: SemanticError::InvalidOperation {
                            operation: operation.clone(),
                            data_types: [type_1, type_2]
                        },
                        lines: expression_node.lines.clone()
                    });
                }
            }
            
            Expression::ProcedureDefinition { args, body } => {
                // check arg names and types
                for arg in args.iter() {
                    self.symbol_must_be_new(&arg.identifier)?;
                    self.check_type(&arg.data_type)?;

                    self.push_symbol(
                        arg.identifier.name.clone(),
                        SymbolInfo {
                            kind: SymbolKind::Variable(arg.data_type.clone()),
                            line: arg.identifier.line
                        }
                    );
                }
                
                // check procedure body
                // TODO: type check return values (must be none)
                self.check_statement(body, None)?;
                
                for _ in 0..args.len() {
                    let top = self.symbol_stack.pop().unwrap();
                    self.symbol_table.remove(&top);
                }
                
                let param_types: Vec<Type> = args.iter()
                    .map(|x| x.data_type.clone())
                    .collect();

                Ok(Type::Procedure {
                    param_types
                })
            },
            
            Expression::FunctionDefinition { args, body, return_type } => {
                // check arg names and types
                for arg in args.iter() {
                    self.symbol_must_be_new(&arg.identifier)?;
                    self.check_type(&arg.data_type)?;

                    self.push_symbol(
                        arg.identifier.name.clone(),
                        SymbolInfo {
                            kind: SymbolKind::Variable(arg.data_type.clone()),
                            line: arg.identifier.line
                        }
                    );
                }

                // check return type
                self.check_type(return_type)?;

                // check body
                // TODO: type check return values
                self.check_statement(body, Some(return_type.clone()))?;

                for _ in 0..args.len() {
                    let top = self.symbol_stack.pop().unwrap();
                    self.symbol_table.remove(&top);
                }
                
                let param_types: Vec<Type> = args.iter()
                    .map(|x| x.data_type.clone())
                    .collect();

                Ok(Type::Function {
                    param_types,
                    return_type: Box::new(return_type.clone())
                })
            },

            Expression::FunctionCall { identifier, args } => {
                let function_symbol = self.symbol_must_be_existing(&identifier)?;
                match function_symbol.kind {
                    SymbolKind::Variable(Type::Function { return_type, param_types }) |
                    SymbolKind::Constant(Type::Function { return_type, param_types}) => {
                        // check arg count
                        if args.len() != param_types.len() {
                            let args_lines: Vec<usize> = args
                                .iter()
                                .map(|x| x.lines.clone())
                                .flatten()
                                .collect();

                            throw_error!(self, Error {
                                kind: SemanticError::InvalidArgumentCount {
                                    expected: param_types.len(),
                                    found: args.len()
                                },
                                lines: args_lines
                            });
                        }

                        // check if given arg types match param types in function definition 
                        let mut args_types: Vec<Type> = Vec::new();
                        for arg in args.iter_mut() {
                            args_types.push(self.check_expression_and_infer_type(arg)?);
                        }
                        
                        let expected_found_pairs = iter::zip(param_types, args_types);
                        for (i, pair) in expected_found_pairs.enumerate() {
                            // 0 = expected
                            // 1 = found

                            if pair.0 != pair.1 {
                                throw_error!(self, Error {
                                    kind: SemanticError::TypeMismatch {
                                        expected: pair.0.to_string(),
                                        found: pair.1.to_string(),
                                    },
                                    lines: args[i].lines.clone()
                                });
                            }
                        }

                        Ok(*return_type)
                    },

                    _ => {
                        throw_error!(self, Error {
                            kind: SemanticError::NotFunction {
                                symbol_name: identifier.name.clone()
                            },
                            lines: vec![identifier.line.clone()]
                        });
                    }
                }
            }
        }
    }

    fn check_type(&mut self, data_type: &Type) -> Result<(), ()> {
        match data_type {
            Type::UserDefined(symbol) => {
                self.symbol_must_be_existing(symbol)?;
                Ok(())
            },
            
            Type::Procedure { param_types } => {
                for t in param_types {
                    _ = self.check_type(t)?;
                }
                Ok(())
            },

            Type::Function { param_types, return_type } => {
                for t in param_types {
                    _ = self.check_type(t)?;
                }

                self.check_type(return_type)?;

                Ok(())
            },

            _ => Ok(())
        }
    }
    
    fn symbol_must_be_new(&mut self, symbol: &Symbol) -> Result<(), ()> {
        if !self.symbol_table.contains_key(&symbol.name) {
            return Ok(());
        }

        let existing_symbol = self.symbol_table.get(&symbol.name).unwrap();

        throw_error!(self, Error {
            kind: SemanticError::RedeclaredSymbol {
                symbol_name: symbol.name.to_owned()
            },
            lines: vec![existing_symbol.line, symbol.line]
        });
    }

    fn symbol_must_be_existing(&mut self, symbol: &Symbol) -> Result<SymbolInfo, ()> {
        if let Some(symbol_info) = self.symbol_table.get(&symbol.name) {
            return Ok(symbol_info.clone());
        }

        throw_error!(self, Error {
            kind: SemanticError::UndeclaredSymbol {
                symbol_name: symbol.name.to_owned()
            },
            lines: vec![symbol.line]
        });
    }

    fn push_symbol(&mut self, name: String, symbol_info: SymbolInfo) {
        self.symbol_table.insert(name.clone(), symbol_info);
        self.symbol_stack.push(name);
    }
}
