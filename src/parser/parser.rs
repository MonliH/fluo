use crate::parser::ast;
use crate::lexer;
use crate::logger::logger::{Error, ErrorType, Logger, ErrorDisplayType, ErrorAnnotation};
use crate::parser::ast::{ Statement, Expr, Scope };
use crate::helpers;
use crate::codegen::module_codegen::CodeGenModule;
use crate::parser:: { custom_syntax, custom_syntax::Impl };
use crate::parser::gen_utils as g;
use crate::parser::std::generate;
use std::collections::HashMap;

/// Recursive descent parser
pub struct Parser<'a> {
    /// Lexer object
    pub lexer: lexer::Lexer,
    /// Abstract syntax tree
    pub ast: Option<ast::Block>,
    pub modules: Vec<CodeGenModule<'a>>,
    statements: Vec<fn (&'a mut Self) -> Result<Statement, Error>>,
    customs: custom_syntax::NamespaceObj<'a>
}

impl<'a> Parser<'a> {
    /// Return a new parser object.
    /// 
    /// Arguments
    /// 
    /// * `l`: lexer to use
    /// * `log`: logger to use
    pub fn new(l: lexer::Lexer) -> Parser<'a> {
        Parser { 
            lexer: l, 
            ast: None, 
            modules: Vec::new(), 
            statements: vec![Parser::parse_impl, Parser::function_define, Parser::expression_statement, Parser::variable_declaration],
            customs: generate::generate_std_lib()
        }
    }
    
    /// Template for syntax error
    pub fn syntax_error(&'a self, t: lexer::Token, message: &str, is_keyword: bool, urgent: bool) -> Error {
        Error::new(
            String::from(
                if !is_keyword { 
                    format!("{}, found {}", message, t) 
                } else {  
                    format!("unexpected {}", t) 
                }
            ),
            ErrorType::Syntax, 
            t.pos,
            ErrorDisplayType::Error,
            self.lexer.filename.clone(),
            vec![
                ErrorAnnotation::new(Some("unexpected token".to_string()), t.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
            ],
            urgent
        )
    }

    /// Validate next token
    pub fn next(&'a mut self, token_type: lexer::TokenType, position: (usize, usize), is_keyword: bool) -> Result<(), Error> {
        let t = self.lexer.advance()?.clone();
        
        if t.token != token_type {
            self.lexer.set_pos(position);
            Err(self.syntax_error(t, &format!("expected {}", token_type)[..], is_keyword, false))
        } else {
            Ok(())
        }
    }

    pub fn position(&'a mut self, position: (usize, usize)) -> helpers::Pos {
        helpers::Pos {
            s: position.0,
            e: self.lexer.position
        }
    }

    /// Parse from lexer
    /// 
    /// Returns nothing
    pub fn parse(&'a mut self) -> Result<(), Vec<Error>> {
        let position = self.lexer.get_pos();

        // Set our scope to outside
        let scope = Scope::Outer;

        let mut ast_list: Vec<Statement> = Vec::new();
        let next = self.lexer.peek();
        match next {
            Err(e) => Err(vec![e]),
            Ok(next) => {
                if next.token != lexer::TokenType::EOF {
                    loop {
                        let mut errors: Vec<Error> = Vec::new();
                        let mut fail = true;
                        for i in 0..self.statements.len() {
                            let statement_ast = self.statements[i](self);
                            match statement_ast {
                                Ok(ast_production) => {
                                    if ast_production.in_scope(&scope) {
                                        fail = false; 
                                        errors.clear();
                                        ast_list.push(ast_production);
                                        break
                                    } else {
                                        break
                                    }
                                },
                                Err(e) => errors.push(e)
                            }
                        }
                        
                        let temp_peek = self.lexer.peek();
                        match temp_peek {
                            Err(e) => { return Err(vec![e]); },
                            Ok(temp_peek) => {
                                if temp_peek.token == lexer::TokenType::EOF && !fail{
                                    // We've successfully parsed, break
                                    break
                                } else if !errors.is_empty() && fail {
                                    // We've found an error, raise the error
                                    return Err(vec![Logger::longest(errors)]);
                                }
                            }
                        }
                    }
                }
                let block = ast::Block { nodes: ast_list, pos: self.position(position) };
                self.ast = Some(block);
                Ok(())
            }
        }
    }

    /// Parse basic block
    pub fn block(&'a mut self) -> Result<ast::Block, Error> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::LCP, position, false)?;

        // Set our scope to inside a block
        let scope = Scope::Block;

        let mut ast_list: Vec<Statement> = Vec::new();
        loop {
            let mut errors: Vec<Error> = Vec::new();
            let mut fail = true;
            for i in 0..self.statements.len() {
                let statement_ast = self.statements[i](self);
                match statement_ast {
                    Ok(ast_production) => {
                        if ast_production.in_scope(&scope) {
                            fail = false; 
                            errors.clear();
                            ast_list.push(ast_production);
                            break
                        } else {
                            break
                        }
                    },
                    Err(e) => { errors.push(e); }
                }
            }

            if self.lexer.peek()?.token == lexer::TokenType::RCP && !fail {
                // We've successfully parsed, break
                break
            } else if !errors.is_empty() && fail {
                // We've found an error, raise the error
                return Err(Logger::longest(errors));
            } else if self.lexer.peek()?.token != lexer::TokenType::RCP && fail {
                // We've forgotten closing brace
                break
            }
        }

        // Check for closing brace here
        self.next(lexer::TokenType::RCP, position, false)?;

        Ok( ast::Block {
            nodes: ast_list,
            pos: self.position(position)
        } )
    }

    
    pub fn eval_impl(&'a mut self, impl_block: custom_syntax::Impl) -> Result<(), Error> {
        let impl_final = self.get_metasyntax(&impl_block.metasyntax)?(self, &impl_block)?;
        self.set_namespace_obj(impl_final.name.clone(), custom_syntax::NamespaceObj::Syntax(impl_final))?;
        Ok(())
    }

    pub fn parse_impl(&'a mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();

        self.next(lexer::TokenType::IMPL, position, true)?;

        let name = self.name_id()?;

        self.next(lexer::TokenType::ARROW, position, false)?;

        let impl_type = self.namespace()?;

        self.next(lexer::TokenType::LCP, position, false)?;

        let mut patterns: HashMap<String, custom_syntax::Custom> = HashMap::new();

        loop {
            if self.lexer.peek()?.token == lexer::TokenType::RCP {
                // We've parsed to the end, break
                break
            }

            let pattern = Parser::parse_pattern(self)?;

            // TODO: fix this :)
            patterns.insert(pattern.name.scopes.last().unwrap().value.clone(), pattern);
        }

        self.next(lexer::TokenType::RCP, position, false)?;

        let impl_node = custom_syntax::Impl {
            patterns,
            name: name,
            pos: (self.position(position)).clone(),
            metasyntax: impl_type,
            operations: None
        };

        let temp_pos = impl_node.pos;

        self.eval_impl(impl_node)?;

        Ok(ast::Statement::Empty(ast::Empty { pos: temp_pos }))
    }

    pub fn set_namespace_obj(&'a mut self, name: ast::NameID, value: custom_syntax::NamespaceObj<'a>) -> Result<(), Error> {
        if let custom_syntax::NamespaceObj::ParserNamespace(mut objects) = &self.customs {
            if objects.contains_key(&name) {
                return Err( Error::new (
                    format!("syntax or namespace `{}` already exists", name.value),
                    ErrorType::UndefinedSyntax,
                    name.pos,
                    ErrorDisplayType::Error,
                    self.lexer.filename.clone(),
                    vec![
                        ErrorAnnotation::new(Some(format!("`{}` already exists in this scope", name.value)), name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                    ],
                    true  // High priority error
                ) )
            } else {
                objects.insert(name, value);
            }
        }
        
        Ok(())
    }

    pub fn get_namespace_obj(&'a self, namespace: &'a ast::Namespace) -> Result<(&custom_syntax::NamespaceObj<'a>, &'a ast::NameID), Error> {
        // Get a certain grammar from namespace
        let mut prev_name: Option<&ast::NameID> = None;
        let mut prev_namespace = &self.customs;

        if namespace.scopes.len() > 1 {
            for name in &namespace.scopes {
                if let custom_syntax::NamespaceObj::ParserNamespace(objects) = prev_namespace {
                    if objects.contains_key(&name) {
                        prev_name = Some(name);
                        prev_namespace = &objects[&name];
                    } else {
                        // Namespace doesn't exit in namespace
                        return Err( Error::new (
                            format!("namespace `{}` has no member `{}`", prev_name.unwrap().value, name.value),
                            ErrorType::UndefinedSyntax,
                            name.pos,
                            ErrorDisplayType::Error,
                            self.lexer.filename.clone(),
                            vec![
                                ErrorAnnotation::new(Some(format!("not a pattern in `{}`", prev_name.unwrap().value)), name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                            ],
                            true  // High priority error
                        ) );
                    }
                } else {
                    // Namespace doesn't exist because it's a syntax
                    return Err( Error::new (
                        format!("syntax `{}` has no member `{}`", prev_name.unwrap().value, name.value),
                        ErrorType::UndefinedSyntax,
                        name.pos,
                        ErrorDisplayType::Error,
                        self.lexer.filename.clone(),
                        vec![
                            ErrorAnnotation::new(Some(format!("not a pattern syntax in `{}`", prev_name.unwrap().value)), name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                        ],
                        true  // High priority error
                    ) );
                }
            };
        } else {
            // Namespace is empty
            return Err( Error::new (
                format!("namespace is empty"),
                ErrorType::UndefinedSyntax,
                namespace.pos,
                ErrorDisplayType::Error,
                self.lexer.filename.clone(),
                vec![
                    ErrorAnnotation::new(Some(format!("not a pattern syntax in `{}`", prev_name.unwrap().value)), namespace.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                ],
                true  // High priority error
            ) );
        }

        Ok((prev_namespace, prev_name.unwrap()))
    }

    pub fn get_syntax(&'a self, namespace: &'a ast::Namespace) -> Result<&'a custom_syntax::Impl, Error> {
        let (prev_namespace, prev_name) = self.get_namespace_obj(namespace)?;

        match prev_namespace {
            custom_syntax::NamespaceObj::ParserNamespace(_) => {
                // This is a namespace, not a syntax
                Err(Error::new (
                    format!("expected syntax, found namespace"),
                    ErrorType::UndefinedSyntax,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    self.lexer.filename.clone(),
                    vec![
                        ErrorAnnotation::new(Some(format!("`{}` is a namespace, not a syntax", prev_name.value)), prev_name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                    ],
                    true  // High priority error
                ))
            },
            custom_syntax::NamespaceObj::Metasyntax(_) => {
                // This is a namespace, not a syntax
                Err(Error::new (
                    format!("expected syntax, found metasyntax"),
                    ErrorType::UndefinedSyntax,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    self.lexer.filename.clone(),
                    vec![
                        ErrorAnnotation::new(Some(format!("`{}` is a metasyntax, not a syntax", prev_name.value)), prev_name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                    ],
                    true  // High priority error
                ))
            },
            custom_syntax::NamespaceObj::Syntax(impl_rule) => Ok(impl_rule)
        }
    }

    pub fn get_metasyntax(&'a self, namespace: &'a ast::Namespace) -> Result<&fn (&mut Parser<'a>, &'a Impl) -> Result<&'a Impl, Error>, Error> {
        let (prev_namespace, prev_name) = self.get_namespace_obj(namespace)?;

        match prev_namespace {
            custom_syntax::NamespaceObj::ParserNamespace(_) => {
                // This is a namespace, not a syntax
                Err(Error::new (
                    format!("expected meta syntax, found namespace"),
                    ErrorType::UndefinedSyntax,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    self.lexer.filename.clone(),
                    vec![
                        ErrorAnnotation::new(Some(format!("`{}` is a namespace, not a meta syntax", prev_name.value)), prev_name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                    ],
                    true  // High priority error
                ))
            },
            custom_syntax::NamespaceObj::Syntax(_) => {
                // This is a syntax, not a metasyntax
                Err(Error::new (
                    format!("expected metasyntax, found syntax"),
                    ErrorType::UndefinedSyntax,
                    namespace.pos,
                    ErrorDisplayType::Error,
                    self.lexer.filename.clone(),
                    vec![
                        ErrorAnnotation::new(Some(format!("`{}` is a syntax, not a meta syntax", prev_name.value)), prev_name.pos, ErrorDisplayType::Error, self.lexer.filename.clone())
                    ],
                    true  // High priority error
                ))
            },
            custom_syntax::NamespaceObj::Metasyntax(rule) => Ok(rule)
        }
    }

    pub fn parse_non_terminal(&'a mut self) -> Result<custom_syntax::NonTerminal, Error> {
        // Something that looks like this:
        // `$my_non_terminal: syntax::expr`
        let position = self.lexer.get_pos();

        let name = self.dollar_id()?;

        self.next(lexer::TokenType::COLON, position, false)?;

        let namespace = self.namespace()?;

        Ok(custom_syntax::NonTerminal { name, grammar: g::new_production_grammar_expr(g::new_namespace_production(namespace)), pos: self.position(position) })
    }
    
    pub fn parse_terminal(&'a mut self) -> Result<custom_syntax::Terminal, Error> {
        // String literals
        let position = self.lexer.get_pos();

        let literal = self.string_literal()?;

        Ok(custom_syntax::Terminal { contents: literal, pos: self.position(position) })
    }

    pub fn parse_arguments(&'a mut self) -> Result<ast::Arguments, Error> {
        let position = self.lexer.get_pos();
        let mut positional_args: Vec<(ast::NameID, ast::Type)> = Vec::new();

        loop {
            if self.lexer.peek()?.token == lexer::TokenType::RP {
                // No error, we've reached the end
                break
            }

            let id = self.name_id()?;

            self.next(lexer::TokenType::COLON, position, false)?;

            let arg_type = self.type_expr()?;

            positional_args.push((id, arg_type));
            
            if self.lexer.peek()?.token == lexer::TokenType::COMMA {
                self.lexer.advance()?;
            } else {
                break
            }
        }

        Ok(ast::Arguments { 
            positional: positional_args,
            pos: self.position(position)
        })
    }

    /// Parse function definition
    pub fn function_define(&'a mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();
        
        self.next(lexer::TokenType::DEF, position, true)?;

        let id = self.name_id()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.parse_arguments()?;

        self.next(lexer::TokenType::RP, position, false)?;

        let mut return_type = ast::Type {
            value: ast::TypeType::Tuple(Vec::new()),
            pos: helpers::Pos {
                s: self.lexer.position,
                e: self.lexer.position,
            }
        };

        if self.lexer.peek()?.token == lexer::TokenType::ARROW {
            self.lexer.advance()?;
            return_type = self.type_expr()?;
        }

        let block = self.block()?;
        
        Ok(ast::Statement::FunctionDefine(ast::FunctionDefine {
            return_type,
            arguments,
            block,
            name: id,
            pos: self.position(position)
        }))
    }

    /// Expressions statement
    pub fn expression_statement(&'a mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();

        let expr = self.expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::ExpressionStatement(ast::ExpressionStatement {
            expression: Box::new(expr),
            pos: self.position(position)
        }))
    }

    pub fn arguments_call(&'a mut self) -> Result<ast::ArgumentsRun, Error> {
        let position = self.lexer.get_pos();
        let mut positional_args: Vec<Expr> = Vec::new();

        loop {
            if self.lexer.peek()?.token == lexer::TokenType::RP {
                // No error, we've reached the end
                break
            }

            let expr = self.expr()?;

            positional_args.push(expr);
            
            if self.lexer.peek()?.token == lexer::TokenType::COMMA {
                self.lexer.advance()?;
            } else {
                break
            }
        }

        Ok(ast::ArgumentsRun {
            positional: positional_args,
            pos: self.position(position)
        })
    }

    pub fn function_call(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::LP, position, false)?;

        let arguments = self.arguments_call()?;

        self.next(lexer::TokenType::RP, position, false)?;

        Ok(ast::Expr::FunctionCall(ast::FunctionCall {
            arguments,
            name: namespace,
            pos: self.position(position)
        }))
    }

    /// Ful variable assign with type declaration and expression
    pub fn variable_assign_full(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        
        let expr = self.expr()?;

        Ok(ast::Expr::VariableAssignDeclaration(ast::VariableAssignDeclaration {
            t: var_type,
            name: namespace,
            expr: Box::new(expr),
            pos: self.position(position)
        }))
    }

    /// Variable Declaration
    pub fn variable_declaration(&'a mut self) -> Result<Statement, Error> {
        let position = self.lexer.get_pos();
        self.next(lexer::TokenType::LET, position, true)?;

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::COLON, position, false)?;
        
        let var_type = self.type_expr()?;

        self.next(lexer::TokenType::SEMI, position, false)?;

        Ok(ast::Statement::VariableDeclaration(ast::VariableDeclaration {
            t: var_type,
            name: namespace,
            pos: self.position(position)
        }))
    }

    /// Variable assign with only expression
    pub fn variable_assign(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let namespace = self.namespace()?;

        self.next(lexer::TokenType::EQUALS, position, false)?;
        
        let expr = self.expr()?;

        Ok(ast::Expr::VariableAssign(ast::VariableAssign {
            name: namespace,
            expr: Box::new(expr),
            pos: self.position(position)
        }))
    }

    /// Top level expression
    pub fn expr(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let mut left = self.term()?;

        loop {
            match self.lexer.peek()?.token {
                lexer::TokenType::ADD  => { 
                    self.lexer.advance()?;
                    let right = self.term()?;

                    left = ast::Expr::Add(ast::Add {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: self.position(position)
                    });
                },

                lexer::TokenType::SUB => {
                    self.lexer.advance()?;
                    let right = self.term()?;
                    
                    left = ast::Expr::Sub(ast::Sub {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: self.position(position)
                    });
                },

                _  => { return Ok(left); }
            }
        }
    }

    pub fn term(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        
        let mut left = self.factor()?;
        
        loop {
            match self.lexer.peek()?.token {
                lexer::TokenType::MUL  => { 
                    self.lexer.advance()?;
                    let right = self.factor()?;
                    
                    left = ast::Expr::Mul(ast::Mul {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: self.position(position)
                    });
                },

                lexer::TokenType::DIV => {
                    self.lexer.advance()?;
                    let right = self.factor()?;
                    
                    left = ast::Expr::Div(ast::Div {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: self.position(position)
                    });
                },

                lexer::TokenType::MOD  => {
                    self.lexer.advance()?;
                    let right = self.factor()?;
                    
                    left = ast::Expr::Mod(ast::Mod {
                        left: Box::new(left),
                        right: Box::new(right),
                        pos: self.position(position)
                    });
                },

                _  => { return Ok(left); }
            }
        }
    }

    pub fn factor(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        
        match self.next(lexer::TokenType::SUB, position, false) {
            Ok(_) => {
                match self.item() {
                    Ok(item) => Ok(ast::Expr::Neg(
                        ast::Neg { 
                            value: Box::new(item), 
                            pos: self.position(position)
                        }
                    )),
                    Err(e) => {
                        self.lexer.set_pos(position);
                        Err(e)
                    }
                }
            },
            Err(_) => {
                match self.item() {
                    Ok(item) => Ok(item),
                    Err(e) => {
                        self.lexer.set_pos(position);
                        Err(e)
                    }
                }
            }
        }
    }

    pub fn item(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();
        let mut errors: Vec<Error> = Vec::new();

        match self.integer() {
            Ok(ast) => return Ok(ast),
            Err(e) => errors.push(e)
        }

        match self.dollar_id() {
            Ok(ast) => return Ok(Expr::DollarID(ast)),
            Err(e) => errors.push(e)
        }

        match self.function_call() {
            Ok(ast) => return Ok(ast),
            Err(e) => errors.push(e)
        }

        match self.variable_assign() {
            Ok(ast) => return Ok(ast),
            Err(e) => errors.push(e)
        }

        match self.variable_assign_full() {
            Ok(ast) => return Ok(ast),
            Err(e) => errors.push(e)
        }
        
        if let lexer::TokenType::LP = self.lexer.advance()?.token {
            let expr = self.expr()?;
            if let lexer::TokenType::RP = self.lexer.advance()?.token { 
                return Ok(expr);
            }
            let next_tok = self.lexer.peek()?.clone();
            return Err(self.syntax_error(next_tok, "expected `)`", false, false));
        }

        self.lexer.set_pos(position);
        Err(Logger::longest(errors))
    }

    pub fn integer(&'a mut self) -> Result<Expr, Error> {
        let position = self.lexer.get_pos();

        let int = self.lexer.advance()?.clone();
        if let lexer::TokenType::NUMBER(value) = &int.token {
            Ok(
                ast::Expr::Integer(ast::Integer {
                    value: value.to_string(),
                    pos: int.pos
                })
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(int, "expected integer", false, false))
        }
    }

    pub fn string_literal(&'a mut self) -> Result<ast::StringLiteral, Error> {
        let position = self.lexer.get_pos();

        let string = self.lexer.advance()?.clone();
        if let lexer::TokenType::STRING(value) = &string.token {
            Ok(
                ast::StringLiteral {
                    value: value.to_string(),
                    pos: string.pos
                }
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(string, "expected string", false, false))
        }
    }

    pub fn namespace(&'a mut self) -> Result<ast::Namespace, Error> {
        let position = self.lexer.get_pos();
        let mut ids: Vec<ast::NameID> = Vec::new();
        let id = self.name_id()?;

        ids.push(id);

        loop {
            if self.lexer.peek()?.token != lexer::TokenType::DOUBLECOLON {
                break
            }

            self.next(lexer::TokenType::DOUBLECOLON, position, false)?;

            match self.name_id() {
                Ok(id) => { ids.push(id); },
                Err(e) => { self.lexer.set_pos(position); return Err(e); } 
            }
        }

        Ok(ast::Namespace {
            scopes: ids,
            pos: self.position(position)
        })
    }

    /// Parse name identifier (i.e. function name)
    pub fn name_id(&'a mut self) -> Result<ast::NameID, Error> {
        let position = self.lexer.get_pos();
        let id = self.lexer.advance()?.clone();
        if let lexer::TokenType::IDENTIFIER(value) = &id.token {
            Ok(
                ast::NameID {
                    value: value.to_string(),
                    pos: id.pos
                }
            )
        } else {
            self.lexer.set_pos(position);
            Err(self.syntax_error(id, "expected identifier", false, false))
        }
    }

    /// Parse type expression
    pub fn type_expr(&'a mut self) -> Result<ast::Type, Error> {
        // TODO: add tuple type  
        let namespace = self.namespace()?;

        Ok(
            ast::Type {
                pos: namespace.pos.clone(),
                value: ast::TypeType::Type(namespace)
            }
        )
    }

    /// Parse dollar id
    pub fn dollar_id(&'a mut self) -> Result<custom_syntax::DollarID, Error> {
        let position = self.lexer.get_pos();

        self.next(lexer::TokenType::DOLLAR, position, false)?;
        let id = self.name_id()?;

        Ok(custom_syntax::DollarID {
            value: id,
            pos: self.position(position)
        })
    }
}
