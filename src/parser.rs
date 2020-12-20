use crate::ast::*;

peg::parser!{
    pub grammar code_parser() for str {
        pub rule program() -> Program
            = _ funcs:func_def() ** _ { Program { funcs } }

        rule func_def() -> Func
            = decl:func_decl() _ body:statement_block()? { Func { decl, body }}

        rule func_decl() -> FuncDecl
            = ext:func_external()? _ "fn" _ ident:ident() "(" _ args:func_args() _ ")" _  "->" _ ty:ident() {
                    FuncDecl {
                        external: ext.is_some(),
                        calling_conv: if let Some(cc) = ext { cc } else { String::new() },
                        ident,
                        args,
                        ret: Type { ident: ty }
                    }
                }

        // dummy
        rule func_external() -> String
            = "extern" _ cc:$("\"C\"") { cc.to_string() }

        rule func_args() -> Vec<VarDecl>
            = decl:general_decl() ** ", " { decl }

        rule statement() -> Stmt
            = e:expression() _ ";" { Stmt::Expr(e) }
            / d:variable_decl() _ ";" { Stmt::VarDecl(d) }
            / s:statement_block() { s }
            / s:statement_return() { s }
            / s:if_statement() { s }
            / s:while_statement() { s }


        rule while_statement() -> Stmt
            = "while" _ e:expression() _ body:statement_block() { Stmt::While(e, Box::new(body)) }

        rule if_statement() -> Stmt
            = "if" _ e:expression() _ then_body:statement_block() _ else_body:else_part()?
                { Stmt::If(e, Box::new(then_body), else_body) }

        rule else_part() -> Box<Stmt>
            = _ "else" _ else_body:statement_block() { Box::new(else_body) }

        rule statement_block() -> Stmt
            = "{" _ s:statement() ** _ _ "}" { Stmt::Block(s) }

        rule statement_return() -> Stmt
            = "return" _ e:expression() _ ";" { Stmt::Ret(e) }
            
        rule variable_decl() -> VarDecl
            = "let" _ d:general_decl() { d }

        rule general_decl() -> VarDecl
            = v:ident() _ ":" _ t:ident() { VarDecl { ident: v, ty: Type { ident: t }} }

        rule expression() -> Expr
            = i:ident() _ "=" _ s:sum() { Expr::Assign(i, Box::new(s)) }
            / sum()

        rule sum() -> Expr
            = a:product() _ "+" _ b:sum() { Expr::Add(Box::new(a), Box::new(b)) }
            / a:product() _ "-" _ b:sum() { Expr::Sub(Box::new(a), Box::new(b)) }
            / product()

        rule product() -> Expr
            = a:atom() _ "*" _ b:product() { Expr::Mul(Box::new(a), Box::new(b)) }
            / a:atom() _ "/" _ b:product() { Expr::Div(Box::new(a), Box::new(b)) }
            / a:atom() _ "%" _ b:product() { Expr::Mod(Box::new(a), Box::new(b)) }
            / atom()

        rule func_call() -> Expr
            = name:ident() _ "(" _ e:expression() ** ", " _ ")" { Expr::Call(name, e) }

        rule atom() -> Expr
            = n:int_literal() { n }
            / n:func_call() { n }
            / n:ident() { Expr::Ref(n) }
            / "(" _ e:expression() _ ")" { e }

        rule int_literal() -> Expr
            = n:$(['0'..='9']+) { Expr::Literal(n.parse().unwrap()) }

        //rule str_literal() -> String
        //    = "\"" n:$(!['"']*) "\"" { n.to_string() }

        rule ident() -> String
            = i:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*) { i.to_string() }

        rule _() = [' ' | '\t' | '\n']*

    }
}