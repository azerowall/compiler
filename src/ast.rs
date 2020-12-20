

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Func {
    pub ident: Ident,
    pub args: Vec<VarDecl>,
    pub ret_type: Type,
    pub body: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Block(Vec<Stmt>),
    VarDecl(VarDecl),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Ret(Expr)
}

#[derive(Debug)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Literal(u32),
    Ref(Ident),
    Assign(Ident, Box<Expr>),
    Call(Ident, Vec<Expr>),
}

#[derive(Debug)]
pub struct VarDecl {
    pub ident: Ident,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Type {
    pub ident: Ident,
}

pub type Ident = String;