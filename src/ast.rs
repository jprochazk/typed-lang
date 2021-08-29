use std::borrow::Cow;

use crate::lexer::Token;
use ast2str::AstToStr;
use derive_more::{Constructor, From};

pub type Span = std::ops::Range<usize>;

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Program<'source> {
  pub imports: Vec<Import<'source>>,
  pub statements: Vec<Stmt<'source>>,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum ImportPath<'source> {
  File(Token<'source>),
  Module(Token<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct ImportItem<'source> {
  name: Token<'source>,
  alias: Option<Token<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct ImportFull<'source> {
  path: ImportPath<'source>,
  alias: Option<Token<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct ImportPartial<'source> {
  path: ImportPath<'source>,
  items: Vec<ImportItem<'source>>,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum Import<'source> {
  Full(#[forward] ImportFull<'source>),
  Partial(#[forward] ImportPartial<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Stmt<'source> {
  #[forward]
  kind: StmtKind<'source>,
  span: Span,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum StmtKind<'source> {
  Block(#[forward] Box<Block<'source>>),
  If(#[forward] Box<If<'source>>),
  Var(#[forward] Box<Var<'source>>),
  For(#[forward] Box<For<'source>>),
  While(#[forward] Box<While<'source>>),
  Loop(#[forward] Box<Loop<'source>>),
  Fn(#[forward] Box<Func<'source>>),
  Throw(#[rename = "value"] Box<Expr<'source>>),
  Try(#[forward] Box<Try<'source>>),
  Pass,
  Continue,
  Break,
  Return(#[rename = "value"] Box<Expr<'source>>),
  Struct(#[forward] Box<Struct<'source>>),
  Trait(#[forward] Box<Trait<'source>>),
  Type(#[forward] Box<Type<'source>>),
  Expr(#[rename = "expr"] Box<Expr<'source>>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Block<'source> {
  pub statements: Vec<Stmt<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct If<'source> {
  pub cond: Expr<'source>,
  pub then: Stmt<'source>,
  pub r#else: Option<Branch<'source>>,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum Branch<'source> {
  Elif(Box<If<'source>>),
  Else(Box<Block<'source>>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Var<'source> {
  pub ident: Token<'source>,
  pub r#type: TypeExpr<'source>,
  pub init: Expr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct For<'source> {
  pub var: Token<'source>,
  pub iter: Expr<'source>,
  pub body: Stmt<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct While<'source> {
  pub cond: Expr<'source>,
  pub body: Stmt<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Loop<'source> {
  pub body: Stmt<'source>,
}

#[derive(Debug, PartialEq, AstToStr)]
pub struct Func<'source> {
  pub decl: FuncDecl<'source>,
  pub body: Stmt<'source>,
}

impl<'source> Func<'source> {
  pub fn new(
    name: Token<'source>,
    type_params: Vec<Param<'source>>,
    params: Vec<Param<'source>>,
    ret: TypeExpr<'source>,
    body: Stmt<'source>,
  ) -> Func<'source> {
    Func {
      decl: FuncDecl::new(name, type_params, params, ret),
      body,
    }
  }
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct FuncDecl<'source> {
  pub name: Token<'source>,
  pub type_params: Vec<Param<'source>>,
  pub params: Vec<Param<'source>>,
  pub ret: TypeExpr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Param<'source> {
  pub name: Token<'source>,
  pub r#type: TypeExpr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Try<'source> {
  pub body: Stmt<'source>,
  pub catch: Option<Box<Catch<'source>>>,
  pub finally: Option<Stmt<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Catch<'source> {
  pub var: Token<'source>,
  pub body: Stmt<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Struct<'source> {
  pub name: Token<'source>,
  pub body: Option<StructBody<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct StructBody<'source> {
  pub props: Vec<Property<'source>>,
  pub funcs: Vec<Func<'source>>,
  pub impls: Vec<Impl<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Property<'source> {
  pub name: Token<'source>,
  pub r#type: TypeExpr<'source>,
  pub init: Expr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Impl<'source> {
  pub name: Token<'source>,
  pub funcs: Vec<Func<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Trait<'source> {
  pub name: Token<'source>,
  pub decls: Vec<FuncDecl<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Type<'source> {
  pub name: Token<'source>,
  pub init: TypeExpr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct TypeExpr<'source> {
  pub kind: TypeExprKind<'source>,
  pub span: Span,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum TypeExprKind<'source> {
  Intersection(
    #[rename = "left"] Box<TypeExpr<'source>>,
    #[rename = "right"] Box<TypeExpr<'source>>,
  ),
  TypeName(#[forward] Token<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Expr<'source> {
  pub kind: ExprKind<'source>,
  pub span: Span,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum ExprKind<'source> {
  Lambda(#[forward] Box<Lambda<'source>>),
  Comma(#[rename = "ops"] Vec<Expr<'source>>),
  Binary(#[forward] Box<Binary<'source>>),
  Unary(#[forward] Box<Unary<'source>>),
  Call(#[forward] Box<Call<'source>>),
  Literal(#[forward] Box<Literal<'source>>),
  Assignment(#[forward] Box<Assignment<'source>>),
  Access(#[forward] Box<Access<'source>>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Lambda<'source> {
  pub params: Vec<Token<'source>>,
  pub body: Stmt<'source>,
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum Assignment<'source> {
  Variable(#[rename = "name"] Token<'source>, #[rename = "value"] Expr<'source>),
  Property(
    #[rename = "node"] Expr<'source>,
    #[rename = "field"] Token<'source>,
    #[rename = "value"] Expr<'source>,
  ),
  Item(
    #[rename = "node"] Expr<'source>,
    #[rename = "key"] Expr<'source>,
    #[rename = "value"] Expr<'source>,
  ),
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum Access<'source> {
  Variable(#[rename = "name"] Token<'source>),
  Property(#[rename = "node"] Expr<'source>, #[rename = "field"] Token<'source>),
  Item(#[rename = "node"] Expr<'source>, #[rename = "key"] Expr<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Call<'source> {
  pub callee: Expr<'source>,
  pub args: Vec<Arg<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Arg<'source> {
  pub name: Token<'source>,
  pub value: Expr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Binary<'source> {
  #[debug]
  pub op: BinaryOp,
  pub left: Expr<'source>,
  pub right: Expr<'source>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
  /// `||`, `or`
  Or,
  /// `&&`, `and`
  And,
  /// `|`
  BitOr,
  /// `^`
  BitXor,
  /// `&`
  BitAnd,
  /// `==`
  Eq,
  /// `!=`
  Neq,
  /// `is`
  Is,
  /// `<`
  Lt,
  /// `<=`
  Le,
  /// `>`
  Gt,
  /// `>=`
  Ge,
  /// `<<`
  BitShl,
  /// `>>`
  BitShr,
  /// `+`
  Add,
  /// `-`
  Sub,
  /// `*`
  Mul,
  /// `/`
  Div,
  /// `%`
  Rem,
  /// `**`
  Pow,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Unary<'source> {
  #[debug]
  pub op: UnaryOp,
  pub operand: Expr<'source>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
  /// `-`
  Neg,
  /// `!`, `not`
  Not,
  /// `~`
  BitNot,
}

#[derive(Debug, PartialEq, AstToStr, From)]
pub enum Literal<'source> {
  None,
  Bool(bool),
  Int(i32),
  Float(f64),
  String(Cow<'source, str>),
  List(Vec<ListItem<'source>>),
  Map(Vec<MapEntry<'source>>),
}

pub type ListItem<'source> = Expr<'source>;
pub type MapEntry<'source> = (Expr<'source>, Expr<'source>);
