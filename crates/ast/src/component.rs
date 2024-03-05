use std::collections::HashMap;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::{expressions::ExpressionData, PackageName, TypeDefId, TypeDefinition};
use claw_common::Source;

use super::{
    expressions::ExpressionId,
    statements::{Let, Statement, StatementId},
    types::FnType,
    NameId, Span, TypeId, ValType,
};

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImportId(u32);
entity_impl!(ImportId, "import");

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GlobalId(u32);
entity_impl!(GlobalId, "global");

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionId(u32);
entity_impl!(FunctionId, "func");

/// Each Claw source file represents a Component
/// and this struct represents the root of the AST.
#[derive(Debug)]
pub struct Component {
    pub src: Source,

    // Top level items
    pub imports: PrimaryMap<ImportId, Import>,
    pub type_defs: PrimaryMap<TypeDefId, TypeDefinition>,
    pub globals: PrimaryMap<GlobalId, Global>,
    pub functions: PrimaryMap<FunctionId, Function>,

    // Inner items
    pub types: PrimaryMap<TypeId, ValType>,
    pub type_spans: HashMap<TypeId, Span>,

    pub statements: PrimaryMap<StatementId, Statement>,
    pub statement_spans: HashMap<StatementId, Span>,

    pub expression_data: ExpressionData,

    pub names: PrimaryMap<NameId, String>,
    pub name_spans: HashMap<NameId, Span>,
}

impl Component {
    pub fn new(src: Source) -> Self {
        Self {
            src,
            imports: Default::default(),
            type_defs: Default::default(),
            globals: Default::default(),
            functions: Default::default(),
            types: Default::default(),
            type_spans: Default::default(),
            statements: Default::default(),
            statement_spans: Default::default(),
            expression_data: Default::default(),
            names: Default::default(),
            name_spans: Default::default(),
        }
    }

    pub fn new_name(&mut self, name: String, span: Span) -> NameId {
        let id = self.names.push(name);
        self.name_spans.insert(id, span);
        id
    }

    pub fn get_name(&self, id: NameId) -> &str {
        self.names.get(id).unwrap()
    }

    pub fn name_span(&self, id: NameId) -> Span {
        *self.name_spans.get(&id).unwrap()
    }

    pub fn new_type(&mut self, valtype: ValType, span: Span) -> TypeId {
        let id = self.types.push(valtype);
        self.type_spans.insert(id, span);
        id
    }

    pub fn get_type(&self, id: TypeId) -> &ValType {
        self.types.get(id).unwrap()
    }

    pub fn type_span(&self, id: TypeId) -> Span {
        *self.type_spans.get(&id).unwrap()
    }

    pub fn new_statement(&mut self, statement: Statement, span: Span) -> StatementId {
        let id = self.statements.push(statement);
        self.statement_spans.insert(id, span);
        id
    }

    pub fn get_statement(&self, id: StatementId) -> &Statement {
        self.statements.get(id).unwrap()
    }

    pub fn statement_span(&self, id: StatementId) -> Span {
        *self.statement_spans.get(&id).unwrap()
    }

    pub fn alloc_let(
        &mut self,
        mutable: bool,
        ident: NameId,
        annotation: Option<TypeId>,
        expression: ExpressionId,
        span: Span,
    ) -> StatementId {
        let let_ = Let {
            mutable,
            ident,
            annotation,
            expression,
        };
        self.new_statement(Statement::Let(let_), span)
    }

    pub fn expr(&self) -> &ExpressionData {
        &self.expression_data
    }

    pub fn expr_mut(&mut self) -> &mut ExpressionData {
        &mut self.expression_data
    }
}

///
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Import {
    Plain(PlainImport),
    ImportFrom(ImportFrom),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PlainImport {
    pub ident: NameId,
    pub external_type: ExternalType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportFrom {
    /// The first name is the imported item's name
    /// The second optional name is an alias
    pub items: Vec<(NameId, Option<NameId>)>,
    pub package: PackageName,
    pub interface: String,
}

///
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalType {
    Function(FnType),
}

///
#[derive(Debug)]
pub struct Function {
    pub exported: bool,
    pub ident: NameId,
    pub params: Vec<(NameId, TypeId)>,
    pub results: Option<TypeId>,
    pub body: Vec<StatementId>,
}

///
#[derive(Debug, Clone)]
pub struct Global {
    pub exported: bool,
    pub mutable: bool,
    pub ident: NameId,
    pub type_id: TypeId,
    pub init_value: ExpressionId,
}
