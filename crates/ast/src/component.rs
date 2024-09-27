//! Contains the [Component] struct which is the root
//! of the AST and contains root items (e.g. import, function),
//! inner AST nodes (e.g. expression), and the source code.

use std::collections::HashMap;

use cranelift_entity::{entity_impl, PrimaryMap};

use crate::PackageName;
use claw_common::Source;

use super::{
    expressions::{Expression, ExpressionId},
    statements::{Statement, StatementId},
    types::{FnType, TypeDefId, TypeDefinition},
    NameId, Span, TypeId, ValType,
};

/// The unique ID of an Import item
///
/// IDs must only be passed to the [Component] they were
/// made by and this is not statically or dynamically validated.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImportId(u32);
entity_impl!(ImportId, "import");

/// The unique ID of a Global item
///
/// IDs must only be passed to the [Component] they were
/// made by and this is not statically or dynamically validated.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GlobalId(u32);
entity_impl!(GlobalId, "global");

/// The unique ID of a Function item
///
/// IDs must only be passed to the [Component] they were
/// made by and this is not statically or dynamically validated.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionId(u32);
entity_impl!(FunctionId, "func");

/// Each Claw source file represents a Component
/// and this struct represents the root of the AST.
///
/// The different types of AST nodes each have a unique ID type,
/// so it is impossible to try to retrieve an import as a function.
///
/// No static or dynamic validation that an ID is from the correct
/// AST is performed and if an ID from one AST is provided to another
/// bad things will happen!
#[derive(Debug)]
pub struct Component {
    /// The source text that the component was created from.
    src: Source,

    // Top level items
    imports: PrimaryMap<ImportId, Import>,
    type_defs: PrimaryMap<TypeDefId, TypeDefinition>,
    globals: PrimaryMap<GlobalId, Global>,
    functions: PrimaryMap<FunctionId, Function>,

    // Inner items
    types: PrimaryMap<TypeId, ValType>,
    type_spans: HashMap<TypeId, Span>,

    statements: PrimaryMap<StatementId, Statement>,
    statement_spans: HashMap<StatementId, Span>,

    expressions: PrimaryMap<ExpressionId, Expression>,
    expression_spans: HashMap<ExpressionId, Span>,

    names: PrimaryMap<NameId, String>,
    name_spans: HashMap<NameId, Span>,
}

impl Component {
    /// Create a new empty Component AST for a source file.
    ///
    /// This does not do any parsing!!!
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
            expressions: Default::default(),
            expression_spans: Default::default(),
            names: Default::default(),
            name_spans: Default::default(),
        }
    }

    /// The source code that the AST represents.
    pub fn source(&self) -> Source {
        self.src.clone()
    }

    /// Add a top-level import item to the AST.
    pub fn push_import(&mut self, import: Import) -> ImportId {
        self.imports.push(import)
    }

    /// Iterate over the top-level import items.
    pub fn iter_imports(&self) -> impl Iterator<Item = (ImportId, &Import)> {
        self.imports.iter()
    }

    /// Get a specific import item by its id.
    pub fn get_import(&self, import: ImportId) -> &Import {
        &self.imports[import]
    }

    /// Add a top-level type definition item to the AST.
    pub fn push_type_def(&mut self, type_def: TypeDefinition) -> TypeDefId {
        self.type_defs.push(type_def)
    }

    /// Iterate over the top-level type definition items.
    pub fn iter_type_defs(&self) -> impl Iterator<Item = (TypeDefId, &TypeDefinition)> {
        self.type_defs.iter()
    }

    /// Get a specific type definition item by its id.
    pub fn get_type_def(&self, type_def: TypeDefId) -> &TypeDefinition {
        &self.type_defs[type_def]
    }

    /// Add a top-level global item to the AST.
    pub fn push_global(&mut self, global: Global) -> GlobalId {
        self.globals.push(global)
    }

    /// Iterate over the top-level global items.
    pub fn iter_globals(&self) -> impl Iterator<Item = (GlobalId, &Global)> {
        self.globals.iter()
    }

    /// Get a specific global item by its id.
    pub fn get_global(&self, global: GlobalId) -> &Global {
        &self.globals[global]
    }

    /// Add a top-level function item to the AST.
    pub fn push_function(&mut self, function: Function) -> FunctionId {
        self.functions.push(function)
    }

    /// Iterate over the top-level function items.
    pub fn iter_functions(&self) -> impl Iterator<Item = (FunctionId, &Function)> {
        self.functions.iter()
    }

    /// Get a specific function item by its id.
    pub fn get_function(&self, function: FunctionId) -> &Function {
        &self.functions[function]
    }

    /// Create a new name AST node.
    pub fn new_name(&mut self, name: String, span: Span) -> NameId {
        let id = self.names.push(name);
        self.name_spans.insert(id, span);
        id
    }

    /// Get the value of a name.
    pub fn get_name(&self, id: NameId) -> &str {
        self.names.get(id).unwrap()
    }

    /// Get the source span for this name.
    pub fn name_span(&self, id: NameId) -> Span {
        *self.name_spans.get(&id).unwrap()
    }

    /// Create a new valtype AST node.
    pub fn new_type(&mut self, valtype: ValType, span: Span) -> TypeId {
        let id = self.types.push(valtype);
        self.type_spans.insert(id, span);
        id
    }

    /// Get the value of a valtype AST node.
    pub fn get_type(&self, id: TypeId) -> &ValType {
        self.types.get(id).unwrap()
    }

    /// Get the source span for this valtype.
    pub fn type_span(&self, id: TypeId) -> Span {
        *self.type_spans.get(&id).unwrap()
    }

    /// Create a new statement AST node.
    pub fn new_statement(&mut self, statement: Statement, span: Span) -> StatementId {
        let id = self.statements.push(statement);
        self.statement_spans.insert(id, span);
        id
    }

    /// Get the value of a statement AST node.
    pub fn get_statement(&self, id: StatementId) -> &Statement {
        self.statements.get(id).unwrap()
    }

    /// Get the source span for this statement.
    pub fn statement_span(&self, id: StatementId) -> Span {
        *self.statement_spans.get(&id).unwrap()
    }

    /// Create a new expression AST node.
    pub fn new_expression(&mut self, expression: Expression, span: Span) -> ExpressionId {
        let id = self.expressions.push(expression);
        self.expression_spans.insert(id, span);
        id
    }

    /// Get the value of a expression AST node.
    pub fn get_expression(&self, id: ExpressionId) -> &Expression {
        self.expressions.get(id).unwrap()
    }

    /// Get the source span for this expression.
    pub fn expression_span(&self, id: ExpressionId) -> Span {
        *self.expression_spans.get(&id).unwrap()
    }
}

/// Import AST node (Claw)
///
/// There are two versions: plain and import-from.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Import {
    Plain(PlainImport),
    ImportFrom(ImportFrom),
}

/// Plain Import AST node (Claw)
///
/// ```claw
/// import foo: func() -> u32;
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PlainImport {
    /// The name of the item to import.
    pub ident: NameId,
    /// The name given to the imported item.
    /// Defaults to the specified name if omitted.
    pub alias: Option<NameId>,
    /// The type of the imported item.
    pub external_type: ExternalType,
}

/// Import From AST node (Claw)
///
/// ```claw
/// import { foo } from bar;
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportFrom {
    /// The first name is the imported item's name
    /// The second optional name is an alias
    pub items: Vec<(NameId, Option<NameId>)>,
    /// The package being imported from
    pub package: PackageName,
    /// Which interface from the package to import
    pub interface: String,
}

/// External Type AST node (Claw)
///
/// ```claw
/// func(foo: string) -> bool
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalType {
    Function(FnType),
}

/// Global Item AST node (Claw)
///
/// ```claw
/// let foo: u32 = 1;
/// ```
#[derive(Debug, Clone)]
pub struct Global {
    /// Whether the global is exported.
    ///
    /// Indicated by the keyword `export` in front
    /// of the global item.
    pub exported: bool,
    /// Whether the global is mutable.
    ///
    /// Indicated by the `mut` keyword before after `let`.
    pub mutable: bool,
    /// The name of the global.
    pub ident: NameId,
    /// The type of the global.
    pub type_id: TypeId,
    /// The initialization expression for the global.
    pub init_value: ExpressionId,
}

/// Function Item AST node (Claw)
///
/// ```claw
/// func always-false() -> bool {
///     return false;
/// }
/// ```
#[derive(Debug)]
pub struct Function {
    /// Whether the global is exported.
    ///
    /// Indicated by the keyword `export` in front
    /// of the function item.
    pub exported: bool,
    /// The name of the function.
    pub ident: NameId,
    /// The function's parameters.
    ///
    /// Each parameter has a name and type.
    pub params: Vec<(NameId, TypeId)>,
    /// The result type of the function.
    ///
    /// Result type is unit if omitted.
    pub results: Option<TypeId>,
    /// The body of the function.
    pub body: Vec<StatementId>,
}
