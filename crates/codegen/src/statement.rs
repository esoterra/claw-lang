use super::{encode_expression, CodeGenerator, GenerationError};
use claw_ast as ast;
use ast::{ExpressionId, FunctionId, NameId, StatementId};
use claw_resolver::{ItemId, ResolvedComponent};

use cranelift_entity::EntityRef;
use wasm_encoder as enc;
use wasm_encoder::Instruction;

pub fn encode_statement(
    generator: &CodeGenerator,
    component: &ResolvedComponent,
    statement: StatementId,
    func: FunctionId,
    builder: &mut enc::Function,
) -> Result<(), GenerationError> {
    let s: &dyn EncodeStatement = match &component.component.get_statement(statement) {
        ast::Statement::Let(statement) => statement,
        ast::Statement::Assign(statement) => statement,
        ast::Statement::Call(statement) => statement,
        ast::Statement::If(statement) => statement,
        ast::Statement::Return(statement) => statement,
    };
    s.encode_statement(generator, component, func, builder)?;
    Ok(())
}

pub trait EncodeStatement {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError>;
}

impl EncodeStatement for ast::Let {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        encode_assignment(
            generator,
            component,
            func,
            self.ident,
            self.expression,
            builder,
        )
    }
}

impl EncodeStatement for ast::Assign {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        encode_assignment(
            generator,
            component,
            func,
            self.ident,
            self.expression,
            builder,
        )
    }
}

impl EncodeStatement for ast::Call {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let resolver = component.resolved_funcs.get(&func).unwrap();

        for arg in self.args.iter() {
            encode_expression(generator, component, *arg, func, builder)?;
        }
        let index = match resolver.bindings.get(&self.ident).unwrap() {
            ItemId::Import(import) => *generator.func_idx_for_import.get(import).unwrap(),
            ItemId::Function(function) => *generator.func_idx_for_func.get(function).unwrap(),
            _ => panic!(""),
        };
        builder.instruction(&Instruction::Call(index.into()));
        Ok(())
    }
}

impl EncodeStatement for ast::If {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        encode_expression(generator, component, self.condition, func, builder)?;
        builder.instruction(&Instruction::If(enc::BlockType::Empty));
        for statement in self.block.iter() {
            encode_statement(generator, component, *statement, func, builder)?;
        }
        builder.instruction(&Instruction::End);
        Ok(())
    }
}

impl EncodeStatement for ast::Return {
    fn encode_statement(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        if let Some(expression) = self.expression {
            encode_expression(generator, component, expression, func, builder)?;
        }
        builder.instruction(&Instruction::Return);
        Ok(())
    }
}

fn encode_assignment(
    generator: &CodeGenerator,
    component: &ResolvedComponent,
    func: FunctionId,
    ident: NameId,
    expression: ExpressionId,
    builder: &mut enc::Function,
) -> Result<(), GenerationError> {
    let resolver = component.resolved_funcs.get(&func).unwrap();
    encode_expression(generator, component, expression, func, builder)?;
    match resolver.bindings.get(&ident).unwrap() {
        ItemId::Import(_) => unimplemented!(),
        ItemId::Global(global) => {
            builder.instruction(&Instruction::GlobalSet(global.index() as u32));
        }
        ItemId::Param(param) => {
            let local_index = param.index() as u32;
            builder.instruction(&Instruction::LocalSet(local_index));
        }
        ItemId::Local(local) => {
            let func = component.component.functions.get(func).unwrap();
            let local_index = local.index() + func.arguments.len();
            let local_index = local_index as u32;
            builder.instruction(&Instruction::LocalSet(local_index));
        }
        ItemId::Function(_) => unimplemented!(),
    }
    Ok(())
}
