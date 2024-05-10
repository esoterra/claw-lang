use crate::code::{CodeGenerator, ExpressionAllocator};

use super::GenerationError;
use ast::{ExpressionId, NameId, Statement};
use claw_ast as ast;
use claw_resolver::ItemId;

use cranelift_entity::EntityRef;
use wasm_encoder as enc;
use wasm_encoder::Instruction;

pub trait EncodeStatement {
    fn alloc_expr_locals(&self, allocator: &mut ExpressionAllocator)
        -> Result<(), GenerationError>;

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError>;
}

impl EncodeStatement for Statement {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        let statement: &dyn EncodeStatement = match self {
            Statement::Let(statement) => statement,
            Statement::Assign(statement) => statement,
            Statement::Call(statement) => statement,
            Statement::If(statement) => statement,
            Statement::Return(statement) => statement,
        };
        statement.alloc_expr_locals(allocator)
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        let statement: &dyn EncodeStatement = match self {
            Statement::Let(statement) => statement,
            Statement::Assign(statement) => statement,
            Statement::Call(statement) => statement,
            Statement::If(statement) => statement,
            Statement::Return(statement) => statement,
        };
        statement.encode(code_gen)
    }
}

impl EncodeStatement for ast::Let {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc_child(self.expression)
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        encode_assignment(self.ident, self.expression, code_gen)
    }
}

impl EncodeStatement for ast::Assign {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc_child(self.expression)
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        encode_assignment(self.ident, self.expression, code_gen)
    }
}

impl EncodeStatement for ast::Call {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        for arg in self.args.iter() {
            allocator.alloc_child(*arg)?;
        }
        Ok(())
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        for arg in self.args.iter() {
            code_gen.encode_child(*arg)?;
        }
        let item = code_gen.lookup_name(self.ident);
        code_gen.encode_call(item, &self.args, None)?;
        Ok(())
    }
}

impl EncodeStatement for ast::If {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc_child(self.condition)?;
        for statement in self.block.iter() {
            allocator.alloc_statement(*statement)?;
        }
        Ok(())
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        code_gen.encode_child(self.condition)?;
        let fields = code_gen.fields(self.condition)?;
        assert_eq!(fields.len(), 1);
        code_gen.read_expr_field(self.condition, &fields[0]);
        code_gen.instruction(&Instruction::If(enc::BlockType::Empty));
        for statement in self.block.iter() {
            code_gen.encode_statement(*statement)?;
        }
        code_gen.instruction(&Instruction::End);
        Ok(())
    }
}

impl EncodeStatement for ast::For {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc_child(self.range_lower)?;
        allocator.alloc_child(self.range_upper)?;
        for statement in self.block.iter() {
            allocator.alloc_statement(*statement)?;
        }
        Ok(())
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        code_gen.encode_child(self.condition)?;
        let fields = code_gen.fields(self.condition)?;
        assert_eq!(fields.len(), 1);
        code_gen.read_expr_field(self.condition, &fields[0]);
        code_gen.instruction(&Instruction::If(enc::BlockType::Empty));
        for statement in self.block.iter() {
            code_gen.encode_statement(*statement)?;
        }
        code_gen.instruction(&Instruction::End);
        Ok(())
    }
}

impl EncodeStatement for ast::Return {
    fn alloc_expr_locals(
        &self,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        if let Some(expression) = self.expression {
            allocator.alloc_child(expression)?;
        }
        Ok(())
    }

    fn encode(&self, code_gen: &mut CodeGenerator) -> Result<(), GenerationError> {
        if let Some(expression) = self.expression {
            code_gen.encode_child(expression)?;

            let fields = code_gen.fields(expression)?;
            if code_gen.spill_return() {
                for field in fields.iter() {
                    code_gen.read_return_ptr()?;
                    code_gen.field_address(field);
                    code_gen.read_expr_field(expression, field);
                    code_gen.write_mem(field);
                }
                code_gen.read_return_ptr()?;
            } else {
                for field in fields.iter() {
                    code_gen.read_expr_field(expression, field);
                }
            }
        }
        code_gen.instruction(&Instruction::Return);
        Ok(())
    }
}

fn encode_assignment(
    ident: NameId,
    expression: ExpressionId,
    code_gen: &mut CodeGenerator,
) -> Result<(), GenerationError> {
    code_gen.encode_child(expression)?;
    let fields = code_gen.fields(expression)?;
    match code_gen.lookup_name(ident) {
        ItemId::ImportFunc(_) => panic!("Assigning to imported function isn't allowed!!"),
        ItemId::Type(_) => panic!("Assigning to imported type isn't allowed!!"),
        ItemId::Global(global) => {
            // TODO handle composite globals
            for field in fields {
                code_gen.read_expr_field(expression, &field);
                code_gen.instruction(&Instruction::GlobalSet(global.index() as u32));
            }
        }
        ItemId::Param(_) => panic!("Assigning to parameters isn't allowed!!"),
        ItemId::Local(local) => {
            for field in fields {
                code_gen.read_expr_field(expression, &field);
                code_gen.write_local_field(local, &field);
            }
        }
        ItemId::Function(_) => panic!("Assigning to functions isn't allowed!!"),
    }
    Ok(())
}
