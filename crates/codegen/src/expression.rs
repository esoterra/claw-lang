use ast::ExpressionId;
use claw_ast as ast;
use claw_resolver::ItemId;

use crate::code::{CodeGenerator, ExpressionAllocator};
use crate::types::{
    Signedness, STRING_CONTENTS_ALIGNMENT, STRING_LENGTH_FIELD, STRING_OFFSET_FIELD,
};
use crate::GenerationError;

use cranelift_entity::EntityRef;
use wasm_encoder as enc;
use wasm_encoder::Instruction;

pub trait EncodeExpression {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError>;

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError>;
}

impl EncodeExpression for ast::Expression {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        let expr: &dyn EncodeExpression = match self {
            ast::Expression::Identifier(expr) => expr,
            ast::Expression::Literal(expr) => expr,
            ast::Expression::Call(expr) => expr,
            ast::Expression::Unary(expr) => expr,
            ast::Expression::Binary(expr) => expr,
        };
        expr.alloc_expr_locals(expression, allocator)
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        let expr: &dyn EncodeExpression = match self {
            ast::Expression::Identifier(expr) => expr,
            ast::Expression::Literal(expr) => expr,
            ast::Expression::Call(expr) => expr,
            ast::Expression::Unary(expr) => expr,
            ast::Expression::Binary(expr) => expr,
        };
        expr.encode(expression, code_gen)?;
        Ok(())
    }
}

impl EncodeExpression for ast::Identifier {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc(expression)
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        let fields = code_gen.fields(expression)?;
        match code_gen.lookup_name(self.ident) {
            ItemId::Import(_) => panic!("Cannot use import as value!!"),
            ItemId::Global(global) => {
                for field in fields.iter() {
                    // TODO handle composite globals
                    code_gen.instruction(&Instruction::GlobalGet(global.index() as u32));
                    code_gen.write_expr_field(expression, field);
                }
            }
            ItemId::Param(param) => {
                for field in fields.iter() {
                    code_gen.read_param_field(param, field);
                    code_gen.write_expr_field(expression, field);
                }
            }
            ItemId::Local(local) => {
                for field in fields.iter() {
                    code_gen.read_local_field(local, field);
                    code_gen.write_expr_field(expression, field);
                }
            }
            ItemId::Function(_) => panic!("Cannot use function as value!!"),
        }
        Ok(())
    }
}

impl EncodeExpression for ast::Literal {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc(expression)
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        match self {
            ast::Literal::String(string) => {
                // Allocate string pointer
                code_gen.instruction(&enc::Instruction::I32Const(0));
                code_gen.instruction(&enc::Instruction::I32Const(0));
                code_gen.instruction(&enc::Instruction::I32Const(
                    2i32.pow(STRING_CONTENTS_ALIGNMENT),
                ));
                code_gen.instruction(&enc::Instruction::I32Const(string.len() as i32));
                code_gen.allocate()?;
                code_gen.write_expr_field(expression, &STRING_OFFSET_FIELD);
                // Store the string length
                code_gen.instruction(&enc::Instruction::I32Const(string.len() as i32));
                code_gen.write_expr_field(expression, &STRING_LENGTH_FIELD);
                // Copy in the data segment
                let index = code_gen.encode_const_bytes(string.as_bytes());
                code_gen.read_expr_field(expression, &STRING_OFFSET_FIELD);
                code_gen.instruction(&enc::Instruction::I32Const(0));
                code_gen.read_expr_field(expression, &STRING_LENGTH_FIELD);
                code_gen.instruction(&enc::Instruction::MemoryInit {
                    mem: 0,
                    data_index: index.into(),
                })
            }
            ast::Literal::Integer(int) => {
                let field = code_gen.one_field(expression)?;
                code_gen.encode_const_int(*int, &field);
                code_gen.write_expr_field(expression, &field);
            }
            ast::Literal::Float(float) => {
                let field = code_gen.one_field(expression)?;
                code_gen.encode_const_float(*float, &field);
                code_gen.write_expr_field(expression, &field);
            }
        }
        Ok(())
    }
}

impl EncodeExpression for ast::Call {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc(expression)?;
        for arg in self.args.iter() {
            allocator.alloc_child(*arg)?;
        }
        Ok(())
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        for arg in self.args.iter() {
            code_gen.encode_child(*arg)?;
            for field in code_gen.fields(*arg)?.iter() {
                code_gen.read_expr_field(*arg, field);
            }
        }
        let item = code_gen.lookup_name(self.ident);
        code_gen.encode_call(item)?;
        for field in code_gen.fields(expression)?.iter() {
            code_gen.write_expr_field(expression, field);
        }
        Ok(())
    }
}

impl EncodeExpression for ast::UnaryExpression {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc(expression)?;
        allocator.alloc_child(self.inner)
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        code_gen.instruction(&enc::Instruction::I32Const(0)); // TODO support 64 bit ints
        code_gen.encode_child(self.inner)?;
        for field in code_gen.fields(self.inner)?.iter() {
            code_gen.read_expr_field(self.inner, field);
        }
        code_gen.instruction(&enc::Instruction::I32Sub);
        for field in code_gen.fields(expression)?.iter() {
            code_gen.write_expr_field(expression, field);
        }
        Ok(())
    }
}

impl EncodeExpression for ast::BinaryExpression {
    fn alloc_expr_locals(
        &self,
        expression: ExpressionId,
        allocator: &mut ExpressionAllocator,
    ) -> Result<(), GenerationError> {
        allocator.alloc(expression)?;
        allocator.alloc_child(self.left)?;
        allocator.alloc_child(self.right)?;
        Ok(())
    }

    fn encode(
        &self,
        expression: ExpressionId,
        code_gen: &mut CodeGenerator,
    ) -> Result<(), GenerationError> {
        code_gen.encode_child(self.left)?;
        code_gen.encode_child(self.right)?;

        let left_fields = code_gen.fields(self.left)?;
        for field in left_fields.iter() {
            code_gen.read_expr_field(self.left, field);
        }
        let right_fields = code_gen.fields(self.right)?;
        for field in right_fields.iter() {
            code_gen.read_expr_field(self.right, field);
        }

        if left_fields.len() == 1 {
            let field = &left_fields[0];
            let valtype = field.stack_type;
            let signedness = field.signedness;
            let mask = field.arith_mask;
            encode_binary_arithmetic(self.op, valtype, signedness, mask, code_gen);
        } else {
            todo!()
        }

        let fields = code_gen.fields(expression)?;
        for field in fields.iter() {
            code_gen.write_expr_field(expression, field);
        }

        Ok(())
    }
}

const S: Signedness = Signedness::Signed;
const U: Signedness = Signedness::Unsigned;

fn encode_binary_arithmetic(
    op: ast::BinaryOp,
    valtype: enc::ValType,
    signedness: Signedness,
    mask: Option<i32>,
    code_gen: &mut CodeGenerator,
) {
    let instruction = match (op, valtype, signedness) {
        // Multiply
        (ast::BinaryOp::Multiply, enc::ValType::I32, _) => enc::Instruction::I32Mul,
        (ast::BinaryOp::Multiply, enc::ValType::I64, _) => enc::Instruction::I64Mul,
        (ast::BinaryOp::Multiply, enc::ValType::F32, _) => enc::Instruction::F32Mul,
        (ast::BinaryOp::Multiply, enc::ValType::F64, _) => enc::Instruction::F64Mul,
        // Divide
        (ast::BinaryOp::Divide, enc::ValType::I32, S) => enc::Instruction::I32DivS,
        (ast::BinaryOp::Divide, enc::ValType::I32, U) => enc::Instruction::I32DivU,
        (ast::BinaryOp::Divide, enc::ValType::I64, S) => enc::Instruction::I64DivS,
        (ast::BinaryOp::Divide, enc::ValType::I64, U) => enc::Instruction::I64DivU,
        (ast::BinaryOp::Divide, enc::ValType::F32, _) => enc::Instruction::F32Div,
        (ast::BinaryOp::Divide, enc::ValType::F64, _) => enc::Instruction::F64Div,
        // Modulo
        (ast::BinaryOp::Modulo, enc::ValType::I32, S) => enc::Instruction::I32RemS,
        (ast::BinaryOp::Modulo, enc::ValType::I32, U) => enc::Instruction::I32RemU,
        (ast::BinaryOp::Modulo, enc::ValType::I64, S) => enc::Instruction::I64RemS,
        (ast::BinaryOp::Modulo, enc::ValType::I64, U) => enc::Instruction::I64RemU,
        // Addition
        (ast::BinaryOp::Add, enc::ValType::I32, _) => enc::Instruction::I32Add,
        (ast::BinaryOp::Add, enc::ValType::I64, _) => enc::Instruction::I64Add,
        (ast::BinaryOp::Add, enc::ValType::F32, _) => enc::Instruction::F32Add,
        (ast::BinaryOp::Add, enc::ValType::F64, _) => enc::Instruction::F64Add,
        // Subtraction
        (ast::BinaryOp::Subtract, enc::ValType::I32, _) => enc::Instruction::I32Sub,
        (ast::BinaryOp::Subtract, enc::ValType::I64, _) => enc::Instruction::I64Sub,
        (ast::BinaryOp::Subtract, enc::ValType::F32, _) => enc::Instruction::F32Sub,
        (ast::BinaryOp::Subtract, enc::ValType::F64, _) => enc::Instruction::F64Sub,
        // Logical Bit Shifting
        (ast::BinaryOp::BitShiftL, enc::ValType::I32, _) => enc::Instruction::I32Shl,
        (ast::BinaryOp::BitShiftL, enc::ValType::I64, _) => enc::Instruction::I64Shl,
        (ast::BinaryOp::BitShiftR, enc::ValType::I32, _) => enc::Instruction::I32ShrU,
        (ast::BinaryOp::BitShiftR, enc::ValType::I64, _) => enc::Instruction::I64ShrU,
        // Arithmetic Bit Shifting
        (ast::BinaryOp::ArithShiftR, enc::ValType::I32, S) => enc::Instruction::I32ShrS,
        (ast::BinaryOp::ArithShiftR, enc::ValType::I32, U) => enc::Instruction::I32ShrU,
        (ast::BinaryOp::ArithShiftR, enc::ValType::I64, S) => enc::Instruction::I64ShrS,
        (ast::BinaryOp::ArithShiftR, enc::ValType::I64, U) => enc::Instruction::I64ShrU,
        // Less than
        (ast::BinaryOp::LessThan, enc::ValType::I32, S) => enc::Instruction::I32LtS,
        (ast::BinaryOp::LessThan, enc::ValType::I32, U) => enc::Instruction::I32LtU,
        (ast::BinaryOp::LessThan, enc::ValType::I64, S) => enc::Instruction::I64LtS,
        (ast::BinaryOp::LessThan, enc::ValType::I64, U) => enc::Instruction::I64LtU,
        (ast::BinaryOp::LessThan, enc::ValType::F32, _) => enc::Instruction::F32Lt,
        (ast::BinaryOp::LessThan, enc::ValType::F64, _) => enc::Instruction::F64Lt,
        // Less than equal
        (ast::BinaryOp::LessThanEqual, enc::ValType::I32, S) => enc::Instruction::I32LeS,
        (ast::BinaryOp::LessThanEqual, enc::ValType::I32, U) => enc::Instruction::I32LeU,
        (ast::BinaryOp::LessThanEqual, enc::ValType::I64, S) => enc::Instruction::I64LeS,
        (ast::BinaryOp::LessThanEqual, enc::ValType::I64, U) => enc::Instruction::I64LeU,
        (ast::BinaryOp::LessThanEqual, enc::ValType::F32, _) => enc::Instruction::F32Le,
        (ast::BinaryOp::LessThanEqual, enc::ValType::F64, _) => enc::Instruction::F64Le,
        // Greater than
        (ast::BinaryOp::GreaterThan, enc::ValType::I32, S) => enc::Instruction::I32GtS,
        (ast::BinaryOp::GreaterThan, enc::ValType::I32, U) => enc::Instruction::I32GtU,
        (ast::BinaryOp::GreaterThan, enc::ValType::I64, S) => enc::Instruction::I64GtS,
        (ast::BinaryOp::GreaterThan, enc::ValType::I64, U) => enc::Instruction::I64GtU,
        (ast::BinaryOp::GreaterThan, enc::ValType::F32, _) => enc::Instruction::F32Gt,
        (ast::BinaryOp::GreaterThan, enc::ValType::F64, _) => enc::Instruction::F64Gt,
        // Greater than or equal
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::I32, S) => enc::Instruction::I32GeS,
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::I32, U) => enc::Instruction::I32GeU,
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::I64, S) => enc::Instruction::I64GeS,
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::I64, U) => enc::Instruction::I64GeU,
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::F32, _) => enc::Instruction::F32Ge,
        (ast::BinaryOp::GreaterThanEqual, enc::ValType::F64, _) => enc::Instruction::F64Ge,
        // Equal
        (ast::BinaryOp::Equals, enc::ValType::I32, _) => enc::Instruction::I32Eq,
        (ast::BinaryOp::Equals, enc::ValType::I64, _) => enc::Instruction::I64Eq,
        (ast::BinaryOp::Equals, enc::ValType::F32, _) => enc::Instruction::F32Eq,
        (ast::BinaryOp::Equals, enc::ValType::F64, _) => enc::Instruction::F64Eq,
        // Not equal
        (ast::BinaryOp::NotEquals, enc::ValType::I32, _) => enc::Instruction::I32Eq,
        (ast::BinaryOp::NotEquals, enc::ValType::I64, _) => enc::Instruction::I64Eq,
        (ast::BinaryOp::NotEquals, enc::ValType::F32, _) => enc::Instruction::F32Eq,
        (ast::BinaryOp::NotEquals, enc::ValType::F64, _) => enc::Instruction::F64Eq,
        // Bitwise and
        (ast::BinaryOp::BitAnd, enc::ValType::I32, _) => enc::Instruction::I32And,
        (ast::BinaryOp::BitAnd, enc::ValType::I64, _) => enc::Instruction::I64And,
        // Bitwise xor
        (ast::BinaryOp::BitXor, enc::ValType::I32, _) => enc::Instruction::I32Xor,
        (ast::BinaryOp::BitXor, enc::ValType::I64, _) => enc::Instruction::I64Xor,
        // Bitwise or
        (ast::BinaryOp::BitOr, enc::ValType::I32, _) => enc::Instruction::I32Or,
        (ast::BinaryOp::BitOr, enc::ValType::I64, _) => enc::Instruction::I64Or,
        // Logical and/or
        (ast::BinaryOp::LogicalAnd, enc::ValType::I32, _) => enc::Instruction::I32And,
        (ast::BinaryOp::LogicalOr, enc::ValType::I32, _) => enc::Instruction::I32Or,
        // Fallback
        (operator, valtype, _) => panic!(
            "Cannot apply binary operator {:?} to type {:?}",
            operator, valtype
        ),
    };
    code_gen.instruction(&instruction);

    if let Some(mask) = mask {
        code_gen.instruction(&enc::Instruction::I32Const(mask));
        code_gen.instruction(&enc::Instruction::I32And);
    }
}
