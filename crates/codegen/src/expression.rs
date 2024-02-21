
use claw_ast as ast;
use ast::{ExpressionId, FunctionId, Signedness};
use claw_resolver::{ItemId, ResolvedComponent};

use super::{CodeGenerator, GenerationError, };

use cranelift_entity::EntityRef;
use wasm_encoder as enc;
use wasm_encoder::Instruction;

/// A simple helper that calls EncodeExpression::encode
pub fn encode_expression(
    generator: &CodeGenerator,
    component: &ResolvedComponent,
    expression: ExpressionId,
    func: FunctionId,
    builder: &mut enc::Function,
) -> Result<(), GenerationError> {
    let expr = component.component.expr().get_exp(expression);
    expr.encode(generator, component, expression, func, builder)?;
    Ok(())
}

pub trait EncodeExpression {
    fn encode(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError>;
}

impl EncodeExpression for ast::Expression {
    fn encode(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let expr: &dyn EncodeExpression = match self {
            ast::Expression::Identifier(expr) => expr,
            ast::Expression::Literal(expr) => expr,
            ast::Expression::Call(expr) => expr,
            ast::Expression::Unary(expr) => expr,
            ast::Expression::Binary(expr) => expr,
        };
        expr.encode(generator, component, expression, func, builder)?;
        Ok(())
    }
}

impl EncodeExpression for ast::Identifier {
    fn encode(
        &self,
        _generator: &CodeGenerator,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let resolver = component.resolved_funcs.get(&func).unwrap();
        match resolver.bindings.get(&self.ident).unwrap() {
            ItemId::Import(_) => unimplemented!(),
            ItemId::Global(global) => {
                builder.instruction(&Instruction::GlobalGet(global.index() as u32));
            }
            ItemId::Param(param) => {
                let local_index = param.index();
                builder.instruction(&Instruction::LocalGet(local_index as u32));
            }
            ItemId::Local(local) => {
                let func = component.component.functions.get(func).unwrap();
                let local_index = local.index() + func.arguments.len();
                builder.instruction(&Instruction::LocalGet(local_index as u32));
            }
            ItemId::Function(_) => unimplemented!(),
        }
        Ok(())
    }
}

impl EncodeExpression for ast::Literal {
    fn encode(
        &self,
        _generator: &CodeGenerator,
        component: &ResolvedComponent,
        expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let comp = &component.component;
        let resolver = component.resolved_funcs.get(&func).unwrap();

        let rtype = resolver.get_resolved_type(expression, comp)?;
        let valtype = super::rtype_to_core_valtype(rtype, &component.component);

        use ast::Literal;
        let instruction = match (valtype, self) {
            (enc::ValType::I32, Literal::Integer(value)) => Instruction::I32Const(*value as i32),
            (enc::ValType::I64, Literal::Integer(value)) => Instruction::I64Const(*value as i64),
            (enc::ValType::F32, Literal::Float(value)) => Instruction::F32Const(*value as f32),
            (enc::ValType::F64, Literal::Float(value)) => Instruction::F64Const(*value),
            _ => todo!(),
        };
        builder.instruction(&instruction);
        Ok(())
    }
}

impl EncodeExpression for ast::Call {
    fn encode(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        for arg in self.args.iter() {
            encode_expression(generator, component, *arg, func, builder)?;
        }
        let resolver = component.resolved_funcs.get(&func).unwrap();
        let index = match resolver.bindings.get(&self.ident).unwrap() {
            ItemId::Import(import) => *generator.func_idx_for_import.get(import).unwrap(),
            ItemId::Function(function) => *generator.func_idx_for_func.get(function).unwrap(),
            _ => panic!(""),
        };
        builder.instruction(&Instruction::Call(index.into()));
        Ok(())
    }
}

impl EncodeExpression for ast::UnaryExpression {
    fn encode(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        builder.instruction(&enc::Instruction::I32Const(0));
        encode_expression(generator, component, self.inner, func, builder)?;
        builder.instruction(&enc::Instruction::I32Sub);
        Ok(())
    }
}

const S: Signedness = Signedness::Signed;
const U: Signedness = Signedness::Unsigned;

impl EncodeExpression for ast::BinaryExpression {
    fn encode(
        &self,
        generator: &CodeGenerator,
        component: &ResolvedComponent,
        _expression: ExpressionId,
        func: FunctionId,
        builder: &mut enc::Function,
    ) -> Result<(), GenerationError> {
        let comp = &component.component;
        encode_expression(generator, component, self.left, func, builder)?;
        encode_expression(generator, component, self.right, func, builder)?;

        let resolver = component.resolved_funcs.get(&func).unwrap();
        let rtype = resolver.get_resolved_type(self.left, comp)?;

        let ptype = crate::rtype_to_ptype(rtype, comp).unwrap();

        let core_valtype = crate::ptype_to_core_valtype(ptype);
        let instruction = match (self.op, core_valtype, ptype.signedness()) {
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
        builder.instruction(&instruction);

        if let Some(mask) = ptype.core_type_mask() {
            builder.instruction(&enc::Instruction::I32Const(mask));
            builder.instruction(&enc::Instruction::I32And);
        }

        Ok(())
    }
}
