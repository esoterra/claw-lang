use ast::{FnTypeInfo, TypeId};
use claw_ast as ast;

use wasm_encoder as enc;

use crate::{
    types::{align_to, EncodeType},
    ModuleBuilder, ModuleTypeIndex,
};

const MAX_FLAT_PARAMS: u8 = 16;
const MAX_FLAT_RESULTS: u8 = 1;

pub struct FunctionGenerator {
    pub params: Vec<ParamInfo>,
    pub param_types: Vec<enc::ValType>,
    pub return_type: ReturnInfo,
}

impl FunctionGenerator {
    pub fn new<FnType: FnTypeInfo>(fn_type: &FnType, comp: &ast::Component) -> Self {
        // Layout parameters
        let ParamConfig {
            params,
            param_types,
        } = prepare_params(fn_type, comp);

        // Layout return types
        let return_type = fn_type.get_return_type();
        let return_type = prepare_return_type(return_type, comp);

        Self {
            params,
            param_types,
            return_type,
        }
    }

    pub fn encode_func_type(&self, module: &mut ModuleBuilder) -> ModuleTypeIndex {
        let params = self.param_types.iter().copied();
        match self.return_type {
            ReturnInfo::Flat(return_type) => module.func_type(params, [return_type]),
            ReturnInfo::Spilled => module.func_type(params, [enc::ValType::I32]),
            ReturnInfo::None => module.func_type(params, []),
        }
    }
}

pub struct ParamConfig {
    pub params: Vec<ParamInfo>,
    pub param_types: Vec<enc::ValType>,
}

pub enum ParamInfo {
    Local(LocalParamInfo),
    Spilled(SpilledParamInfo),
}

pub struct LocalParamInfo {
    pub index_offset: u32,
}

pub struct SpilledParamInfo {
    pub mem_offset: u32,
}

fn prepare_params<FnType: FnTypeInfo>(fn_type: &FnType, comp: &ast::Component) -> ParamConfig {
    // Flatten parameters
    let mut flat_params = Vec::new();
    for (_name, type_id) in fn_type.get_args() {
        type_id.append_flattened(comp, &mut flat_params);
    }
    // Either generate as locals or spill to memory based on flattened size
    if flat_params.len() <= MAX_FLAT_PARAMS as usize {
        let params = param_info_local(fn_type, comp);
        let param_types = flat_params;
        ParamConfig {
            params,
            param_types,
        }
    } else {
        let params = param_info_spilled(fn_type, comp);
        let param_types = vec![enc::ValType::I32];
        ParamConfig {
            params,
            param_types,
        }
    }
}

fn param_info_local<FnType: FnTypeInfo>(fn_type: &FnType, comp: &ast::Component) -> Vec<ParamInfo> {
    let mut params = Vec::new();
    let mut index_offset = 0;
    for (_name, type_id) in fn_type.get_args() {
        params.push(ParamInfo::Local(LocalParamInfo { index_offset }));
        index_offset += type_id.flat_size(comp);
    }
    params
}

fn param_info_spilled<FnType: FnTypeInfo>(
    fn_type: &FnType,
    comp: &ast::Component,
) -> Vec<ParamInfo> {
    let mut params = Vec::new();
    let mut mem_offset = 0;
    for (_name, type_id) in fn_type.get_args() {
        let align = type_id.align(comp);
        let size = type_id.mem_size(comp);
        mem_offset = align_to(mem_offset, align);
        params.push(ParamInfo::Spilled(SpilledParamInfo { mem_offset }));
        mem_offset += size;
    }
    params
}

pub enum ReturnInfo {
    Flat(enc::ValType),
    Spilled,
    None,
}

impl ReturnInfo {
    pub fn spill(&self) -> bool {
        match self {
            ReturnInfo::Flat(_) | ReturnInfo::None => false,
            ReturnInfo::Spilled => true,
        }
    }
}

fn prepare_return_type(return_type: Option<TypeId>, comp: &ast::Component) -> ReturnInfo {
    if let Some(return_type) = return_type {
        if return_type.flat_size(comp) > MAX_FLAT_RESULTS as u32 {
            ReturnInfo::Spilled
        } else {
            let return_types = return_type.flatten(comp);
            assert_eq!(return_types.len(), 1);
            ReturnInfo::Flat(return_types[0])
        }
    } else {
        ReturnInfo::None
    }
}
