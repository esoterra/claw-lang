use claw_ast as ast;

use claw_resolver::types::ResolvedType;
use wasm_encoder as enc;

use crate::{
    types::{align_to, EncodeType},
    builders::{
        module::{ModuleBuilder, ModuleTypeIndex},
        component::{ComponentBuilder, ComponentTypeIndex}
    }
};

const MAX_FLAT_PARAMS: u8 = 16;
const MAX_FLAT_RESULTS: u8 = 1;

pub struct FunctionGenerator {
    pub spill_params: bool,
    pub params: Vec<ParamInfo>,
    pub flat_params: Vec<enc::ValType>,
    pub results: Option<ResultsInfo>,
}

pub struct ParamInfo {
    pub name: String,
    pub rtype: ResolvedType,
    pub index_offset: u32,
    pub mem_offset: u32,
}

impl FunctionGenerator {
    pub fn new(
        params: Vec<(String, ResolvedType)>,
        results: Option<ResolvedType>,
        comp: &ast::Component,
    ) -> Self {
        // Layout parameters
        let (spill_params, params, flat_params) = prepare_params(params, comp);
        // Layout return types
        let results = results.map(|results| ResultsInfo {
            rtype: results,
            spill: ResultSpillInfo::new(results, comp),
        });

        Self {
            spill_params,
            params,
            flat_params,
            results,
        }
    }

    pub fn encode_comp_type(
        &self,
        builder: &mut ComponentBuilder,
        comp: &ast::Component,
    ) -> ComponentTypeIndex {
        let params = self
            .params
            .iter()
            .map(|info| (info.name.as_str(), info.rtype.to_comp_valtype(comp)));
        let result = self.results.as_ref().map(|info| info.rtype.to_comp_valtype(comp));
        builder.func_type(params, result)
    }

    pub fn encode_mod_type(&self, builder: &mut ModuleBuilder) -> ModuleTypeIndex {
        let params = self.flat_params.iter().copied();
        type RSI = ResultSpillInfo;
        match self.results.as_ref().map(|info| &info.spill) {
            Some(RSI::Flat { valtype }) => builder.func_type(params, [*valtype]),
            Some(RSI::Spilled) => builder.func_type(params, [enc::ValType::I32]),
            None => builder.func_type(params, []),
        }
    }
}

fn prepare_params(
    params: Vec<(String, ResolvedType)>,
    comp: &ast::Component,
) -> (bool, Vec<ParamInfo>, Vec<enc::ValType>) {
    // Flatten parameters
    let mut flat_params = Vec::new();
    let mut mem_offset = 0;
    let mut param_info = Vec::new();

    for (name, rtype) in params {
        let index_offset = flat_params.len() as u32;
        mem_offset = align_to(mem_offset, rtype.align(comp));
        let info = ParamInfo {
            name,
            rtype,
            index_offset,
            mem_offset,
        };
        param_info.push(info);
        rtype.append_flattened(comp, &mut flat_params);
        mem_offset += rtype.mem_size(comp);
    }
    // Either generate as locals or spill to memory based on flattened size
    let spill = flat_params.len() > MAX_FLAT_PARAMS as usize;
    let flat_params = match spill {
        true => vec![enc::ValType::I32],
        false => flat_params,
    };
    (spill, param_info, flat_params)
}

pub struct ResultsInfo {
    pub rtype: ResolvedType,
    pub spill: ResultSpillInfo,
}

impl ResultsInfo {
    pub fn spill(&self) -> bool {
        self.spill.spill()
    }
}

pub enum ResultSpillInfo {
    Flat { valtype: enc::ValType },
    Spilled,
}

impl ResultSpillInfo {
    pub fn new(rtype: ResolvedType, comp: &ast::Component) -> Self {
        if rtype.flat_size(comp) > MAX_FLAT_RESULTS as u32 {
            ResultSpillInfo::Spilled
        } else {
            let result_types = rtype.flatten(comp);
            assert_eq!(result_types.len(), 1);
            let valtype = result_types[0];
            ResultSpillInfo::Flat { valtype }
        }
    }

    pub fn spill(&self) -> bool {
        matches!(self, ResultSpillInfo::Spilled)
    }

    pub fn valtype(&self) -> enc::ValType {
        match self {
            ResultSpillInfo::Flat { valtype } => *valtype,
            ResultSpillInfo::Spilled => enc::ValType::I32,
        }
    }
}
