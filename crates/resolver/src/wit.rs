use thiserror::Error;

use miette::Diagnostic;

pub use wit_parser::{
    Function, Interface, InterfaceId, PackageName, Resolve, Type, TypeDef, TypeDefKind, TypeId,
    TypeOwner,
};

#[derive(Error, Debug, Diagnostic)]
pub enum WitError {
    #[error("Package {package} does not exist")]
    NoSuchPackage { package: PackageName },
    #[error("Interface {interface} does not exist in package {package}")]
    NoSuchInterface {
        package: PackageName,
        interface: String,
    },
    #[error("Item {item} does not exists in interface {interface}")]
    NoSuchItem { interface: String, item: String },
}

#[derive(Debug)]
pub struct ResolvedWit {
    pub resolve: Resolve,
}

pub enum WitItem {
    Func(Function),
    Type(),
}

impl ResolvedWit {
    pub fn new(resolve: Resolve) -> Self {
        Self { resolve }
    }

    pub fn lookup_interface(
        &self,
        package_name: &PackageName,
        interface: &String,
    ) -> Result<InterfaceId, WitError> {
        let package_id = self.resolve.package_names.get(package_name);
        let package_id = match package_id {
            Some(id) => *id,
            None => {
                return Err(WitError::NoSuchPackage {
                    package: package_name.clone(),
                })
            }
        };

        let package = self.resolve.packages.get(package_id).unwrap();

        let interface_id = package.interfaces.get(interface);
        let interface_id = match interface_id {
            Some(id) => *id,
            None => {
                return Err(WitError::NoSuchInterface {
                    package: package_name.clone(),
                    interface: interface.clone(),
                })
            }
        };

        Ok(interface_id)
    }

    pub fn get_interface(&self, interface_id: InterfaceId) -> &Interface {
        self.resolve.interfaces.get(interface_id).unwrap()
    }

    pub fn lookup_func(&self, interface_id: InterfaceId, name: &str) -> Option<&Function> {
        let interface = self.get_interface(interface_id);
        interface.functions.get(name)
    }

    pub fn lookup_type(&self, interface_id: InterfaceId, name: &str) -> Option<&TypeDef> {
        let interface = self.get_interface(interface_id);
        let type_id = interface.types.get(name)?;
        Some(self.resolve.types.get(*type_id).unwrap())
    }
}
