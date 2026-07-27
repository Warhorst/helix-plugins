use std::fs::File;
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

pub struct FileCreator;

impl FileCreator {
    pub fn create_file(path: String) {
        File::create(path).unwrap();
    }
}

impl Custom for FileCreator {}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("helix-plugins-native/create-file");

    module.register_fn("create-file", FileCreator::create_file);

    module
}
