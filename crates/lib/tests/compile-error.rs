use compile_claw::compile;
use miette::{GraphicalReportHandler, GraphicalTheme};

use std::fs;

use wit_parser::Resolve;

#[test]
fn test_bad_programs() {
    for f in fs::read_dir("./tests/bad-programs").unwrap() {
        let f = f.unwrap();
        let source_name = f.file_name().into_string().unwrap();

        if source_name.ends_with(".error.txt") {
            continue; // skip error files
        }

        assert!(source_name.ends_with(".claw"));

        let source_code = fs::read_to_string(f.path()).unwrap();

        let mut error_file_path = f.path();
        error_file_path.set_extension("error.txt");
        let error_file_contents = fs::read_to_string(error_file_path).unwrap();

        let wit = Resolve::new();

        let result = compile(source_name.clone(), &source_code, wit);
        match result {
            Ok(_) => {
                eprintln!(
                    "File '{}' compiled without error when the following error was expected:",
                    source_name
                );
                eprintln!("{}", error_file_contents);
                panic!()
            }
            Err(error) => {
                let mut error_string = String::new();
                GraphicalReportHandler::new_themed(GraphicalTheme::none())
                    .render_report(&mut error_string, &error)
                    .unwrap();
                assert_eq!(error_string, error_file_contents);
            }
        }
    }
}
