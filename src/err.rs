use std::fmt::Display;

pub type Span = (usize, usize);

pub fn error_maybe<T>(v: Result<T, impl Display>, msg: String) -> T {
    match v {
        Ok(v) => v,
        Err(e) => {
            eprintln!("jace: {msg}: {err_msg}", err_msg = e.to_string());

            std::process::exit(1);
        }
    }
}
