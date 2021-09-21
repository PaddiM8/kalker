#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    let mut ctx = kalk::parser::Context::new().set_timeout(Some(5));

    // We don't care if it parses or not, we only care about if it panicked
    // while parsing
    let _ = kalk::parser::parse(&mut ctx, data);
});
