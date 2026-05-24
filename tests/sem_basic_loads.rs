use jigglefab::chemistry::load_chemistry_compiled;

#[test]
fn sem_basic_compiles() {
    let compiled = load_chemistry_compiled("chemistries/sem_basic.toml").unwrap();
    // Program "empty" was registered.
    assert!(!compiled.program_offset.is_empty());
}
