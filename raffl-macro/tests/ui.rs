#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    // t.compile_fail("tests/ui/*.rs");
    t.pass("tests/ui/test_ui_test.rs");
}
