use anyhow::Result;
use raffl_macro::{callback_wrappers, params};
struct Test {}

impl Test {
    pub fn new() -> Self {
        Self {}
    }
}

#[callback_wrappers(pub, unwrap_result)]
impl Test {
    #[params(!slf: *mut std::ffi::c_void, ...)]
    pub fn test(&self, a: i32, b: i32) -> i32 {
        a + b
    }

    #[params(!slf: *mut std::ffi::c_void, ...)]
    pub fn test_result(&self, a: i32, b: i32) -> Result<i32> {
        Ok(a + b)
    }
}

impl<'a> From<*mut std::ffi::c_void> for &'a mut Test {
    fn from(ptr: *mut std::ffi::c_void) -> &'a mut Test {
        unsafe { *(ptr as *mut Self) }
    }
}

// #[test]
fn main() {
    let t = Test::new();
    let t_ptr = &t as *const Test as *mut std::ffi::c_void;
    println!("{}", test_callbacks::test(t_ptr, 1, 2));
    test_callbacks::test_result(t_ptr, 1, 2);
}
