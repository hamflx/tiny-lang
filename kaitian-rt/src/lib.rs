use std::ffi::CStr;

#[no_mangle]
pub fn print(ptr: *const u8) {
    let msg = unsafe { CStr::from_ptr(ptr as _) };
    println!("{}", msg.to_str().unwrap());
}
