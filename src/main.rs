use windows::Win32::System::Memory::{VirtualProtect, PAGE_EXECUTE_READWRITE};

const EXE_CODE: &[u8] = include_bytes!("../machine_code.bin");

type ExeCodeFn = extern "system" fn() -> usize;

fn main() {
    let exe = EXE_CODE.to_vec();
    let mut old = Default::default();
    if !unsafe {
        VirtualProtect(
            exe.as_ptr() as _,
            exe.len(),
            PAGE_EXECUTE_READWRITE,
            &mut old,
        )
    }
    .as_bool()
    {
        panic!("VirtualProtect failed");
    }

    let exe_fn: ExeCodeFn = unsafe { std::mem::transmute(exe.as_ptr()) };
    let value = exe_fn();
    println!("==> value is: {}", value);

    println!("Hello, world!");
}
