const EXE_CODE_LEN: usize = include_bytes!("../../machine_code.bin").len();

#[link_section = ".text"]
static EXE_CODE: [u8; EXE_CODE_LEN] = *include_bytes!("../../machine_code.bin");

type ExeCodeFn = extern "system" fn() -> usize;

fn main() {
    let exe_fn: ExeCodeFn = unsafe { std::mem::transmute(EXE_CODE.as_ptr()) };
    let value = exe_fn();
    println!("==> value is: {}", value);

    println!("Hello, world!");
}
