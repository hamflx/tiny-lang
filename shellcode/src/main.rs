extern "system" {
    fn main_1() -> usize;
}

fn main() {
    let value = unsafe { main_1() };
    println!("==> value is: {}", value);

    println!("Hello, world!");
}
