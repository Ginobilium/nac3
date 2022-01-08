use std::env;

static mut NOW: i64 = 0;

#[no_mangle]
pub extern "C" fn now_mu() -> i64 {
    unsafe { NOW }
}

#[no_mangle]
pub extern "C" fn at_mu(t: i64) {
    unsafe { NOW = t }
}

#[no_mangle]
pub extern "C" fn delay_mu(dt: i64) {
    unsafe { NOW += dt }
}

#[no_mangle]
pub extern "C" fn rtio_init() {
    println!("rtio_init");
}

#[no_mangle]
pub extern "C" fn rtio_get_counter() -> i64 {
    0
}

#[no_mangle]
pub extern "C" fn rtio_output(target: i32, data: i32) {
    println!("rtio_output @{} target={:04x} data={}", unsafe { NOW }, target, data);
}

#[no_mangle]
pub extern "C" fn print_int32(x: i32) {
    println!("print_int32: {}", x);
}

#[no_mangle]
pub extern "C" fn print_int64(x: i64) {
    println!("print_int64: {}", x);
}


fn main() {
    let filename = env::args().nth(1).unwrap();
    unsafe {
        let lib = libloading::Library::new(filename).unwrap();
        let func: libloading::Symbol<unsafe extern fn()> = lib.get(b"__modinit__").unwrap();
        func()
    }
}
