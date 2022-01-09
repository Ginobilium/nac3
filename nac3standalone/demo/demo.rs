#[no_mangle]
pub extern "C" fn output_int32(x: i32) {
    println!("{}", x);
}

#[no_mangle]
pub extern "C" fn output_int64(x: i64) {
    println!("{}", x);
}

#[no_mangle]
pub extern "C" fn output_asciiart(x: i32) {
    let chars = " .,-:;i+hHM$*#@  ";
    if x < 0 {
        println!("");
    } else {
        print!("{}", chars.chars().nth(x as usize).unwrap());
    }
}

extern "C" {
    fn run() -> i32;
}

fn main() {
    unsafe {
        run();
    }
}
