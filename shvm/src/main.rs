use std::mem::size_of;

use shvm::hwquery::runtime_arm;

fn main() {
    // runtime_arm::neon();
    println!("{}", size_of::<libludi::atomic::NumberType>())
}
