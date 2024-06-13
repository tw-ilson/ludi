use std::process::Command;


#[cfg(
    all(
        target_arch = "x86_64",
        target_feature = "avx2"
    )
)]
fn avx() {
    #[cfg(target_arch = "x86_64")]
    use std::arch::x86_64::_mm256_add_epi64;

    unsafe {
        // _mm256_add_epi64(...);
    }
}


pub fn cpuid() {
    let information = cupid::master();
    println!("{:#?}", information);
    if let Some(information) = information {
        if information.avx2() {
             println!("AVX2 Available");
        }
    }
}

