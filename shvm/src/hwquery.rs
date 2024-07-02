use std::process::Command;


#[cfg(any(target_arch = "x86_64", target_arch="x86"))]
pub mod runtime_x86 {
    pub fn cpuid() {
        let information = cupid::master();
        println!("{:#?}", information);
        if let Some(information) = information {
            if information.avx2() {
                 println!("AVX2 Available");
            }
        }
    }
    #[cfg(target_feature = "avx2")]
    fn avx() {
        use std::arch::x86_64::_mm256_add_epi64;

        unsafe {
            // _mm256_add_epi64(...);
        }
    }
}


#[cfg(target_arch = "aarch64")]
pub mod runtime_arm {
    #[cfg(any(target_feature = "sve2"))]
    pub fn sve() {
        println!("success");
    }
    #[cfg(target_feature="neon")]
    pub fn neon() {
        println!("success");
    }
    #[cfg(target_feature="sme")]
    pub fn sme() {
        println!("success");
    }
    #[cfg(target_vendor="apple")]
    pub fn amx() {
        println!("success");
    }
}

