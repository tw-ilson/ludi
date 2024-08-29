use std::process::Command;

#[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
pub mod runtime_x86 {
    pub fn cpuid() {
        // let information = cupid::master();
        // println!("{:#?}", information);
        // if let Some(information) = information {
        //     if information.avx2() {
        //         println!("AVX2 Available");
        //     }
        // }
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
    use core::arch::aarch64;
    use core::arch::asm;

    #[cfg(any(target_feature = "sve", target_feature = "sve2"))]
    pub fn sve() {
        println!("success");
    }
    #[cfg(target_feature = "neon")]
    pub unsafe fn neon() {
        use std::arch::aarch64::{uint8x8_t, vadd_u8};
        // use core::core_arch::arm_shared::neon;

        let a: uint8x8_t = bytemuck::cast([1u8, 1u8, 1u8, 1u8, 1u8, 1u8, 1u8, 1u8]);
        let b: uint8x8_t = bytemuck::cast([2u8, 2u8, 2u8, 2u8, 2u8, 2u8, 2u8, 2u8]);
        let c: uint8x8_t;
        c = vadd_u8(a, b);

        // asm!(
        // "vadd.u8 {c:d} {a:d} {b:d}",
        // a = in(vreg) a,
        // b = in(vreg) b,
        // c = out(vreg) c,
        // );
        println!("{:?}", c);
    }
    #[cfg(target_feature = "sme")]
    pub fn sme() {
        println!("success");
    }
    #[cfg(target_vendor = "apple")]
    pub fn amx() {
        println!("success");
    }
}
