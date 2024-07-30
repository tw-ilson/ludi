use libludi::array::ArrayType;
use libludi::array::*;
use libludi::atomic::AtomicType;
use libludi::data::{Data, DataType};
use libludi::err::Result;
use libludi::ops::*;

#[test]
fn test_indexing() {
    let mut a: Array<u32> = Iota::iota(24);
    a.reshape(&[4, 3, 2]).unwrap();
    println!("{}", a);
    assert_eq!(Some(&8), a.get(&[2, 1, 0]))
}
#[test]
fn test_add() {
    let mut a: Array<u32> = Iota::iota(9);
    let mut b: Array<u32> = Iota::iota(9);
    a.reshape(&[3, 3]);
    b.reshape(&[3, 3]);
    let c = Add::add(
        DataType::Array(ArrayType::UInt32(a)),
        DataType::Array(ArrayType::UInt32(b)),
    )
    .unwrap();
}

// #[test]
// fn test_types() {
//     let a = DataType::Array(ArrayType::UInt8(Array::new(&[], &[0])));
//     let b = DataType::Atomic(AtomicType::Boolean(false));
//     let c = DataType::Atomic(AtomicType::Character('c'));
//     assert_ne!(std::mem::discriminant(&b), std::mem::discriminant(&c))
// }

#[test]
fn test_frame() {
    let mut a = ArrayType::UInt8(Iota::iota(100));
    a.reshape(&[10,10]);
    let mut b = ArrayType::UInt8(Iota::iota(100));
    b.reshape(&[10,10]);

    let c = Result::<ArrayType>::from_iter(vec![a, b]).unwrap();
    assert_eq!(&[2,10,10], c.shape());
    println!("{}", c);
}
