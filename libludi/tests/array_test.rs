use libludi::array::*;
use libludi::data::{Data, DataType};
use libludi::array::ArrayType;
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
    a.reshape(&[3,3]);
    b.reshape(&[3,3]);
    let c = Add::add(
                DataType::Array(ArrayType::UInt32(a)),
                DataType::Array(ArrayType::UInt32(b)))
        .unwrap();
    println!("{}", c);
    panic!();
}
