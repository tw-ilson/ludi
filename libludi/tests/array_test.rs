use libludi::array::*;

#[test]
fn test_indexing() {
    let mut a: Array<u32> = Iota::iota(24);
    a.reshape(&[4, 3, 2]).unwrap();
    println!("{}", a);
    assert_eq!(Some(&8), a.get(&[2, 1, 0]))
}
