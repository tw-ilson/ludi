use std::{rc::Rc, time::{SystemTime, UNIX_EPOCH}};
use libludi::err::Result;
use crate::{
    datatypes::{AtomicType, DataType},
    function::Callable,
    interpret::DynamicEnv,
};

struct Clock;
impl Callable for Clock {
    fn call(self, _a: Vec<DataType>, _e: &mut DynamicEnv) -> Result<DataType> {
        Ok(DataType::Atomic(AtomicType::Index(
            SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as usize,
        )))
    }
}
const CLOCK: Clock = Clock;
