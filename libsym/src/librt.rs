
use std::rc::Rc;
use super::Definition;
use super::Info;
use super::Scope;
use super::Function;
use libast::DataType;
use libast::IntType::*;
use libast::FloatType::*;

fn function(s: &mut Scope, name: &str, args: Vec<DataType>,
            ret: DataType)
{
    let f = DataType::Function(args, Box::new(ret));
    let d = Definition::Function(Function::new(false, true));
    let info = Info::new(d, Rc::new(f), true);
    s.insert(name.into(), info);
}

pub fn insert(mut s: Scope) -> Scope
{
    function(&mut s, "print", vec![DataType::String], DataType::Unit);
    function(&mut s, "iprint", vec![DataType::Integer(S64)], DataType::Unit);
    function(&mut s, "exit", vec![DataType::Integer(S32)], DataType::Unit);
    s
}
