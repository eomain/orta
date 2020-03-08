
use super::Signature;
use super::TypeInfo;
use super::Scope;
use libast::DataType;
use libast::IntType::*;
use libast::FloatType::*;

pub fn insert(mut s: Scope) -> Scope
{
    s.insert("print", (TypeInfo::Function(
        (vec![DataType::String], DataType::Unit)
    ), true));
    s.insert("iprint", (TypeInfo::Function(
        (vec![DataType::Integer(S64)], DataType::Unit)
    ), true));
    /*s.insert("fprint", (TypeInfo::Function(
        (vec![DataType::Float(F64)], DataType::Unit)
    ), true));*/
    s.insert("exit", (TypeInfo::Function(
        (vec![DataType::Integer(S32)], DataType::Unit)
    ), true));
    s
}
