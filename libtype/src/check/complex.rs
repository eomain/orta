
use std::collections::HashMap;
use super::*;
use libast::ComplexLiteral as Complex;
use libast::StructLiteral as Struct;
use libast::ArrayLiteral as ArrayLit;
use libast::Index;

// Check for indexing into arrays, or pointers in the case of
// pointer arithmetic.
pub fn index(i: &mut Info, s: &mut Scope, e: &mut Index,
             expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;
    expr(i, s, &mut e.expr, None)?;

    let dtype = e.expr.get_type();
    let index = match dtype {
        Array(a) => {
            if !i.is_unsafe() {
                return Err(error!("cannot index [] array, outside an 'unsafe'"));
            }
            if a.sizes.len() > 1 {
                let mut a = (**a).clone();
                a.sizes.remove(0);
                DataType::from(a)
            } else {
                (**a).dtype.clone()
            }
        },
        Pointer(t) => {
            match &**t {
                Array(a) => Pointer(Rc::new((*a).dtype.clone())),
                _ => dtype.clone()
            }
        },
        _ => return Err(error!("expected type: array [], cannot index into `{}`", dtype))
    };

    expr(i, s, &mut e.index, None)?;

    let dtype = e.index.get_type();
    match dtype {
        Integer(_) => (),
        _ => return Err(error!("index error: found type '{}', expected integer type", dtype))
    }

    e.dtype = index;

    Ok(())
}

fn array(i: &mut Info, s: &mut Scope, a: &mut ArrayLit,
         expt: Option<DataType>) -> Result<(), Error>
{
    use DataType::*;

    let el = if let Some(expt) = &expt {
        let dtype = match expt {
            Array(a) => {
                if a.sizes.len() > 1 {
                    let mut a = (**a).clone();
                    a.sizes.remove(0);
                    DataType::from(a)
                } else {
                    a.dtype.clone()
                }
            },
            _ => return Err(error!("expected type: array [], found type: {}", expt))
        };
        Some(dtype)

    } else {
        None
    };

    for e in &mut a.elements {
        expr(i, s, e, el.clone())?;
    }

    let len = a.elements.len();
    let dtype = a.elements[0].get_type();
    match dtype {
        Array(array) => {
            let mut array = (**array).clone();
            array.sizes.insert(0, len);
            a.dtype = DataType::from(array);
        },
        _ => {
            let array = libast::Array::new(vec![len], dtype.clone());
            a.dtype = DataType::from(array);
        }
    }

    Ok(())
}

fn structs(i: &mut Info, s: &mut Scope, c: &mut Struct,
           expt: Option<DataType>) -> Result<(), Error>
{
    let comp = match s.find(&c.name) {
        None => return Err(error!("undefined type struct `{}`", &c.name)),
        Some(info) => {
            match &*info.dtype {
                DataType::Record(r) => r.clone(),
                _ => return Err(error!(""))
            }
        }
    };

    let fields = HashMap::from(&*comp);
    let (a, b) = (c.fields.len(), fields.len());
    if a != b {
        return Err(error!(
            "type struct '{}' defined with {} fields, {} were specified",
            &c.name, b, a
        ));
    }

    for (n, e) in &mut c.fields {
        let dtype = match fields.get(n) {
            None => return Err(error!("field '{}' not defined in type struct '{}'", n, &c.name)),
            Some(t) => (*t).clone()
        };
        expr(i, s, e, Some(dtype))?;
    }

    c.dtype = DataType::from((*comp).clone());

    Ok(())
}

pub fn complex(i: &mut Info, s: &mut Scope, c: &mut Complex,
               expt: Option<DataType>) -> Result<(), Error>
{
    use Complex::*;
    match c {
        Array(a) => array(i, s, a, expt),
        Struct(c) => structs(i, s, c, expt),
    }
}
