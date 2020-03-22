
pub type Lib = String;
pub type Obj = String;

pub struct LinkOpt {
    libs: Vec<Lib>,
    objs: Vec<Obj>,
    output: String,
    extra: Vec<String>
}

impl LinkOpt {

}

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
fn ld_cmd(output: &str) -> Vec<&str>
{
    vec![
        "ld", "-o", output
    ]
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn ld_cmd(output: &str) -> Vec<&str>
{
    vec![
        "ld", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
        "-o", output
    ]
}

// Default program entry point
static ENTRY: &str = "_start";

pub fn ld<'a>(output: &'a str, objs: &mut Vec<&'a str>) -> Vec<&'a str>
{
    let mut v = ld_cmd(output);
    v.append(&mut vec!["-lc", "-e", ENTRY]);
    v.append(objs);
    v
}
