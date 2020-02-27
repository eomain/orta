
use std::io::Write;
use std::io::Result;
use std::process::Command;
use std::process::{ Output, Stdio };

pub fn exec(command: &[&str])
{
    let (name, args) = command.split_first().unwrap();
    let mut c = Command::new(name)
                        .stdin(Stdio::null())
                        .stdout(Stdio::null())
                        .args(args)
                        .spawn()
                        .expect("");
    c.wait();
}

#[derive(Debug, Clone)]
pub struct Pipeline<'a> {
    input: &'a str,
    commands: Vec<&'a [&'a str]>
}

impl<'a> Pipeline<'a> {
    pub fn new(input: &'a str) -> Self
    {
        Self {
            input,
            commands: Vec::new()
        }
    }

    pub fn add(&mut self, cmd: &'a [&'a str])
    {
        self.commands.push(cmd);
    }

    fn len(&self) -> usize
    {
        self.commands.len()
    }

    pub fn run(&self) //-> Result<Output>
    {
        //assert_eq!(self.len() > 1, true);
        let end = self.len() - 1;

        let (name, args) = self.commands[0].split_first().unwrap();
        let child = Command::new(name)
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .args(args)
                            .spawn()
                            .expect("");

        {
            let mut stdin = child.stdin.unwrap();
            write!(stdin, "{}", self.input);
        }
        if self.len() == 1 {
            return;
        }
        let mut stdout = child.stdout.unwrap();

        for c in (&self.commands[1..end]) {
            let (name, args) = c.split_first().unwrap();
            let child = Command::new(name)
                                .stdin(Stdio::from(stdout))
                                .stdout(Stdio::piped())
                                .args(args)
                                .spawn()
                                .expect("");
            stdout = child.stdout.unwrap();
        };

        let (name, args) = self.commands[end].split_first().unwrap();
        let mut child = Command::new(name)
                            .stdin(Stdio::from(stdout))
                            .stdout(Stdio::piped())
                            .args(args)
                            .spawn()
                            .expect("");
        child.wait();
    }

    pub fn run_with<F>(&self, f: F)
        where F: Fn()
    {
        self.run();
        f();
    }
}

impl<'a> From<(&'a str, &'a [&'a [&'a str]])> for Pipeline<'a> {
    fn from(s: (&'a str, &'a [&'a [&'a str]])) -> Self
    {
        let input = s.0;
        let mut p = Pipeline::new(input);
        for c in (s.1) {
            p.add(c);
        }
        p
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static RT: &str = include_str!("../rt.ll");

    static INPUT: &str = r#"
        declare i32 @printf(i8*, ...)
        @.str = constant [14 x i8] c"hello, world\0A\00"

        define void @main() {
            %1 = getelementptr [14 x i8], [14 x i8]* @.str, i32 0, i32 0
            %2 = call i32 (i8*, ...) @printf( i8* %1 ) nounwind
            ret void
        }
    "#;

    #[test]
    fn pipe()
    {
        let llc = vec!["llc"];
        {
            let r#as = vec!["as", "-o", "rt.o"];
            let mut pipeline = Pipeline::new(RT);
            pipeline.add(&llc);
            pipeline.add(&r#as);
            pipeline.run();
        }

        {
            let r#as = vec!["as", "-o", "test.o"];
            let mut pipeline = Pipeline::new(INPUT);
            pipeline.add(&llc);
            pipeline.add(&r#as);
            pipeline.run();
        }

        let ld = vec![
            "ld", "--dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
            "test.o", "rt.o", "-lc", "-o", "test"
        ];
        exec(&ld);

        let files = vec![
            "rt.o", "test.o"
        ];
        for f in &files {
            std::fs::remove_file(f);
        }
    }
}
