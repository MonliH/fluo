use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lib::logger;
use lib::parser;

use std::cell::RefCell;
use std::path;
use std::rc::Rc;

#[allow(unused_must_use)]
fn criterion_benchmark(c: &mut Criterion) {
    let logger = Rc::new(RefCell::new(logger::logger::Logger::new(true)));

    c.bench_function("parser simple", |b| {
        b.iter(|| {
            let mut my_parser = parser::parser::Parser::new(
                black_box(path::Path::new("my_long_filename_test_this_is_really_long_but_its_for_a_Test_so_who_cares.fl")),
                black_box(r#"/*	
hlleoa1234567890qwertyuiopasdfghjklzxcvbnm,./?><;'":[]\|}{=-0987654321`~!@#$%^&*()_+	
one two	
djawd	
sfghsdjajdksajfiwjijfa	
*/	

def other() {	
    def other_1() -> () {	
        return 10;	
    }	
    let x: int = 10;	

    def other_1() -> int {	
        return 10;	
    }

    let _qwertyuiopasdfghjklzxcvbnm: int;	
    _qwertyuiopasdfghjklzxcvbnm = "hi";	
    hi(10, 102, _qwertyuiopasdfghjklzxcvbnm+"other hi", x);	
}
"#),
                Rc::clone(&logger),
            );
            my_parser.initialize_expr();
            my_parser.parse();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
