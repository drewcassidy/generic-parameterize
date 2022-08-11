use generic_parameterize::test_parameterize;


#[test_parameterize(A = (String,), B = [42, 1312])]
fn do_the_thing<A, const B: usize>() {
    println!("Hello!")
}