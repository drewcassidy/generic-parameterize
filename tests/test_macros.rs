use generic_parameterize::parameterize;


#[parameterize(A = (String,), B = [42, 1312])]
fn do_the_thing<A, const B: usize>() {
    println!("Hello!")
}