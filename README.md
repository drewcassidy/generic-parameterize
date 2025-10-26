> [!IMPORTANT] 
> Migrated to https://git.offworldcolonies.nexus/drewcassidy/generic-parameterize

Generic Parameterize
====================

This crate provides the `parameterize` macro, which allow parameterizing generic functions for applications like unit
testing.

for example,

```rust
use generic_parameterize::parameterize;
use std::fmt::Debug;

#[parameterize(T = (i32, f32), N = [4, 5, 6])]
#[test]
fn test_array<T: Default, const N: usize>() where [T; N]: Default + Debug {
    let foo: [T; N] = Default::default();
    println!("{:?}", foo)
}
 ```

generates a module called `test_array` containing functions called `test_array_i32_4`, `test_array_i32_5` etc.
The `#[test]` attribute gets copied to the child functions, which in turn call a copy of `test_array`. The result looks
like:

```rust
mod test_array {
    use std::println;
    fn test_array<T: Default, const N : usize>() where [T;N]: Default + std::fmt::Debug{
         let foo: [T;N] = Default::default();
         println!("{:?}", foo)
    }

    #[test]
    fn test_array_i32_4() {test_array::<i32,4>();}
    #[test]
    fn test_array_f32_4() {test_array::<f32,4>();}
    #[test]
    fn test_array_i32_5() {test_array::<i32,5>();}
    // etc...
 }
