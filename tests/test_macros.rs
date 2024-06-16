/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use generic_parameterize::parameterize;
use std::any::TypeId;
use std::collections::HashSet;

use std::process::{ExitCode, Termination};

/// Allows getting data out of a unit test by wrapping it in a Termination.
/// Played by Arnold Schwarzenegger.
#[derive(PartialEq, Eq, Hash)]
pub struct Terminator<T> {
    pub data: Box<T>,
}

impl<T> Termination for Terminator<T> {
    fn report(self) -> ExitCode {
        ExitCode::SUCCESS
    }
}

/// Generates 6 parameterized tests using a matrix of types and constants
#[parameterize(A = (i32, f32), B = [5, 6, 7], fmt = "{fn}_{B}ｘ{A}")]
#[test]
fn test_generic_matrix<A: 'static, const B: usize>() -> Terminator<(TypeId, usize)> {
    return Terminator {
        data: Box::new((TypeId::of::<A>(), B)),
    };
}

/// Tests to make sure the previous test expanded to the correct subtests
#[test]
fn metatest_generic_matrix() {
    let mut results: HashSet<_> = test_generic_matrix::manifest
        .iter()
        .map(|f| (*f)().data)
        .collect();

    assert_eq!(results.len(), 6);
    assert!(results.remove(&(TypeId::of::<i32>(), 5usize)));
    assert!(results.remove(&(TypeId::of::<i32>(), 6usize)));
    assert!(results.remove(&(TypeId::of::<i32>(), 7usize)));
    assert!(results.remove(&(TypeId::of::<f32>(), 5usize)));
    assert!(results.remove(&(TypeId::of::<f32>(), 6usize)));
    assert!(results.remove(&(TypeId::of::<f32>(), 7usize)));
    assert_eq!(results.len(), 0);
}

/// Generates 2 parameterized tests that say hello and goodbye
#[parameterize(msg=["hello", "goodbye"], n=[1,2,4-1], fmt = "{fn}_say_{msg}_{n}_times")]
#[test]
fn test_fn_matrix(msg: &'static str, n: usize) -> Terminator<(&'static str, usize)> {
    for _ in 0..n {
        println!("{}", msg);
    }
    return Terminator {
        data: Box::new((msg, n)),
    };
}

/// Tests to make sure the previous test expanded to the correct subtests
#[test]
fn metatest_fn_matrix() {
    let mut results: HashSet<_> = test_fn_matrix::manifest
        .iter()
        .map(|f| (*f)().data)
        .collect();

    assert_eq!(results.len(), 6);
    assert!(results.remove(&("hello", 1)));
    assert!(results.remove(&("hello", 2)));
    assert!(results.remove(&("hello", 3)));
    assert!(results.remove(&("goodbye", 1)));
    assert!(results.remove(&("goodbye", 2)));
    assert!(results.remove(&("goodbye", 3)));
    assert_eq!(results.len(), 0);
}
