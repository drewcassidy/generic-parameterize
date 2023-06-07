/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

pub(crate) trait Extract<T, F: FnMut(&T) -> bool> {
    /// Removes all elements of `self` for which `f(&e)` returns `true`, and returns them as an iterator
    fn extract(&mut self, f: F) -> ExtractIterator<T, F>;
}

impl<T, F: FnMut(&T) -> bool> Extract<T, F> for Vec<T> {
    fn extract(&mut self, f: F) -> ExtractIterator<T, F> {
        ExtractIterator {
            vec: self,
            f,
            index: 0,
        }
    }
}

pub(crate) struct ExtractIterator<'a, T, F: FnMut(&T) -> bool> {
    vec: &'a mut Vec<T>,
    f: F,
    index: usize,
}

impl<'a, T, F: FnMut(&T) -> bool> Iterator for ExtractIterator<'a, T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.vec.len() {
            if (self.f)(self.vec.get(self.index)?) {
                return Some(self.vec.remove(self.index));
            }
            self.index += 1;
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::extract::Extract;
    use itertools::Itertools;

    #[test]
    fn extract() {
        // https://oeis.org/A122115
        let mut myvec = vec![-3, -1, 4, 8, 15, 16, 23, 42, 66, 104];

        let removed = myvec.extract(|i| i % 2 == 0).collect_vec(); // extract all odd numbers
        assert_eq!(removed, vec![4, 8, 16, 42, 66, 104]);
        assert_eq!(myvec, vec![-3, -1, 15, 23]);
    }
}
