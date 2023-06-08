/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

pub(crate) trait Extract {
    type Item;
    /// Removes all elements of `self` for which `f(&e)` returns `true`, and returns them as an iterator
    fn extract<F: FnMut(&Self::Item) -> bool>(&mut self, f: F) -> ExtractIterator<Self::Item, F>;

    fn extract_map<U, F: FnMut(&Self::Item) -> Option<U>>(
        &mut self,
        f: F,
    ) -> ExtractMap<Self::Item, U, F>;
}

impl<T> Extract for Vec<T> {
    type Item = T;
    fn extract<F: FnMut(&Self::Item) -> bool>(&mut self, f: F) -> ExtractIterator<Self::Item, F> {
        ExtractIterator {
            vec: self,
            f,
            index: 0,
        }
    }

    fn extract_map<U, F: FnMut(&Self::Item) -> Option<U>>(
        &mut self,
        f: F,
    ) -> ExtractMap<Self::Item, U, F> {
        ExtractMap {
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

pub(crate) struct ExtractMap<'a, T, U, F: FnMut(&T) -> Option<U>> {
    vec: &'a mut Vec<T>,
    f: F,
    index: usize,
}

impl<'a, T, U, F: FnMut(&T) -> Option<U>> Iterator for ExtractMap<'a, T, U, F> {
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.vec.len() {
            if let Some(res) = (self.f)(self.vec.get(self.index)?) {
                Some(self.vec.remove(self.index));
                return Some(res);
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
