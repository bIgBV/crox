use std::sync::RwLock;

use miette::SourceSpan;

use crate::memory::Offset;

#[derive(Debug)]
pub struct LineStore {
    spans: RwLock<Vec<Span>>,
}

impl LineStore {
    pub fn new() -> Self {
        Self {
            spans: RwLock::new(Vec::new()),
        }
    }

    pub fn add_byte(&self, line: usize) {
        if let Some(idx) = self.find_line(line) {
            let span = &mut self.spans.write().unwrap()[idx];
            span.count += 1
        } else {
            let span = Span { line, count: 1 };
            self.spans.write().unwrap().push(span);
        }
    }

    pub fn add_bytes(&self, line: usize, count: usize) {
        if let Some(idx) = self.find_line(line) {
            let span = &mut self.spans.write().unwrap()[idx];
            span.count += count;
        } else {
            let span = Span { line, count };
            self.spans.write().unwrap().push(span);
        }
    }

    /// Returns the associated line for the given byte offset if available
    pub fn get_line(&self, byte_offset: Offset) -> Option<usize> {
        let mut sum: usize = 0;
        // TODO: This can be optimized. This is currently going to be an n^2 algo
        // because of how it's called.
        for span in &*self.spans.read().unwrap() {
            if (sum..sum + span.count).contains(&byte_offset.0) {
                return Some(span.line);
            }
            sum += span.count;
        }

        None
    }

    fn find_line(&self, line: usize) -> Option<usize> {
        self.spans
            .read()
            .unwrap()
            .binary_search_by(|span| span.line.cmp(&line))
            .ok()
    }
}

/// Represents a span on code
#[derive(Debug)]
pub struct Span {
    /// The associated line from the source code
    line: usize,

    /// The number of bytes of bytecode contained in this span
    count: usize,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn insert_byte_range() {
        let store = LineStore::new();

        for chunk in (0..356).collect::<Vec<usize>>().chunks(14) {
            for i in chunk {
                store.add_byte(*i);
            }
        }

        for i in 0..356 {
            assert!(store.get_line(Offset(i as usize)).is_some())
        }
    }

    #[test]
    fn search_outside_range() {
        let store = LineStore::new();

        assert!(store.get_line(Offset(67)).is_none())
    }

    #[test]
    fn add_bytes() {
        let store = LineStore::new();
        let mut lines = Vec::new();

        let byte_slices = (0..356).collect::<Vec<usize>>();

        for (idx, chunk) in byte_slices.chunks(14).enumerate() {
            store.add_bytes(idx, chunk.len());
            lines.push(idx);
        }

        for i in byte_slices {
            let line = store.get_line(Offset(i));
            assert!(line.is_some());

            let expected_line = if i < 14 { 0 } else { i / 14 };
            assert_eq!(line, Some(expected_line));
        }
    }
}
