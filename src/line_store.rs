use crate::memory::Offset;

pub struct LineStore {
    spans: Vec<Span>,
}

impl LineStore {
    pub fn new() -> Self {
        Self { spans: Vec::new() }
    }

    pub fn add_byte(&mut self, line: usize) {
        if let Some(idx) = self.find_line(line) {
            let mut span = &mut self.spans[idx];
            span.count += 1
        } else {
            let span = Span { line, count: 1 };

            self.spans.push(span);
        }
    }

    /// Returns the associated line for the given byte offset if available
    pub fn get_line(&self, byte_offset: Offset) -> Option<usize> {
        let mut sum: usize = 0;
        // TODO: This can be optimized. This is currently going to be an n^2 algo
        // because of how it's called.
        for span in &self.spans {
            if (sum..sum + span.count).contains(&byte_offset.0) {
                return Some(span.line);
            }
            sum += span.count;
        }

        None
    }

    fn find_line(&self, line: usize) -> Option<usize> {
        self.spans
            .binary_search_by(|span| span.line.cmp(&line))
            .ok()
    }
}

/// Represents a span on code
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
        let mut store = LineStore::new();

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
}