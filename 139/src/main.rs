
use std::fmt::Debug;

#[derive(Debug)]
struct PeekableIter<T: Debug + Clone, I: Iterator<Item=T>> {
	iter: I,
	peek_buf: Option<T>
}

impl<T, I> PeekableIter<T, I> where I: Iterator<Item=T>, T: Debug + Clone {
	pub fn from_forward(iter: I) -> Self {
		Self { iter, peek_buf: None }
	}

	pub fn peek(&mut self) -> Option<T> {
		match &self.peek_buf {
			None => {
				self.peek_buf = self.iter.next();
				self.peek_buf.clone()
			}
			Some(t) => Some(t.clone())
		}
	}

	pub fn next(&mut self) -> Option<T> {
		match &self.peek_buf {
			None => {
				self.iter.next()
			}
			Some(t) => {
				let t = self.peek_buf.clone().unwrap();
				self.peek_buf = None;
				Some(t)
			}
		}
	}
}

fn main() {
    let _testSet = vec![5, 2, 6, 2, 3, 7, 4, 5, 1, 2, 9, 5, 3];
	let _iter = _testSet.iter();

	let mut _peekable = PeekableIter::from_forward(_iter);

	loop {
		if let (Some(peek), Some(next)) = (_peekable.peek(), _peekable.next()) {
			println!("Peek: {}, Next: {}", peek, next);
		}
		else {
			break;
		}
	}

	println!("{:?}", _peekable.peek());
}
