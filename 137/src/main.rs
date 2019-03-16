#![feature(alloc)]

use std::alloc::{self, Layout};
use std::mem;
use std::ptr;

/// A dense bitset
/// 
/// Defaults all elements to be false
///
/// Note that this bitset has no notion of capacity.
/// It will always assume it's capacity to be it's current length rounded up to the nearest multiple of eight.
/// As such it also has no notion of growing or pushing into the set.
/// The only way to resize it is to request so manually.


#[derive(Debug)]
pub struct Bitset {
    ptr: *mut u8,
    size: usize,
}

impl Bitset {
    pub fn empty() -> Bitset {
        Bitset {
            ptr: ptr::null_mut(),
            size: 0,
        }
    }

    pub fn with_size(c: usize) -> Bitset {
        let mut this = Bitset {
            ptr: ptr::null_mut(),
            size: 0,
        };

        this.resize(c);
        return this;
    }

    pub fn resize(&mut self, c: usize) {
        // We don't care for downsizing around these parts
        if self.size > c { return; }

        unsafe {
            let bytes_needed = (c - 1) / 8 + 1;
            let align = mem::align_of::<u8>();

            let ptr = alloc::alloc_zeroed(Layout::from_size_align(bytes_needed, align).unwrap());

            // If we had an allocation before, copy elements and drop the old allocation
            if self.size != 0 {
                let current_bytes = (self.size - 1) / 8 + 1;
                for i in 0..current_bytes {
                    *ptr.offset(i as isize) = *self.ptr.offset(i as isize);
                }

                alloc::dealloc(self.ptr, Layout::from_size_align(current_bytes, align).unwrap());
            }

            self.ptr = ptr;
            self.size = c;
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn get(&self, i: usize) -> bool {
        unsafe {
            let n = i as isize / 8;
            let byte = *(self.ptr.offset(n));

            return ((byte >> i) & 0b00000001) != 0;
        }
    }

    pub fn set(&mut self, i: usize, v: bool) {
        unsafe {
            let n = i as isize / 8;
            let byte = (self.ptr.offset(n));

            (*byte) = (*byte) | ((v as u8) << i);
        }
    }
}

fn main() {
    let mut set = Bitset::with_size(376);

    let v = [true, false, true, true, true, false, true, false];

    for (i, e) in v.iter().enumerate() {
        set.set(i, *e);
        println!("Expect: {}, Got: {}", e, set.get(i));
    }

    set.resize(528);

    for (i, e) in v.iter().enumerate() {
        println!("Expect: {}, Got: {}", e, set.get(i));
    }

    println!("{:?}", set);
}
