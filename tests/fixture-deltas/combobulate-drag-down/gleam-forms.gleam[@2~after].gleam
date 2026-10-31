// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 153) (2 outline 186) (3 outline 209) (4 outline 230)); -*-
import gleam/io

/// The answer.
pub type Id =
  Int

pub const answer = 42

pub fn main() {
  io.println("hi")
}
