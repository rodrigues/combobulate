// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 181) (2 outline 193) (3 outline 214)); -*-
pub fn pick(x: Int) -> Int {
  case x {
    0 -> 10
    1 if x > 0 -> 20
    _ -> 30
  }
}
