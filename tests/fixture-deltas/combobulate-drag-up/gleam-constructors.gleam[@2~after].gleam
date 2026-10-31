// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 156) (2 outline 187) (3 outline 204)); -*-
pub type Order {
  Draft(id: Int)
  Order(id: Int, total: Float)
  Lost
}
