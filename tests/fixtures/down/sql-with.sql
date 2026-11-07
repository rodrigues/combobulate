-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 153) (2 outline 158) (3 outline 172) (4 outline 179)); -*-
WITH active AS (
  SELECT id, name FROM users
)
SELECT id FROM active;
