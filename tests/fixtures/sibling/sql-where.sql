-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 164) (2 outline 184) (3 outline 199)); -*-
SELECT id
FROM users
WHERE active = true
  AND age > 18
  AND name LIKE $1;
