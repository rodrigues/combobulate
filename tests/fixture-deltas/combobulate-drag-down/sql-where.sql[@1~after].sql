-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 164) (2 outline 184) (3 outline 199)); -*-
SELECT id
FROM users
WHERE age > 18
  AND active = true
  AND name LIKE $1;
