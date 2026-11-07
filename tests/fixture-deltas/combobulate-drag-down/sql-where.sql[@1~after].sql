-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 148) (2 outline 168)); -*-
SELECT id
FROM users
WHERE age > 18
  AND active = true
  AND name LIKE $1;
