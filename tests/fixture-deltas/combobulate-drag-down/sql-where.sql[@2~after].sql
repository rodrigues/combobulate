-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 148) (2 outline 168)); -*-
SELECT id
FROM users
WHERE active = true
  AND age > 18
  AND name LIKE $1;
