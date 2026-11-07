-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 154) (2 outline 166) (3 outline 179)); -*-
UPDATE users
SET name = 'x', email = 'y', age = 1
WHERE id = 1;
