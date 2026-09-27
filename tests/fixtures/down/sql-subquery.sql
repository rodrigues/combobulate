-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 170) (2 outline 174) (3 outline 181)); -*-
SELECT id
FROM users
WHERE id IN (
  SELECT user_id FROM admins
);
