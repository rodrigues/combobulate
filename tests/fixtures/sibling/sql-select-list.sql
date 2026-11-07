-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 144) (2 outline 148) (3 outline 154)); -*-
SELECT id, name, lower(email) AS email
FROM users;
