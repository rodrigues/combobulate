-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 153) (2 outline 163) (3 outline 169)); -*-
SELECT coalesce(nickname, 'anonymous', name)
FROM users;
