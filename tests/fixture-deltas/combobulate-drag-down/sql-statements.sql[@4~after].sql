-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 153) (2 outline 176) (3 outline 212) (4 outline 255)); -*-
SELECT id FROM users;

INSERT INTO users (id) VALUES (1);

UPDATE users SET name = 'x' WHERE id = 1;

DELETE FROM users WHERE id = 1;
