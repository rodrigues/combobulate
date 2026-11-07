-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 160) (2 outline 188) (3 outline 211)); -*-
CREATE TABLE posts (
  id bigserial PRIMARY KEY,
  user_id bigint,
  title text NOT NULL
);
