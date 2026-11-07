-- -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 142) (2 outline 193) (3 outline 243)); -*-
WITH admins AS (
  SELECT id FROM users WHERE admin
), active AS (
  SELECT id FROM users WHERE active
), owners AS (
  SELECT id FROM users WHERE owner
)
SELECT id FROM active;
