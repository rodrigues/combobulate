;;; test-sql.el --- Tests for SQL and SQL in Elixir sigils  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-sql (source &rest body)
  "Run BODY in a `sql-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'sql-ts-mode) (treesit-language-available-p 'sql)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (sql-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defconst combobulate-test-sql-query
  "SELECT u.id\nFROM users u\nJOIN teams t ON t.id = u.team_id\nLEFT JOIN orgs o ON o.id = t.org_id\nWHERE u.active\nORDER BY u.id\nLIMIT 10;\n")

(defun combobulate-test-sql-at (marker)
  "Return `combobulate-test-sql-query' with `‸' before MARKER."
  (let ((pos (string-search marker combobulate-test-sql-query)))
    (concat (substring combobulate-test-sql-query 0 pos) "‸"
            (substring combobulate-test-sql-query pos))))

(ert-deftest combobulate-test-sql-next-visits-the-clauses-after-from ()
  (combobulate-test-sql (combobulate-test-sql-at "users u")
    (dolist (next '("JOIN teams" "LEFT JOIN" "WHERE" "ORDER BY" "LIMIT"))
      (combobulate-navigate-next)
      (should (looking-at-p next)))))

(ert-deftest combobulate-test-sql-drag-swaps-joins ()
  (combobulate-test-sql (combobulate-test-sql-at "JOIN teams")
    (combobulate-sql-drag-down)
    (should (string-search "FROM users u\nLEFT JOIN orgs o ON o.id = t.org_id\nJOIN teams t ON t.id = u.team_id\nWHERE"
                           (buffer-string)))))

(ert-deftest combobulate-test-sql-drag-refuses-to-move-a-join-before-the-table ()
  (combobulate-test-sql (combobulate-test-sql-at "JOIN teams")
    (should-error (combobulate-sql-drag-up) :type 'user-error)
    (should (equal (buffer-string) combobulate-test-sql-query))))

(ert-deftest combobulate-test-sql-drag-refuses-to-move-where-among-joins ()
  (combobulate-test-sql (combobulate-test-sql-at "WHERE")
    (should-error (combobulate-sql-drag-up) :type 'user-error)
    (should (equal (buffer-string) combobulate-test-sql-query))))

(ert-deftest combobulate-test-sql-drag-refuses-to-swap-select-and-from ()
  (combobulate-test-sql "‸SELECT id\nFROM users;\n"
    (should-error (combobulate-sql-drag-down) :type 'user-error)
    (should (equal (buffer-string) "SELECT id\nFROM users;\n"))))

(ert-deftest combobulate-test-sql-lone-statement-navigates-its-clauses ()
  (combobulate-test-sql "‸SELECT id\nFROM users;\n"
    (combobulate-navigate-next)
    (should (looking-at-p "FROM users"))))

(ert-deftest combobulate-test-sql-insert-column-list-is-not-a-row ()
  (combobulate-test-sql "INSERT INTO users ‸(id, name) VALUES (1, 'a'), (2, 'b');\n"
    (should-error (combobulate-sql-drag-down))
    (should (equal (buffer-string) "INSERT INTO users (id, name) VALUES (1, 'a'), (2, 'b');\n"))))

(ert-deftest combobulate-test-sql-conditions-stay-within-their-operator ()
  (combobulate-test-sql "SELECT id FROM t WHERE ‸a = 1 AND b = 2 OR c = 3;\n"
    (combobulate-navigate-next)
    (should (looking-at-p "b = 2"))
    (combobulate-navigate-next)
    (should (looking-at-p "b = 2"))))

(defmacro combobulate-test-sql-in-elixir (source &rest body)
  "Run BODY in an `elixir-ts-mode' buffer holding SOURCE, with point at `‸'.

`~SQL' sigils are parsed as SQL, as a user's configuration would."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'elixir-ts-mode) (treesit-language-available-p 'elixir)
                       (fboundp 'sql-ts-mode) (treesit-language-available-p 'sql)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (elixir-ts-mode)
       (setq-local treesit-range-settings
                   (append treesit-range-settings
                           (treesit-range-rules
                            :embed 'sql :host 'elixir :local t
                            '((sigil (sigil_name) @_name (:equal "SQL" @_name)
                                     (quoted_content) @sql)))))
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defconst combobulate-test-sql-elixir
  "defmodule Repo do\n  def active do\n    query(~SQL\"\"\"\n    SELECT id\n    FROM users\n    WHERE active = true AND age > 18\n    \"\"\")\n  end\n\n  def other, do: :ok\nend\n")

(defun combobulate-test-sql-elixir-at (marker)
  "Return `combobulate-test-sql-elixir' with `‸' before MARKER."
  (let ((pos (string-search marker combobulate-test-sql-elixir)))
    (concat (substring combobulate-test-sql-elixir 0 pos) "‸"
            (substring combobulate-test-sql-elixir pos))))

(ert-deftest combobulate-test-sql-in-elixir-next-visits-the-clauses ()
  (combobulate-test-sql-in-elixir (combobulate-test-sql-elixir-at "SELECT")
    (combobulate-elixir-navigate-next)
    (should (looking-at-p "FROM users"))
    (combobulate-elixir-navigate-previous)
    (should (looking-at-p "SELECT id"))))

(ert-deftest combobulate-test-sql-in-elixir-drag-swaps-conditions ()
  (combobulate-test-sql-in-elixir (combobulate-test-sql-elixir-at "active = true")
    (combobulate-elixir-drag-down)
    (should (string-search "WHERE age > 18 AND active = true" (buffer-string)))))

(ert-deftest combobulate-test-sql-in-elixir-drag-refuses-to-swap-clauses ()
  (combobulate-test-sql-in-elixir (combobulate-test-sql-elixir-at "FROM users")
    (should-error (combobulate-elixir-drag-up) :type 'user-error)
    (should (equal (buffer-string) combobulate-test-sql-elixir))))

(ert-deftest combobulate-test-sql-in-elixir-outside-the-sigil-is-elixir ()
  (combobulate-test-sql-in-elixir (combobulate-test-sql-elixir-at "def other")
    (combobulate-elixir-navigate-previous)
    (should (looking-at-p "def active do"))))

(provide 'test-sql)
;;; test-sql.el ends here
