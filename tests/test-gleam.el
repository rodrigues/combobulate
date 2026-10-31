;;; test-gleam.el --- Tests for Gleam navigation  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-gleam (source &rest body)
  "Run BODY in a `gleam-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'gleam-ts-mode) (treesit-language-available-p 'gleam)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (gleam-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(ert-deftest combobulate-test-gleam-next-moves-between-clauses-with-patterns ()
  (combobulate-test-gleam "fn f(x) {\n  case x {\n    ‸Ok(a) -> a\n    Error(_) -> 0\n  }\n}\n"
    (combobulate-navigate-next)
    (should (looking-at-p "Error(_)"))))

(ert-deftest combobulate-test-gleam-next-skips-doc-comments ()
  (combobulate-test-gleam "‸import gleam/io\n\n/// Docs.\npub fn f() {\n  1\n}\n"
    (combobulate-navigate-next)
    (should (looking-at-p "pub fn f"))))

(ert-deftest combobulate-test-gleam-down-enters-constructor-arguments ()
  (combobulate-test-gleam "pub type T {\n  ‸A(x: Int, y: Int)\n}\n"
    (combobulate-navigate-down)
    (should (looking-at-p "x: Int"))))

(provide 'test-gleam)
;;; test-gleam.el ends here
