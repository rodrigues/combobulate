;;; test-rust.el --- Tests for Rust navigation  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'rust-ts-mode)

(defmacro combobulate-test-rust (source &rest body)
  "Run BODY in a `rust-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'rust))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (rust-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(ert-deftest combobulate-test-rust-next-moves-between-items ()
  (combobulate-test-rust "‸fn a() {}\n\nstruct B;\n"
    (combobulate-navigate-next)
    (should (looking-at-p "struct B"))))

(ert-deftest combobulate-test-rust-next-skips-comments ()
  (combobulate-test-rust "fn f() {\n    ‸a();\n    // note\n    b();\n}\n"
    (combobulate-navigate-next)
    (should (looking-at-p "b();"))))

(ert-deftest combobulate-test-rust-next-moves-between-match-arms ()
  (combobulate-test-rust "fn f(x: i32) {\n    match x {\n        ‸0 => 1,\n        _ => 2,\n    };\n}\n"
    (combobulate-navigate-next)
    (should (looking-at-p "_ => 2"))))

(ert-deftest combobulate-test-rust-next-moves-between-arguments ()
  (combobulate-test-rust "fn f() {\n    g(‸1, [2, 3]);\n}\n"
    (combobulate-navigate-next)
    (should (looking-at-p "\\[2, 3\\]"))))

(ert-deftest combobulate-test-rust-down-enters-function-body ()
  (combobulate-test-rust "‸fn f() {\n    a();\n}\n"
    (combobulate-navigate-down)
    (should (looking-at-p "a();"))))

(ert-deftest combobulate-test-rust-down-enters-impl-items ()
  (combobulate-test-rust "‸impl P {\n    fn new() {}\n}\n"
    (combobulate-navigate-down)
    (should (looking-at-p "fn new"))))

(ert-deftest combobulate-test-rust-down-enters-struct-fields ()
  (combobulate-test-rust "‸struct P { x: i32, y: i32 }\n"
    (combobulate-navigate-down)
    (should (looking-at-p "x: i32"))))

(ert-deftest combobulate-test-rust-down-enters-enum-variants ()
  (combobulate-test-rust "‸enum S { A, B(u8) }\n"
    (combobulate-navigate-down)
    (should (looking-at-p "A, B"))))

(ert-deftest combobulate-test-rust-up-leaves-function-body ()
  (combobulate-test-rust "fn f() {\n    ‸a();\n}\n"
    (combobulate-navigate-up)
    (should (looking-at-p "fn f"))))

(ert-deftest combobulate-test-rust-drag-down-swaps-arguments ()
  (combobulate-test-rust "fn f() {\n    g(‸1, 2);\n}\n"
    (combobulate-drag-down)
    (should (string-match-p "g(2, 1)" (buffer-string)))))

(ert-deftest combobulate-test-rust-drag-down-swaps-items ()
  (combobulate-test-rust "‸fn a() {}\n\nfn b() {}\n"
    (combobulate-drag-down)
    (should (equal "fn b() {}\n\nfn a() {}\n" (buffer-string)))))

(provide 'test-rust)
;;; test-rust.el ends here
