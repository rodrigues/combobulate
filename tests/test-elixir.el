;;; test-elixir.el --- Tests for the Elixir editing commands  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-elixir (source &rest body)
  "Run BODY in an `elixir-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'elixir-ts-mode) (treesit-language-available-p 'elixir)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (elixir-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defun combobulate-test-elixir--expression-at-point ()
  "Return the largest expression starting at point.

Envelope tests with a point placement pass it explicitly, because the
stubbed proffer returns the last node it was given, not the chosen one."
  (combobulate-elixir--outermost-at (combobulate-elixir--node-at (point))))

(ert-deftest combobulate-test-elixir-envelope-dbg-wraps-the-call-at-point ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\n  :ok\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "dbg"))
    (should (equal (buffer-string) "def f(x) do\n  dbg(foo(x))\n  :ok\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-dbg-wraps-the-pipeline-at-point ()
  (combobulate-test-elixir "def f(x) do\n  ‸x |> foo() |> bar()\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "dbg"))
    (should (equal (buffer-string) "def f(x) do\n  dbg(x |> foo() |> bar())\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-dbg-wraps-the-region ()
  (combobulate-test-elixir "def f(x) do\n  y = ‸x + 1\nend\n"
    (set-mark (point))
    (end-of-line)
    (activate-mark)
    (combobulate-execute-envelope "dbg")
    (should (equal (buffer-string) "def f(x) do\n  y = dbg(x + 1)\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-case-uses-the-expression-as-subject ()
  (combobulate-test-elixir "def f(x) do\n  ‸fetch(x)\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "case" (combobulate-test-elixir--expression-at-point)))
    (should (equal (buffer-string) "def f(x) do\n  case fetch(x) do\n    \n  end\nend\n"))
    (should (equal (buffer-substring (line-beginning-position) (point)) "    "))))

(ert-deftest combobulate-test-elixir-envelope-with-binds-the-expression ()
  (combobulate-test-elixir "def f(id) do\n  ‸fetch(id)\nend\n"
    (let ((combobulate-envelope-prompt-actions '("user")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "with" (combobulate-test-elixir--expression-at-point)))))
    (should (equal (buffer-string) "def f(id) do\n  with {:ok, user} <- fetch(id) do\n    user\n  end\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-ok-tuple-wraps-the-expression ()
  (combobulate-test-elixir "def f(x) do\n  ‸%{x: x}\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "ok-tuple"))
    (should (equal (buffer-string) "def f(x) do\n  {:ok, %{x: x}}\nend\n"))))

(ert-deftest combobulate-test-elixir-dbg-pipe-appends-to-a-multiline-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  x\n  |> ‸foo()\n  |> bar()\nend\n"
    (combobulate-elixir-dbg-pipe)
    (should (equal (buffer-string) "def f(x) do\n  x\n  |> foo()\n  |> bar()\n  |> dbg()\nend\n"))))

(ert-deftest combobulate-test-elixir-dbg-pipe-appends-to-a-single-line-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  y = ‸x |> foo()\n  y\nend\n"
    (combobulate-elixir-dbg-pipe)
    (should (equal (buffer-string) "def f(x) do\n  y = x |> foo() |> dbg()\n  y\nend\n"))))

(ert-deftest combobulate-test-elixir-dbg-pipe-picks-the-innermost-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  x |> foo(y |> ‸bar())\nend\n"
    (combobulate-elixir-dbg-pipe)
    (should (equal (buffer-string) "def f(x) do\n  x |> foo(y |> bar() |> dbg())\nend\n"))))

(ert-deftest combobulate-test-elixir-dbg-pipe-refuses-outside-a-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\nend\n"
    (should-error (combobulate-elixir-dbg-pipe) :type 'user-error)
    (should (equal (buffer-string) "def f(x) do\n  foo(x)\nend\n"))))
