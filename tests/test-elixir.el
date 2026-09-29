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

(ert-deftest combobulate-test-elixir-envelope-error-tuple-wraps-the-expression ()
  (combobulate-test-elixir "def f(x) do\n  ‸:not_found\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "error-tuple"))
    (should (equal (buffer-string) "def f(x) do\n  {:error, :not_found}\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-assert-matches-the-expression-against-the-pattern ()
  (combobulate-test-elixir "test \"creates\" do\n  ‸create(attrs)\nend\n"
    (let ((combobulate-envelope-prompt-actions '("{:ok, user}")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "assert"))))
    (should (equal (buffer-string) "test \"creates\" do\n  assert {:ok, user} = create(attrs)\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-assert-without-a-pattern-asserts-the-expression ()
  (combobulate-test-elixir "test \"shows\" do\n  ‸has_element?(view, \"#x\")\nend\n"
    (let ((combobulate-envelope-prompt-actions '("")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "assert"))))
    (should (equal (buffer-string) "test \"shows\" do\n  assert has_element?(view, \"#x\")\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-noreply-wraps-the-expression ()
  (combobulate-test-elixir "def handle_event(_, _, socket) do\n  ‸assign(socket, a: 1)\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "noreply"))
    (should (equal (buffer-string) "def handle_event(_, _, socket) do\n  {:noreply, assign(socket, a: 1)}\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-describe-wraps-the-test ()
  (combobulate-test-elixir "defmodule ATest do\n  ‸test \"a\" do\n    :ok\n  end\nend\n"
    (let ((combobulate-envelope-prompt-actions '("things")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "describe"))))
    (should (equal (buffer-string)
                   "defmodule ATest do\n  describe \"things\" do\n    test \"a\" do\n      :ok\n    end\n  end\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-describe-wraps-the-tests-in-the-region ()
  (combobulate-test-elixir "defmodule ATest do\n  ‸test \"a\" do\n    :ok\n  end\n\n  test \"b\" do\n    :ok\n  end\nend\n"
    (set-mark (point))
    (goto-char (point-max))
    (search-backward "\nend")
    (activate-mark)
    (let ((combobulate-envelope-prompt-actions '("things")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-execute-envelope "describe")))
    (should (equal (buffer-string)
                   (concat "defmodule ATest do\n  describe \"things\" do\n"
                           "    test \"a\" do\n      :ok\n    end\n\n"
                           "    test \"b\" do\n      :ok\n    end\n  end\nend\n")))))

(ert-deftest combobulate-test-elixir-envelope-for-iterates-over-the-expression ()
  (combobulate-test-elixir "def f(users) do\n  ‸users\nend\n"
    (let ((combobulate-envelope-prompt-actions '("user")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "for" (combobulate-test-elixir--expression-at-point)))))
    (should (equal (buffer-string) "def f(users) do\n  for user <- users do\n    \n  end\nend\n"))
    (should (equal (buffer-substring (line-beginning-position) (point)) "    "))))

(ert-deftest combobulate-test-elixir-envelope-if-wraps-the-statement ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\n  :ok\nend\n"
    (let ((combobulate-envelope-prompt-actions '("x > 0")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "if"))))
    (should (equal (buffer-string) "def f(x) do\n  if x > 0 do\n    foo(x)\n  end\n  :ok\nend\n"))))

(ert-deftest combobulate-test-elixir-envelope-if-offers-statements-only ()
  (combobulate-test-elixir "def f(x) do\n  y = foo(‸x)\nend\n"
    (let ((types (mapcar #'combobulate-node-type
                         (combobulate-envelope-get-applicable-nodes (combobulate-get-envelope-by-name "if")))))
      (should (equal types '("binary_operator" "call"))))))

(ert-deftest combobulate-test-elixir-envelope-fn-wraps-the-statement ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "fn" (combobulate-test-elixir--expression-at-point)))
    (should (equal (buffer-string) "def f(x) do\n  fn ->\n    foo(x)\n  end\nend\n"))
    (should (looking-at-p "->"))))

(ert-deftest combobulate-test-elixir-envelope-def-wraps-the-statement-in-a-private-function ()
  (combobulate-test-elixir "defmodule A do\n  ‸IO.puts(1)\nend\n"
    (let ((combobulate-envelope-prompt-actions '("helper")))
      (combobulate-with-stubbed-envelope-prompt
        (combobulate-with-stubbed-proffer-choices (:choices '(1 0))
          (combobulate-execute-envelope "def" (combobulate-test-elixir--expression-at-point)))))
    (should (equal (buffer-string) "defmodule A do\n  defp helper() do\n    IO.puts(1)\n  end\nend\n"))
    (should (looking-at-p ")"))))

(ert-deftest combobulate-test-elixir-envelope-try-wraps-the-statement ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\nend\n"
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-execute-envelope "try" (combobulate-test-elixir--expression-at-point)))
    (should (equal (buffer-string) "def f(x) do\n  try do\n    foo(x)\n  rescue\n    e -> \n  end\nend\n"))
    (should (looking-back "e -> " (line-beginning-position)))))

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

(defconst combobulate-test-elixir--queries
  "defp regions(a, b) do
  %{rows: rows} =
    Repo.query!(
      ~SQL\"\"\"
      SELECT 1
      \"\"\",
      [a, b]
    )

  Enum.map(rows, fn x -> ~w(x) end)
end

defp memberships(a, b) do
  %{rows: rows} =
    Repo.query!(
      ~SQL\"\"\"
      SELECT 2
      \"\"\",
      [a, b]
    )
end
"
  "Two functions that each call `Repo.query!' with a `~SQL' sigil.")

(defun combobulate-test-elixir--queries-at (marker count)
  "Return `combobulate-test-elixir--queries' with `‸' before the COUNTth MARKER."
  (let ((source combobulate-test-elixir--queries)
        (start 0))
    (dotimes (_ count)
      (setq start (1+ (string-search marker source start))))
    (concat (substring source 0 (1- start)) "‸" (substring source (1- start)))))

(ert-deftest combobulate-test-elixir-previous-occurrence-reaches-the-same-call-in-another-function ()
  (combobulate-test-elixir (combobulate-test-elixir--queries-at "Repo" 2)
    (combobulate-elixir-navigate-previous-occurrence)
    (should (= (line-number-at-pos) 3))
    (should (looking-at-p "Repo\\.query!"))))

(ert-deftest combobulate-test-elixir-next-occurrence-reaches-the-same-call-in-another-function ()
  (combobulate-test-elixir (combobulate-test-elixir--queries-at "query!" 1)
    (combobulate-elixir-navigate-next-occurrence)
    (should (= (line-number-at-pos) 15))
    (should (looking-at-p "Repo\\.query!"))))

(ert-deftest combobulate-test-elixir-previous-occurrence-matches-a-sigil-by-name ()
  (combobulate-test-elixir (combobulate-test-elixir--queries-at "SQL" 2)
    (combobulate-elixir-navigate-previous-occurrence)
    (should (= (line-number-at-pos) 4))
    (should (looking-at-p "~SQL"))))

(ert-deftest combobulate-test-elixir-next-occurrence-stays-put-without-a-match ()
  (combobulate-test-elixir (combobulate-test-elixir--queries-at "Enum" 1)
    (let ((start (point)))
      (combobulate-elixir-navigate-next-occurrence)
      (should (= (point) start)))))
