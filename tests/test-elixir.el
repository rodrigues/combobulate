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

(defun combobulate-test-elixir--fiery-p (text)
  "Return non-nil if the first TEXT in the buffer has the fiery highlight."
  (goto-char (point-min))
  (search-forward text)
  (eq (get-text-property (match-beginning 0) 'face)
      'combobulate-query-highlight-fiery-flames-face))

(ert-deftest combobulate-test-elixir-highlights-debugging-calls ()
  (combobulate-test-elixir "def f(x) do\n  ‸dbg(x)\n  x |> IO.inspect()\n  IEx.pry()\n  Other.inspect(x)\n  x\nend\n"
    (font-lock-ensure)
    (should (combobulate-test-elixir--fiery-p "dbg"))
    (should (combobulate-test-elixir--fiery-p "IO.inspect"))
    (should (combobulate-test-elixir--fiery-p "IEx.pry"))
    (should-not (combobulate-test-elixir--fiery-p "Other.inspect"))))

(ert-deftest combobulate-test-elixir-highlights-focus-and-skip-tags ()
  (combobulate-test-elixir "defmodule MTest do\n  @moduletag :skip\n\n  @tag :focus\n  test \"a\" do\n  end\n\n  @tag :slow\n  ‸test \"b\" do\n  end\nend\n"
    (font-lock-ensure)
    (should (combobulate-test-elixir--fiery-p "@moduletag :skip"))
    (should (combobulate-test-elixir--fiery-p "@tag :focus"))
    (should-not (combobulate-test-elixir--fiery-p "@tag :slow"))))

(ert-deftest combobulate-test-elixir-toggle-private-flips-every-clause ()
  (combobulate-test-elixir "defmodule M do\n  def f(nil), do: nil\n  def g(x), do: x\n  def f(x) do\n    ‸x\n  end\n  def f(x, y), do: x + y\nend\n"
    (combobulate-elixir-toggle-private)
    (should (equal (buffer-string) "defmodule M do\n  defp f(nil), do: nil\n  def g(x), do: x\n  defp f(x) do\n    x\n  end\n  def f(x, y), do: x + y\nend\n"))
    (should (looking-at-p "x\n  end"))))

(ert-deftest combobulate-test-elixir-toggle-private-handles-guards-and-macros ()
  (combobulate-test-elixir "defmodule M do\n  defmacrop ‸m(x) when is_atom(x), do: x\n  defp f do\n    :ok\n  end\nend\n"
    (combobulate-elixir-toggle-private)
    (search-forward "defp f")
    (combobulate-elixir-toggle-private)
    (should (equal (buffer-string) "defmodule M do\n  defmacro m(x) when is_atom(x), do: x\n  def f do\n    :ok\n  end\nend\n"))))

(ert-deftest combobulate-test-elixir-toggle-private-refuses-outside-a-definition ()
  (combobulate-test-elixir "defmodule M do\n  ‸@x 1\nend\n"
    (should-error (combobulate-elixir-toggle-private) :type 'user-error)))

(ert-deftest combobulate-test-elixir-pipeline-head-and-last-stage ()
  (combobulate-test-elixir "def f(x) do\n  x\n  |> foo()\n  |> ‸bar()\n  |> baz()\nend\n"
    (combobulate-elixir-navigate-pipeline-head)
    (should (looking-at-p "x\n  |> foo"))
    (combobulate-elixir-navigate-pipeline-last-stage)
    (should (looking-at-p "baz()"))))

(ert-deftest combobulate-test-elixir-pipeline-ends-pick-the-innermost-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  x |> foo(y |> ‸bar() |> qux())\nend\n"
    (combobulate-elixir-navigate-pipeline-head)
    (should (looking-at-p "y |> bar"))
    (combobulate-elixir-navigate-pipeline-last-stage)
    (should (looking-at-p "qux()"))))

(ert-deftest combobulate-test-elixir-pipeline-ends-refuse-outside-a-pipeline ()
  (combobulate-test-elixir "def f(x) do\n  ‸foo(x)\nend\n"
    (should-error (combobulate-elixir-navigate-pipeline-head) :type 'user-error)))

(defconst combobulate-test-elixir--documented
  "defmodule M do
  @moduledoc \"M\"
  def h, do: 0

  @doc \"F\"
  @spec f(integer) :: integer
  def f(0), do: 0
  def f(x) do
    x
  end

  def g, do: 1
end
"
  "A module with a documented function of two clauses.")

(ert-deftest combobulate-test-elixir-function-attributes-round-trip ()
  (combobulate-test-elixir (replace-regexp-in-string "^    x" "    ‸x" combobulate-test-elixir--documented)
    (combobulate-elixir-navigate-function-attributes)
    (should (looking-at-p "@doc \"F\""))
    (search-forward "integer")
    (combobulate-elixir-navigate-function-attributes)
    (should (looking-at-p "def f(0)"))))

(ert-deftest combobulate-test-elixir-function-attributes-refuse-without-attributes ()
  (dolist (name '("g" "h"))
    (combobulate-test-elixir (replace-regexp-in-string (format "def %s," name) (format "def ‸%s," name)
                                                       combobulate-test-elixir--documented)
      (should-error (combobulate-elixir-navigate-function-attributes) :type 'user-error))))

(ert-deftest combobulate-test-elixir-toggle-do-block-round-trips-a-definition ()
  (combobulate-test-elixir "defmodule M do\n  def f(x) when x > 0, do: ‸x + 1\nend\n"
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "defmodule M do\n  def f(x) when x > 0 do\n    x + 1\n  end\nend\n"))
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "defmodule M do\n  def f(x) when x > 0, do: x + 1\nend\n"))))

(ert-deftest combobulate-test-elixir-toggle-do-block-keeps-else ()
  (combobulate-test-elixir "‸if a, do: b, else: c\n"
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "if a do\n  b\nelse\n  c\nend\n"))
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "if a, do: b, else: c\n"))))

(ert-deftest combobulate-test-elixir-toggle-do-block-handles-calls-without-a-comma ()
  (combobulate-test-elixir "‸quote do: x\nfoo(a, do: y)\n"
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "quote do\n  x\nend\nfoo(a, do: y)\n"))
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "quote do: x\nfoo(a, do: y)\n"))
    (search-forward "foo")
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "quote do: x\nfoo(a) do\n  y\nend\n"))
    (combobulate-elixir-toggle-do-block)
    (should (equal (buffer-string) "quote do: x\nfoo(a, do: y)\n"))))

(ert-deftest combobulate-test-elixir-toggle-do-block-refuses-blocks-that-do-not-fit-a-keyword ()
  (dolist (source '("def f(x) do\n  ‸y = x\n  y\nend\n"
                    "case ‸x do\n  1 -> :one\nend\n"
                    "def f(x) do\n  # why\n  ‸x\nend\n"
                    "try do\n  ‸x\nrescue\n  _ -> nil\nend\n"))
    (combobulate-test-elixir source
      (let ((before (buffer-string)))
        (should-error (combobulate-elixir-toggle-do-block) :type 'user-error)
        (should (equal (buffer-string) before))))))

(defmacro combobulate-test-elixir--toggles-pipe (before after)
  "Assert that toggling the pipe in BEFORE, with point at `‸', gives AFTER."
  `(combobulate-test-elixir ,before
     (combobulate-elixir-toggle-pipe)
     (should (equal (buffer-string) ,after))))

(ert-deftest combobulate-test-elixir-toggle-pipe-round-trips-a-call ()
  (combobulate-test-elixir "‸Enum.map(rows, &f/1)\n"
    (combobulate-elixir-toggle-pipe)
    (should (equal (buffer-string) "rows |> Enum.map(&f/1)\n"))
    (combobulate-elixir-toggle-pipe)
    (should (equal (buffer-string) "Enum.map(rows, &f/1)\n"))))

(ert-deftest combobulate-test-elixir-toggle-pipe-pipes-the-innermost-call ()
  (combobulate-test-elixir--toggles-pipe "foo(‸x)\n" "x |> foo()\n")
  (combobulate-test-elixir--toggles-pipe "x |> foo(bar(‸y, z))\n" "x |> foo(y |> bar(z))\n")
  (combobulate-test-elixir--toggles-pipe "‸foo(x |> bar())\n" "x |> bar() |> foo()\n")
  (combobulate-test-elixir--toggles-pipe "‸foo(a == b, c)\n" "(a == b) |> foo(c)\n"))

(ert-deftest combobulate-test-elixir-toggle-pipe-unpipes-the-stage-at-point ()
  (combobulate-test-elixir--toggles-pipe "x |> ‸a() |> b()\n" "a(x) |> b()\n")
  (combobulate-test-elixir--toggles-pipe "‸x |> a() |> b()\n" "a(x) |> b()\n")
  (combobulate-test-elixir--toggles-pipe "x |> a() |> ‸b(y)\n" "b(x |> a(), y)\n")
  (combobulate-test-elixir--toggles-pipe "x |> ‸foo\n" "foo(x)\n"))

(ert-deftest combobulate-test-elixir-toggle-pipe-refuses-calls-without-a-pipeable-argument ()
  (dolist (source '("‸foo()\n" "‸foo(a: 1)\n" "x |> ‸case do\n  _ -> 1\nend\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-pipe) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(ert-deftest combobulate-test-elixir-toggle-multi-alias-splits ()
  (combobulate-test-elixir "defmodule M do\n  alias A.B.{C, ‸D.E}\n  x\nend\n"
    (combobulate-elixir-toggle-multi-alias)
    (should (equal (buffer-string) "defmodule M do\n  alias A.B.C\n  alias A.B.D.E\n  x\nend\n")))
  (combobulate-test-elixir "‸alias __MODULE__.{\n  X,\n  Y\n}\n"
    (combobulate-elixir-toggle-multi-alias)
    (should (equal (buffer-string) "alias __MODULE__.X\nalias __MODULE__.Y\n"))))

(ert-deftest combobulate-test-elixir-toggle-multi-alias-merges-the-aliases-around-point ()
  (combobulate-test-elixir "defmodule M do\n  alias A.B.C\n  alias X.Y\n  alias ‸A.B.D\n  import Z\n\n  alias A.B.F\nend\n"
    (combobulate-elixir-toggle-multi-alias)
    (should (equal (buffer-string) "defmodule M do\n  alias A.B.{C, D}\n  alias X.Y\n  import Z\n\n  alias A.B.F\nend\n"))
    (should (looking-at-p "alias A.B.{C, D}"))))

(ert-deftest combobulate-test-elixir-toggle-multi-alias-refuses-what-it-cannot-merge ()
  (dolist (source '("alias ‸A.B.C\nalias X.Y\n" "alias ‸A.B.C, as: D\nalias A.B.E\n" "alias ‸A\nalias B\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-multi-alias) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(ert-deftest combobulate-test-elixir-split-or-join-round-trips-a-list ()
  (combobulate-test-elixir "def f do\n  x = [1, ‸2, 3]\nend\n"
    (combobulate-elixir-split-or-join)
    (should (equal (buffer-string) "def f do\n  x = [\n    1,\n    2,\n    3\n  ]\nend\n"))
    (combobulate-elixir-split-or-join)
    (should (equal (buffer-string) "def f do\n  x = [1, 2, 3]\nend\n"))))

(ert-deftest combobulate-test-elixir-split-or-join-handles-each-collection ()
  (dolist (case '(("%{‸a: 1, b: %{c: 2}}\n" "%{\n  a: 1,\n  b: %{c: 2}\n}\n")
                  ("%User{‸name: \"x\", age: 1}\n" "%User{\n  name: \"x\",\n  age: 1\n}\n")
                  ("{‸:ok, value}\n" "{\n  :ok,\n  value\n}\n")
                  ("<<‸a, b>>\n" "<<\n  a,\n  b\n>>\n")
                  ("foo(‸a, b: 1)\n" "foo(\n  a,\n  b: 1\n)\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-split-or-join)
      (should (equal (buffer-string) (cadr case)))
      (combobulate-elixir-split-or-join)
      (should (equal (buffer-string) (string-replace "‸" "" (car case)))))))

(ert-deftest combobulate-test-elixir-split-or-join-refuses-comments-and-empty-collections ()
  (dolist (source '("[\n  1,\n  # two\n  ‸2\n]\n" "x = [‸]\n" "‸x\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-split-or-join) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(defmacro combobulate-test-elixir--toggles-capture (before after)
  "Assert that toggling the capture in BEFORE, with point at `‸', gives AFTER."
  `(combobulate-test-elixir ,before
     (combobulate-elixir-toggle-capture)
     (should (equal (buffer-string) ,after))))

(ert-deftest combobulate-test-elixir-toggle-capture-turns-functions-into-captures ()
  (combobulate-test-elixir--toggles-capture "Enum.map(xs, fn x -> ‸foo(x) end)\n" "Enum.map(xs, &foo/1)\n")
  (combobulate-test-elixir--toggles-capture "‸fn x, y -> String.contains?(x, y) end\n" "&String.contains?/2\n")
  (combobulate-test-elixir--toggles-capture "‸fn x -> Map.get(x, :k) end\n" "&Map.get(&1, :k)\n")
  (combobulate-test-elixir--toggles-capture "‸fn a, b -> b - a end\n" "&(&2 - &1)\n")
  (combobulate-test-elixir--toggles-capture "‸fn x -> f.(x) end\n" "&f.(&1)\n")
  (combobulate-test-elixir--toggles-capture "‸fn x -> x.name end\n" "&(&1.name)\n")
  (combobulate-test-elixir--toggles-capture "‸fn -> now() end\n" "&now/0\n"))

(ert-deftest combobulate-test-elixir-toggle-capture-turns-captures-into-functions ()
  (combobulate-test-elixir--toggles-capture "Enum.map(xs, ‸&String.upcase/1)\n" "Enum.map(xs, fn arg1 -> String.upcase(arg1) end)\n")
  (combobulate-test-elixir--toggles-capture "‸&Map.get(&1, :k)\n" "fn arg1 -> Map.get(arg1, :k) end\n")
  (combobulate-test-elixir--toggles-capture "&(&2 - ‸&1)\n" "fn arg1, arg2 -> arg2 - arg1 end\n")
  (combobulate-test-elixir--toggles-capture "‸&now/0\n" "fn -> now() end\n"))

(ert-deftest combobulate-test-elixir-toggle-capture-refuses-functions-a-capture-cannot-express ()
  (dolist (source '("‸fn\n  nil -> 0\n  x -> x\nend\n"
                    "‸fn {a, b} -> a end\n"
                    "‸fn x when x > 0 -> x end\n"
                    "‸fn x -> 1 end\n"
                    "‸fn x ->\n  y = x\n  y\nend\n"
                    "‸fn x -> Enum.map(x, fn y -> y end) end\n"
                    "‸fn -> 1 end\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-capture) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(defconst combobulate-test-elixir--functions
  "defmodule M do
  use GenServer

  # Section

  # sobelow_skip [\"SQL.Query\"]
  @doc \"F\"
  @spec f(integer) :: integer
  def f(0), do: 0

  def f(x) do
    x
  end

  def g, do: 1
end
"
  "A module with a commented, documented function of two clauses.")

(defconst combobulate-test-elixir--function-f
  "  # sobelow_skip [\"SQL.Query\"]
  @doc \"F\"
  @spec f(integer) :: integer
  def f(0), do: 0

  def f(x) do
    x
  end
"
  "The whole of `f' in `combobulate-test-elixir--functions'.")

(defun combobulate-test-elixir--functions-at (text)
  "Return `combobulate-test-elixir--functions' with `‸' before TEXT."
  (string-replace text (concat "‸" text) combobulate-test-elixir--functions))

(ert-deftest combobulate-test-elixir-mark-function-marks-clauses-attributes-and-comments ()
  (dolist (at '("x\n  end" "@spec" "# sobelow" "def f(0)"))
    (combobulate-test-elixir (combobulate-test-elixir--functions-at at)
      (combobulate-elixir-mark-function)
      (should mark-active)
      (should (equal (buffer-substring (region-beginning) (region-end)) combobulate-test-elixir--function-f)))))

(ert-deftest combobulate-test-elixir-mark-function-marks-a-lone-clause ()
  (combobulate-test-elixir (combobulate-test-elixir--functions-at "g, do")
    (combobulate-elixir-mark-function)
    (should (equal (buffer-substring (region-beginning) (region-end)) "  def g, do: 1\n"))))

(ert-deftest combobulate-test-elixir-mark-function-refuses-outside-a-function ()
  (combobulate-test-elixir (combobulate-test-elixir--functions-at "use")
    (should-error (combobulate-elixir-mark-function) :type 'user-error)))

(ert-deftest combobulate-test-elixir-mark-function-from-a-comment-in-its-body ()
  (combobulate-test-elixir "defmodule M do\n  def f(x) do\n    ‸# why\n    x\n  end\nend\n"
    (combobulate-elixir-mark-function)
    (should (equal (buffer-substring (region-beginning) (region-end)) "  def f(x) do\n    # why\n    x\n  end\n"))))

(defconst combobulate-test-elixir--functions-swapped
  "defmodule M do
  use GenServer

  # Section

  def g, do: 1

  # sobelow_skip [\"SQL.Query\"]
  @doc \"F\"
  @spec f(integer) :: integer
  def f(0), do: 0

  def f(x) do
    x
  end
end
"
  "`combobulate-test-elixir--functions' with `f' and `g' swapped.")

(ert-deftest combobulate-test-elixir-drag-function-round-trips ()
  (combobulate-test-elixir (combobulate-test-elixir--functions-at "x\n  end")
    (combobulate-elixir-drag-function-down)
    (should (equal (buffer-string) combobulate-test-elixir--functions-swapped))
    (should (looking-at-p "# sobelow"))
    (combobulate-elixir-drag-function-up)
    (should (equal (buffer-string) combobulate-test-elixir--functions))
    (should (looking-at-p "# sobelow"))))

(ert-deftest combobulate-test-elixir-drag-function-refuses-without-a-neighbouring-function ()
  (dolist (case '(("x\n  end" . combobulate-elixir-drag-function-up)
                  ("g, do" . combobulate-elixir-drag-function-down)))
    (combobulate-test-elixir (combobulate-test-elixir--functions-at (car case))
      (should-error (funcall (cdr case)) :type 'user-error)
      (should (equal (buffer-string) combobulate-test-elixir--functions)))))

(ert-deftest combobulate-test-elixir-clone-clones-the-sibling-at-point ()
  (dolist (case '(("defmodule M do\n  ‸def g do\n    2\n  end\nend\n"
                   "defmodule M do\n  def g do\n    2\n  end\n  def g do\n    2\n  end\nend\n")
                  ("def f do\n  ‸a = 1\n  b\nend\n" "def f do\n  a = 1\n  a = 1\n  b\nend\n")
                  ("case x do\n  1 -> :one\n  ‸2 -> :two\nend\n"
                   "case x do\n  1 -> :one\n  2 -> :two\n  2 -> :two\nend\n")
                  ("x = [1, ‸2, 3]\n" "x = [1, 2, 2, 3]\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-clone-node-dwim)
      (should (equal (buffer-string) (cadr case))))))

(ert-deftest combobulate-test-elixir-clone-refuses-outside-a-sibling ()
  (combobulate-test-elixir "‸\n"
    (should-error (combobulate-elixir-clone-node-dwim) :type 'user-error)))

(defconst combobulate-test-elixir--overloaded
  "defmodule M do
  @doc \"F\"
  @spec f(integer) :: integer
  @spec f(a) :: a when a: atom
  def f(0), do: 0
  def f(x) when is_atom(x), do: x
  def f(x) do
    x
  end

  @spec f(integer, integer) :: integer
  def f(x, y), do: x + y

  @spec g :: integer
  defp g, do: f(1)
end
"
  "A module with `f/1', `f/2' and `g/0', each with a `@spec'.")

(defun combobulate-test-elixir--overloaded-at (text)
  "Return `combobulate-test-elixir--overloaded' with `‸' before TEXT."
  (string-replace text (concat "‸" text) combobulate-test-elixir--overloaded))

(defun combobulate-test-elixir--edited-names ()
  "Return the (LINE . TEXT) of the nodes that editing the function name at point edits."
  (let ((edited))
    (cl-letf (((symbol-function 'combobulate-cursor-edit-nodes)
               (lambda (nodes &rest _) (setq edited nodes))))
      (combobulate-elixir-edit-function-name nil))
    (mapcar (lambda (node) (cons (line-number-at-pos (treesit-node-start node)) (treesit-node-text node t)))
            edited)))

(ert-deftest combobulate-test-elixir-edit-function-name-covers-every-clause-and-spec ()
  (dolist (at '("x\n  end" "@spec f(a)" "f(0)"))
    (combobulate-test-elixir (combobulate-test-elixir--overloaded-at at)
      (should (equal (combobulate-test-elixir--edited-names)
                     '((3 . "f") (4 . "f") (5 . "f") (6 . "f") (7 . "f")))))))

(ert-deftest combobulate-test-elixir-edit-function-name-handles-functions-without-arguments ()
  (combobulate-test-elixir (combobulate-test-elixir--overloaded-at "defp g")
    (should (equal (combobulate-test-elixir--edited-names) '((14 . "g") (15 . "g"))))))

(ert-deftest combobulate-test-elixir-edit-function-name-refuses-outside-a-function ()
  (combobulate-test-elixir (combobulate-test-elixir--overloaded-at "@doc")
    (should-error (combobulate-test-elixir--edited-names) :type 'user-error)))

(ert-deftest combobulate-test-elixir-edit-function-name-covers-the-calls-of-a-private-function ()
  (combobulate-test-elixir "defmodule M do
  @spec g(integer) :: integer
  defp ‸g(0), do: 0
  defp g(x), do: g(x - 1)

  def run(xs) do
    a = g(1)
    b = xs |> g()
    c = Enum.map(xs, &g/1)
    d = Enum.map(xs, &g(&1))
    e = xs |> g
    g(1, 2)
    other.g(1)
  end

  defmodule Inner do
    def h(x), do: g(x)
  end
end
"
    (should (equal (combobulate-test-elixir--edited-names)
                   '((2 . "g") (3 . "g") (4 . "g") (4 . "g") (7 . "g") (8 . "g") (9 . "g") (10 . "g") (11 . "g"))))))

(defmacro combobulate-test-elixir--round-trips (command before after)
  "Assert that COMMAND turns BEFORE, with point at `‸', into AFTER and back."
  `(combobulate-test-elixir ,before
     (,command)
     (should (equal (buffer-string) ,after))
     (,command)
     (should (equal (buffer-string) (string-replace "‸" "" ,before)))))

(ert-deftest combobulate-test-elixir-toggle-keyword-map-round-trips ()
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-keyword-map
                                        "x = [‸a: 1, b: 2]\n" "x = %{a: 1, b: 2}\n")
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-keyword-map
                                        "[\n  ‸a: 1,\n  # c\n  b: 2\n]\n" "%{\n  a: 1,\n  # c\n  b: 2\n}\n")
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-keyword-map
                                        "[a: %{‸b: 1}]\n" "[a: [b: 1]]\n"))

(ert-deftest combobulate-test-elixir-toggle-keyword-map-refuses-other-collections ()
  (dolist (source '("[‸1, 2]\n" "[‸]\n" "%{‸\"a\" => 1}\n" "%User{‸a: 1}\n" "%{m | ‸a: 1}\n" "foo(‸a: 1)\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-keyword-map) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(ert-deftest combobulate-test-elixir-toggle-map-keys-round-trips ()
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-map-keys
                                        "%{‸a: 1, \"b-c\": 2}\n" "%{\"a\" => 1, \"b-c\" => 2}\n")
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-map-keys
                                        "%{\n  # c\n  ‸a: %{b: 1},\n  # d\n  ok?: true\n}\n"
                                        "%{\n  # c\n  \"a\" => %{b: 1},\n  # d\n  \"ok?\" => true\n}\n"))

(ert-deftest combobulate-test-elixir-toggle-map-keys-turns-atom-arrows-into-strings ()
  (combobulate-test-elixir "%{:a => 1, ‸b: 2}\n"
    (combobulate-elixir-toggle-map-keys)
    (should (equal (buffer-string) "%{\"a\" => 1, \"b\" => 2}\n"))))

(ert-deftest combobulate-test-elixir-toggle-map-keys-refuses-what-it-cannot-convert ()
  (dolist (source '("%{\"a\" => 1, ‸b: 2}\n" "%User{‸a: 1}\n" "%{m | ‸a: 1}\n" "%{\"#{x}\" => ‸1}\n"
                    "%{k => ‸1}\n" "[‸a: 1]\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-map-keys) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(ert-deftest combobulate-test-elixir-extract-variable-binds-the-expression-above-its-statement ()
  (dolist (case '(("def f(x) do\n  y = foo(‸bar(x), 1)\n  y\nend\n" "b"
                   "def f(x) do\n  b = bar(x)\n  y = foo(b, 1)\n  y\nend\n")
                  ("def f(xs) do\n  if xs do\n    Enum.map(xs, ‸&g/1)\n  end\nend\n" "fun"
                   "def f(xs) do\n  if xs do\n    fun = &g/1\n    Enum.map(xs, fun)\n  end\nend\n")
                  ("def f do\n  foo(‸%{\n    a: 1\n  })\nend\n" "m"
                   "def f do\n  m = %{\n    a: 1\n  }\n  foo(m)\nend\n")
                  ("def f(x) do\n  ‸bar(x)\n  |> baz()\nend\n" "b"
                   "def f(x) do\n  b = bar(x)\n  b\n  |> baz()\nend\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-extract-variable (cadr case))
      (should (equal (buffer-string) (caddr case))))))

(ert-deftest combobulate-test-elixir-extract-variable-extracts-the-region ()
  (combobulate-test-elixir "def f(x) do\n  ‸x + 1 + 2\nend\n"
    (set-mark (point))
    (forward-char 5)
    (activate-mark)
    (combobulate-elixir-extract-variable "s")
    (should (equal (buffer-string) "def f(x) do\n  s = x + 1\n  s + 2\nend\n"))))

(ert-deftest combobulate-test-elixir-extract-variable-refuses-what-would-change-the-code ()
  (dolist (case '(("def f(x) do\n  {:ok, ‸y} = x\nend\n" . "v")
                  ("case x do\n  {:ok, ‸y} -> y\nend\n" . "v")
                  ("def f(‸x) do\n  x\nend\n" . "v")
                  ("with {:ok, a} <- f(),\n     {:ok, b} <- ‸g(a) do\n  b\nend\n" . "v")
                  ("x = if c, do: ‸expensive()\n" . "v")
                  ("x = a && ‸f()\n" . "v")
                  ("x |> ‸foo()\n" . "v")
                  ("Enum.map(xs, fn x -> ‸x * 2 end)\n" . "v")
                  ("def f(x) do\n  y = foo(‸bar(x))\n  x + y\nend\n" . "x")
                  ("def f(x) do\n  y = foo(‸bar(x))\nend\n" . "Bar")))
    (combobulate-test-elixir (car case)
      (should-error (combobulate-elixir-extract-variable (cdr case)) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" (car case)))))))

(ert-deftest combobulate-test-elixir-inline-variable-replaces-the-use-with-the-value ()
  (dolist (case '(("def f(x) do\n  ‸y = bar(x)\n  foo(y, 1)\nend\n" "def f(x) do\n  foo(bar(x), 1)\nend\n")
                  ("def f(x) do\n  y = ‸x + 1\n  y * 2\nend\n" "def f(x) do\n  (x + 1) * 2\nend\n")
                  ("def f(x) do\n  ‸y = bar(x)\n  y |> baz()\nend\n" "def f(x) do\n  bar(x) |> baz()\nend\n")
                  ("def f do\n  ‸m = %{\n    a: 1\n  }\n  foo(m)\nend\n" "def f do\n  foo(%{\n    a: 1\n  })\nend\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-inline-variable)
      (should (equal (buffer-string) (cadr case))))))

(ert-deftest combobulate-test-elixir-inline-variable-round-trips-extract-variable ()
  (combobulate-test-elixir "def f(x) do\n  y = foo(‸bar(x), 1)\n  y\nend\n"
    (combobulate-elixir-extract-variable "b")
    (combobulate-elixir-inline-variable)
    (should (equal (buffer-string) "def f(x) do\n  y = foo(bar(x), 1)\n  y\nend\n"))))

(ert-deftest combobulate-test-elixir-inline-variable-refuses-what-would-change-the-code ()
  (dolist (source '("def f do\n  ‸y = g()\n  y + y\nend\n"
                    "def f do\n  ‸y = g()\n  :ok\nend\n"
                    "def f(z) do\n  ‸y = z + 1\n  z = 2\n  y\nend\n"
                    "def f do\n  ‸y = g()\n  y = 2\n  y\nend\n"
                    "def f(xs) do\n  ‸y = g()\n  Enum.map(xs, fn x -> x + y end)\nend\n"
                    "def f(z) do\n  ‸y = g()\n  ^y = z\nend\n"
                    "def f do\n  ‸{:ok, y} = g()\n  y\nend\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-inline-variable) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(defun combobulate-test-elixir--mark-to (marker)
  "Delete MARKER after point and mark the text between point and it."
  (let ((start (point)))
    (search-forward marker)
    (delete-char -1)
    (set-mark start)
    (activate-mark)))

(ert-deftest combobulate-test-elixir-extract-function-moves-the-region-to-a-private-function ()
  (dolist (case '(("defmodule M do\n  def f(x, y) do\n    a = x + 1\n    ‸b = a * y\n    IO.puts(b)¦\n  end\nend\n" "show"
                   "defmodule M do\n  def f(x, y) do\n    a = x + 1\n    show(a, y)\n  end\n\n  defp show(a, y) do\n    b = a * y\n    IO.puts(b)\n  end\nend\n")
                  ("defmodule M do\n  def f(x) do\n    ‸a = x + 1¦\n    b = a * 2\n    a + b\n  end\nend\n" "calc"
                   "defmodule M do\n  def f(x) do\n    a = calc(x)\n    b = a * 2\n    a + b\n  end\n\n  defp calc(x) do\n    a = x + 1\n    a\n  end\nend\n")
                  ("defmodule M do\n  def f(x) do\n    ‸a = x + 1\n    b = a * 2¦\n    a + b\n  end\nend\n" "calc"
                   "defmodule M do\n  def f(x) do\n    {a, b} = calc(x)\n    a + b\n  end\n\n  defp calc(x) do\n    a = x + 1\n    b = a * 2\n    {a, b}\n  end\nend\n")
                  ("defmodule M do\n  def f(x) do\n    foo(‸x * 2 + 1¦)\n  end\nend\n" "double"
                   "defmodule M do\n  def f(x) do\n    foo(double(x))\n  end\n\n  defp double(x) do\n    x * 2 + 1\n  end\nend\n")
                  ("defmodule M do\n  def f do\n    ‸IO.puts(1)¦\n  end\nend\n" "hello"
                   "defmodule M do\n  def f do\n    hello()\n  end\n\n  defp hello do\n    IO.puts(1)\n  end\nend\n")
                  ("defmodule M do\n  def f(0), do: 0\n\n  def f(x) do\n    ‸IO.puts(x)¦\n  end\n\n  def g, do: 1\nend\n" "say"
                   "defmodule M do\n  def f(0), do: 0\n\n  def f(x) do\n    say(x)\n  end\n\n  defp say(x) do\n    IO.puts(x)\n  end\n\n  def g, do: 1\nend\n")))
    (combobulate-test-elixir (car case)
      (combobulate-test-elixir--mark-to "¦")
      (combobulate-elixir-extract-function (cadr case))
      (should (equal (buffer-string) (caddr case))))))

(ert-deftest combobulate-test-elixir-extract-function-takes-the-parameters-given ()
  (combobulate-test-elixir "defmodule M do\n  def f(x) do\n    ‸IO.puts(x)¦\n  end\nend\n"
    (combobulate-test-elixir--mark-to "¦")
    (combobulate-elixir-extract-function "say" "value")
    (should (equal (buffer-string)
                   "defmodule M do\n  def f(x) do\n    say(value)\n  end\n\n  defp say(value) do\n    IO.puts(x)\n  end\nend\n"))))

(ert-deftest combobulate-test-elixir-extract-function-refuses-what-it-cannot-move ()
  (dolist (source '("defmodule M do\n  ‸@x 1¦\nend\n"
                    "defmodule M do\n  def f(x) do\n    ‸a = x + 1\n    b = a¦ * 2\n  end\nend\n"
                    "defmodule M do\n  def f(x) do\n    {:ok, ‸a¦} = x\n  end\nend\n"
                    "defmodule M do\n  def f(x) do\n    x |> ‸foo()¦\n  end\nend\n"
                    "defmodule M do\n  def f(xs) do\n    Enum.map(xs, &(‸&1 + 1¦))\n  end\nend\n"))
    (combobulate-test-elixir source
      (combobulate-test-elixir--mark-to "¦")
      (should-error (combobulate-elixir-extract-function "g") :type 'user-error)
      (should (equal (buffer-string) (string-replace "¦" "" (string-replace "‸" "" source)))))))

(ert-deftest combobulate-test-elixir-toggle-pipe-chain-round-trips ()
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-pipe-chain
                                        "‸c(b(a(x), y), z)\n" "x |> a() |> b(y) |> c(z)\n")
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-pipe-chain
                                        "‸Enum.map(Enum.filter(rows, &f/1), &g/1)\n"
                                        "rows |> Enum.filter(&f/1) |> Enum.map(&g/1)\n")
  (combobulate-test-elixir--round-trips combobulate-elixir-toggle-pipe-chain
                                        "‸foo(bar(a == b))\n" "(a == b) |> bar() |> foo()\n"))

(ert-deftest combobulate-test-elixir-toggle-pipe-chain-pipes-the-nest-around-point ()
  (dolist (case '(("c(b(‸a(x), y), z)\n" "x |> a() |> b(y) |> c(z)\n")
                  ("x |> foo(bar(‸y))\n" "x |> foo(y |> bar())\n")
                  ("‸c(b(x)) |> d()\n" "d(c(b(x)))\n")
                  ("c(‸b(x)) |> d()\n" "x |> b() |> c() |> d()\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-toggle-pipe-chain)
      (should (equal (buffer-string) (cadr case))))))

(ert-deftest combobulate-test-elixir-toggle-pipe-chain-unpipes-the-whole-pipeline ()
  (dolist (case '(("x\n|> ‸a()\n|> b(y)\n" "b(a(x), y)\n")
                  ("x |> ‸foo |> bar()\n" "bar(foo(x))\n")))
    (combobulate-test-elixir (car case)
      (combobulate-elixir-toggle-pipe-chain)
      (should (equal (buffer-string) (cadr case))))))

(ert-deftest combobulate-test-elixir-toggle-pipe-chain-refuses-what-it-cannot-convert ()
  (dolist (source '("‸foo()\n" "x |> ‸case do\n  _ -> 1\nend\n"))
    (combobulate-test-elixir source
      (should-error (combobulate-elixir-toggle-pipe-chain) :type 'user-error)
      (should (equal (buffer-string) (string-replace "‸" "" source))))))

(ert-deftest combobulate-test-elixir-toggle-pipe-with-a-prefix-toggles-the-chain ()
  (combobulate-test-elixir "‸c(b(a(x)))\n"
    (combobulate-elixir-toggle-pipe t)
    (should (equal (buffer-string) "x |> a() |> b() |> c()\n"))))
