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
