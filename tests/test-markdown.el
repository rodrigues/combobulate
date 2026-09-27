;;; test-markdown.el --- Tests for Markdown and the languages embedded in it  -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))
(require 'combobulate)
(require 'combobulate-test-prelude)

;; The test `html-ts-mode' predates the one `markdown-ts-mode' reads these from.
(defvar html-ts-mode--font-lock-settings nil)
(defvar html-ts-mode--treesit-font-lock-feature-list nil)

(defmacro combobulate-test-markdown (source &rest body)
  "Run BODY in a `markdown-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'markdown-ts-mode)
                       (treesit-language-available-p 'markdown)
                       (treesit-language-available-p 'markdown-inline)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (markdown-ts-mode)
       (combobulate-mode)
       (treesit-update-ranges)
       (let ((combobulate-flash-node nil))
         ,@body))))

(ert-deftest combobulate-test-markdown-language-in-text-is-markdown ()
  (combobulate-test-markdown "- ‸one *two*\n- four\n"
    (should (eq (treesit-language-at (point)) 'markdown-inline))
    (should (eq (combobulate-primary-language) 'markdown))))

(ert-deftest combobulate-test-markdown-language-in-supported-fence ()
  (skip-unless (and (fboundp 'elixir-ts-mode) (treesit-language-available-p 'elixir)))
  (combobulate-test-markdown "# A\n\n```elixir\n‸x = 1\n```\n"
    (should (eq (combobulate-primary-language) 'elixir))))

(ert-deftest combobulate-test-markdown-language-in-unsupported-fence ()
  (combobulate-test-markdown "# A\n\n```bash\n‸echo 1\n```\n"
    (should (eq (combobulate-primary-language) 'markdown))))

(ert-deftest combobulate-test-markdown-next-from-list-item-text ()
  (combobulate-test-markdown "- ‸one *two*\n- four\n"
    (combobulate-navigate-next)
    (should (looking-at-p "- four"))))

(defconst combobulate-test-markdown-document
  "# Title\n\nIntro.\n\nSecond.\n\n## A\n\n- one\n- two\n  - nested\n  - nested2\n\n| a | b |\n|---|---|\n| 1 | 2 |\n| 3 | 4 |\n\n### A.1\n\n> quote\n\n## B\n\ntext\n")

(defun combobulate-test-markdown-at (marker)
  "Return `combobulate-test-markdown-document' with `‸' before MARKER."
  (let ((pos (string-search marker combobulate-test-markdown-document)))
    (concat (substring combobulate-test-markdown-document 0 pos) "‸"
            (substring combobulate-test-markdown-document pos))))

(ert-deftest combobulate-test-markdown-next-block-in-section ()
  (combobulate-test-markdown (combobulate-test-markdown-at "Intro.")
    (combobulate-navigate-next)
    (should (looking-at-p "Second\\."))))

(ert-deftest combobulate-test-markdown-next-block-skips-subsections ()
  (combobulate-test-markdown (combobulate-test-markdown-at "Second.")
    (combobulate-navigate-next)
    (should (looking-at-p "Second\\."))))

(ert-deftest combobulate-test-markdown-next-section-at-same-level ()
  (combobulate-test-markdown (combobulate-test-markdown-at "## A")
    (combobulate-navigate-next)
    (should (looking-at-p "## B"))
    (combobulate-navigate-previous)
    (should (looking-at-p "## A"))))

(ert-deftest combobulate-test-markdown-next-nested-list-item ()
  (combobulate-test-markdown (combobulate-test-markdown-at "nested\n")
    (combobulate-navigate-next)
    (should (looking-at-p "- nested2"))))

(ert-deftest combobulate-test-markdown-next-table-row ()
  (combobulate-test-markdown (combobulate-test-markdown-at "| 1")
    (combobulate-navigate-next)
    (should (looking-at-p "| 3"))))

(ert-deftest combobulate-test-markdown-next-table-cell ()
  (combobulate-test-markdown (combobulate-test-markdown-at "1 |")
    (combobulate-navigate-next)
    (should (looking-at-p " ?2 |"))))

(ert-deftest combobulate-test-markdown-down-from-section ()
  (combobulate-test-markdown (combobulate-test-markdown-at "## A")
    (combobulate-navigate-down)
    (should (looking-at-p "- one"))))

(ert-deftest combobulate-test-markdown-down-into-nested-list ()
  (combobulate-test-markdown (combobulate-test-markdown-at "- two")
    (combobulate-navigate-down)
    (should (looking-at-p "- nested\n"))))

(ert-deftest combobulate-test-markdown-down-into-table-row ()
  (combobulate-test-markdown (combobulate-test-markdown-at "| 1")
    (combobulate-navigate-down)
    (should (looking-at-p " ?1 |"))))

(ert-deftest combobulate-test-markdown-up-from-nested-list-item ()
  (combobulate-test-markdown (combobulate-test-markdown-at "- nested\n")
    (combobulate-navigate-up)
    (should (looking-at-p "- two"))))

(ert-deftest combobulate-test-markdown-beginning-of-defun-is-section ()
  (combobulate-test-markdown (combobulate-test-markdown-at "text")
    (combobulate-navigate-beginning-of-defun)
    (should (looking-at-p "## B"))))

(ert-deftest combobulate-test-markdown-drag-list-item-down ()
  (combobulate-test-markdown "‸- one\n- two\n  - nested\n\nAfter.\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "- two\n  - nested\n- one\n\nAfter.\n"))
    (should (looking-at-p "- one"))))

(ert-deftest combobulate-test-markdown-drag-list-item-from-its-text ()
  (combobulate-test-markdown "- ‸one\n- two\n\nAfter.\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "- two\n- one\n\nAfter.\n"))))

(ert-deftest combobulate-test-markdown-drag-into-last-nested-item ()
  (combobulate-test-markdown "- one\n  ‸- nested one\n  - nested two\n- two\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "- one\n  - nested two\n  - nested one\n- two\n"))))

(ert-deftest combobulate-test-markdown-previous-row-stops-before-header ()
  (combobulate-test-markdown "| a |\n|---|\n‸| 1 |\n| 2 |\n"
    (combobulate-navigate-previous)
    (should (looking-at-p "| 1 |"))))

(ert-deftest combobulate-test-markdown-drag-paragraph-from-its-middle ()
  (combobulate-test-markdown "# T\n\nfirst pa‸ra\n\nsecond\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "# T\n\nsecond\n\nfirst para\n"))))

(ert-deftest combobulate-test-markdown-drag-section-down ()
  (combobulate-test-markdown "# T\n\n‸## A\n\na\n\n## B\n\nb\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "# T\n\n## B\n\nb\n\n## A\n\na\n"))
    (should (looking-at-p "## A"))
    (combobulate-markdown-drag-up)
    (should (equal (buffer-string) "# T\n\n## A\n\na\n\n## B\n\nb\n"))))

(ert-deftest combobulate-test-markdown-drag-table-row-down ()
  (combobulate-test-markdown "| a |\n|---|\n‸| 1 |\n| 2 |\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "| a |\n|---|\n| 2 |\n| 1 |\n"))))

(defmacro combobulate-test-markdown-fence (language source &rest body)
  "Like `combobulate-test-markdown', skipped unless LANGUAGE has a grammar and mode."
  (declare (indent 2))
  `(progn
     (skip-unless (and (fboundp (intern (format "%s-ts-mode" ,language)))
                       (treesit-language-available-p ,language)))
     (combobulate-test-markdown ,source ,@body)))

(ert-deftest combobulate-test-markdown-elixir-fence-next ()
  (combobulate-test-markdown-fence 'elixir "# A\n\n```elixir\n‸x = 1\ny = 2\n```\n\nafter\n"
    (should (eq (combobulate-primary-language) 'elixir))
    (combobulate-navigate-next)
    (should (looking-at-p "y = 2"))
    (should-not (treesit-parser-list nil 'elixir))))

(ert-deftest combobulate-test-markdown-elixir-fence-down ()
  (combobulate-test-markdown-fence 'elixir "```elixir\n‸def f do\n  :ok\nend\n```\n"
    (combobulate-navigate-down)
    (should (looking-at-p ":ok"))))

(ert-deftest combobulate-test-markdown-elixir-fence-drag ()
  (combobulate-test-markdown-fence 'elixir "```elixir\n‸x = 1\ny = 2\n```\n"
    (combobulate-markdown-drag-down)
    (should (equal (buffer-string) "```elixir\ny = 2\nx = 1\n```\n"))))

(ert-deftest combobulate-test-markdown-back-to-markdown-after-fence ()
  (combobulate-test-markdown-fence 'elixir "# A\n\n‸```elixir\nx = 1\n```\n\nafter\n"
    (should (eq (combobulate-primary-language) 'markdown))
    (combobulate-navigate-next)
    (should (looking-at-p "after"))))

(ert-deftest combobulate-test-markdown-erlang-fence-next ()
  (combobulate-test-markdown-fence 'erlang "# A\n\n```erlang\n‸f() -> ok.\ng() -> ok.\n```\n"
    (should (eq (combobulate-primary-language) 'erlang))
    (combobulate-navigate-next)
    (should (looking-at-p "g()"))))

;;; Markdown in Elixir and Erlang doc strings

;; These rules stand in for the ones a user adds to `elixir-ts-mode' and
;; `erlang-ts-mode'; the major modes do not embed Markdown themselves.
;; They use `:pred' rather than `:match', which crashes Emacs 31 in
;; `erlang-ts-mode' by running `syntax-propertize' in the middle of a query.

(defun combobulate-test-markdown--heredoc-ranges (node _offset)
  "Return the lines of the heredoc NODE without the indentation the compiler strips."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-backward "\"")
    (let ((indent (- (point) (line-beginning-position)))
          (last (line-beginning-position))
          (ranges))
      (goto-char (treesit-node-start node))
      (forward-line 1)
      (while (< (point) last)
        (let ((bol (point)))
          (end-of-line)
          (push (cons (min (+ bol indent) (point)) (min (1+ (point)) last)) ranges)
          (forward-line 1)))
      (or (nreverse ranges) (cons last last)))))

(defun combobulate-test-markdown--doc-attribute-p (node)
  (member (treesit-node-text node t) '("doc" "moduledoc" "typedoc")))

(defun combobulate-test-markdown--triple-quoted-p (node)
  (string-match-p "\\`\\(?:~[sS]\\)?\"\"\"" (treesit-node-text node t)))

(defun combobulate-test-markdown--doc-range-rules (host query fence-language)
  "Return rules embedding Markdown in the HOST nodes QUERY captures.

Fenced FENCE-LANGUAGE blocks and indented code blocks in the Markdown
embed HOST again, as ExDoc treats indented code as Elixir."
  (treesit-range-rules
   ;; A function rather than `markdown', which `markdown-mode' defines as a command.
   :embed (lambda (_node) 'markdown) :host host :local t
   :range-fn #'combobulate-test-markdown--heredoc-ranges
   query
   :embed host :host 'markdown :local t
   `((fenced_code_block (info_string (language) @_lang)
                        (code_fence_content) @content
                        (:equal ,fence-language @_lang))
     (indented_code_block) @content)))

(defmacro combobulate-test-markdown-doc (mode source &rest body)
  "Run BODY in a MODE buffer holding SOURCE, with Markdown in its doc strings."
  (declare (indent 2))
  `(progn
     (skip-unless (and (fboundp ',mode)
                       (treesit-language-available-p 'markdown)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (,mode)
       (setq-local treesit-range-settings
                   (append treesit-range-settings
                           (pcase ',mode
                             ('elixir-ts-mode
                              (combobulate-test-markdown--doc-range-rules
                               'elixir
                               '((unary_operator
                                  operand: (call target: (identifier) @_name
                                                 (arguments [(string) (sigil)] @markdown))
                                  (:pred combobulate-test-markdown--doc-attribute-p @_name)
                                  (:pred combobulate-test-markdown--triple-quoted-p @markdown)))
                               "elixir"))
                             ('erlang-ts-mode
                              (combobulate-test-markdown--doc-range-rules
                               'erlang
                               '((wild_attribute
                                  name: (attr_name name: (atom) @_name)
                                  value: (string) @markdown
                                  (:pred combobulate-test-markdown--doc-attribute-p @_name)
                                  (:pred combobulate-test-markdown--triple-quoted-p @markdown)))
                               "erlang")))))
       (combobulate-mode)
       (treesit-update-ranges)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defconst combobulate-test-markdown-elixir-source
  "defmodule M do\n  defmodule N do\n    @moduledoc \"\"\"\n    # Title\n\n    - one\n    - two\n\n    ```elixir\n    x = 1\n    y = 2\n    ```\n    \"\"\"\n\n    @doc \"One line.\"\n    def f, do: 1\n\n    def g, do: 2\n  end\nend\n")

(defun combobulate-test-markdown-elixir-at (marker)
  "Return `combobulate-test-markdown-elixir-source' with `‸' before MARKER."
  (let ((pos (string-search marker combobulate-test-markdown-elixir-source)))
    (concat (substring combobulate-test-markdown-elixir-source 0 pos) "‸"
            (substring combobulate-test-markdown-elixir-source pos))))

(ert-deftest combobulate-test-markdown-elixir-doc-next-list-item ()
  (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "- one")
    (should (eq (combobulate-primary-language) 'markdown))
    (combobulate-elixir-navigate-next)
    (should (looking-at-p "- two"))))

(ert-deftest combobulate-test-markdown-elixir-doc-drag-list-item ()
  (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "- two")
    (combobulate-elixir-drag-up)
    (should (string-search "    - two\n    - one\n\n" (buffer-string)))))

(ert-deftest combobulate-test-markdown-elixir-doc-fence-is-elixir ()
  (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "x = 1")
    (should (eq (combobulate-primary-language) 'elixir))
    (combobulate-elixir-navigate-next)
    (should (looking-at-p "y = 2"))))

(ert-deftest combobulate-test-markdown-elixir-doc-indented-code-is-elixir ()
  (combobulate-test-markdown-doc elixir-ts-mode
      "defmodule M do\n  @moduledoc \"\"\"\n  Text.\n\n      ‸x = 1\n      y = 2\n  \"\"\"\nend\n"
    (should (eq (combobulate-primary-language) 'elixir))
    (combobulate-elixir-navigate-next)
    (should (looking-at-p "y = 2"))))

(ert-deftest combobulate-test-markdown-elixir-one-line-doc-stays-elixir ()
  (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "One line")
    (should (eq (combobulate-primary-language) 'elixir))))

(ert-deftest combobulate-test-markdown-elixir-outside-doc-is-elixir ()
  (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "def f")
    (combobulate-elixir-navigate-next)
    (should (looking-at-p "def g"))))

(ert-deftest combobulate-test-markdown-elixir-doc-with-markdown-command ()
  ;; `markdown-mode' defines a `markdown' command, which `treesit' would call as
  ;; a function that picks the embedded language.
  (cl-letf (((symbol-function 'markdown) (lambda (&rest _) "*markdown-output*")))
    (combobulate-test-markdown-doc elixir-ts-mode (combobulate-test-markdown-elixir-at "- one")
      (combobulate-elixir-navigate-next)
      (should (looking-at-p "- two")))))

(defconst combobulate-test-markdown-erlang-source
  "-module(m).\n-moduledoc \"\"\"\n# Title\n\n- one\n- two\n\"\"\".\n\n-doc \"One line.\".\nf() -> ok.\n\ng() -> ok.\n")

(ert-deftest combobulate-test-markdown-erlang-doc-next-list-item ()
  (combobulate-test-markdown-doc erlang-ts-mode
      (string-replace "- one" "‸- one" combobulate-test-markdown-erlang-source)
    (should (eq (combobulate-primary-language) 'markdown))
    (combobulate-navigate-next)
    (should (looking-at-p "- two"))))

(ert-deftest combobulate-test-markdown-erlang-doc-drag-list-item ()
  (combobulate-test-markdown-doc erlang-ts-mode
      (string-replace "- two" "‸- two" combobulate-test-markdown-erlang-source)
    (combobulate-erlang-drag-up)
    (should (string-search "- two\n- one\n\"\"\"." (buffer-string)))))

(ert-deftest combobulate-test-markdown-erlang-one-line-doc-stays-erlang ()
  (combobulate-test-markdown-doc erlang-ts-mode
      (string-replace "One line" "‸One line" combobulate-test-markdown-erlang-source)
    (should (eq (combobulate-primary-language) 'erlang))))

(provide 'test-markdown)
;;; test-markdown.el ends here
