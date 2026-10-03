;;; combobulate-elixir.el --- elixir support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Victor Rodrigues

;; Author: Victor Rodrigues
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Supports the tree-sitter-elixir grammar.
;;
;; In that grammar `def', `case', `if' and `Enum.map(...)' are all
;; `call' nodes, and a pipeline is a chain of nested `binary_operator'
;; nodes.  Whether a node is a function head, a clause head or a
;; pipeline stage depends on its surroundings, not on its type, so
;; the sibling and hierarchy procedures delegate to the functions
;; below through a tree-sitter `:pred' predicate.
;;
;; The keys for next, previous and down run commands that call those
;; functions directly, because the procedure queries walk the whole
;; enclosing block and get slow in large modules.  The procedures
;; still serve the rest of Combobulate.
;;
;; Defun navigation is remapped to the `treesit-*-defun' commands,
;; which use the major mode's `treesit-defun-type-regexp' predicate.
;;
;; The Elixir-only commands have no keys by default.
;; This suggestion puts them under Combobulate's `C-c o' prefix,
;; so change that part if you customized `combobulate-key-prefix':
;;
;;   (with-eval-after-load 'combobulate-elixir
;;     (let ((map combobulate-elixir-map))
;;       (keymap-set map "C-c o n" #'combobulate-elixir-navigate-next-same-kind)
;;       (keymap-set map "C-c o p" #'combobulate-elixir-navigate-previous-same-kind)
;;       (keymap-set map "C-c o N" #'combobulate-elixir-navigate-next-occurrence)
;;       (keymap-set map "C-c o P" #'combobulate-elixir-navigate-previous-occurrence)
;;       (keymap-set map "C-c o <" #'combobulate-elixir-navigate-pipeline-head)
;;       (keymap-set map "C-c o >" #'combobulate-elixir-navigate-pipeline-last-stage)
;;       (keymap-set map "C-c o @" #'combobulate-elixir-navigate-function-attributes)
;;       (keymap-set map "C-c o m" #'combobulate-elixir-mark-function)
;;       (keymap-set map "C-c o [" #'combobulate-elixir-drag-function-up)
;;       (keymap-set map "C-c o ]" #'combobulate-elixir-drag-function-down)
;;       (keymap-set map "C-c o |" #'combobulate-elixir-toggle-pipe)
;;       (keymap-set map "C-c o &" #'combobulate-elixir-toggle-capture)
;;       (keymap-set map "C-c o d" #'combobulate-elixir-toggle-do-block)
;;       (keymap-set map "C-c o D" #'combobulate-elixir-toggle-private)
;;       (keymap-set map "C-c o j" #'combobulate-elixir-split-or-join)
;;       (keymap-set map "C-c o %" #'combobulate-elixir-toggle-keyword-map)
;;       (keymap-set map "C-c o k" #'combobulate-elixir-toggle-map-keys)
;;       (keymap-set map "C-c o \"" #'combobulate-elixir-toggle-heredoc)
;;       (keymap-set map "C-c o v" #'combobulate-elixir-extract-variable)
;;       (keymap-set map "C-c o i" #'combobulate-elixir-inline-variable)
;;       (keymap-set map "C-c o f" #'combobulate-elixir-extract-function)
;;       (keymap-set map "C-c o A" #'combobulate-elixir-toggle-multi-alias)))

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-cursor)
(require 'combobulate-rules)

(declare-function combobulate-heex-navigate-next-same-kind "combobulate-heex")
(declare-function combobulate-heex-navigate-previous-same-kind "combobulate-heex")

(defgroup combobulate-elixir nil
  "Configuration switches for Elixir"
  :group 'combobulate
  :prefix "combobulate-elixir-")

(defconst combobulate-elixir--wrappers
  '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "block" "arguments" "keywords" "map_content")
  "Node types that only group other nodes.")

(defconst combobulate-elixir--sequences
  '("arguments" "list" "tuple" "map_content" "keywords" "bitstring")
  "Node types whose children are comma-separated elements.")

(defconst combobulate-elixir--blocks
  '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "block" "anonymous_function")
  "Node types whose children are statements or clauses.")

(defun combobulate-elixir--type-p (node types)
  (and node (member (treesit-node-type node) types)))

(defun combobulate-elixir--child-of-type (node type)
  (seq-find (lambda (child) (equal (treesit-node-type child) type))
            (treesit-node-children node t)))

(defun combobulate-elixir--do-block (node)
  "Return the `do_block' of NODE if NODE is a call that has one."
  (and (equal (treesit-node-type node) "call")
       (combobulate-elixir--child-of-type node "do_block")))

(defun combobulate-elixir--pipe-p (node)
  (and (equal (treesit-node-type node) "binary_operator")
       (equal (treesit-node-text (treesit-node-child-by-field-name node "operator") t) "|>")))

(defun combobulate-elixir--pipe-stages (node)
  "Return the head and stages of the pipeline that the `|>' NODE belongs to."
  (while (combobulate-elixir--pipe-p (treesit-node-parent node))
    (setq node (treesit-node-parent node)))
  (let ((stages))
    (while (combobulate-elixir--pipe-p node)
      (push (treesit-node-child-by-field-name node "right") stages)
      (setq node (treesit-node-child-by-field-name node "left")))
    (cons node stages)))

(defun combobulate-elixir--pipeline-at (pos)
  "Return the whole `|>' chain of the pipeline nearest to POS."
  (when-let* ((node (treesit-parent-until (combobulate-elixir--node-at pos)
                                          #'combobulate-elixir--pipe-p t)))
    (while (combobulate-elixir--pipe-p (treesit-node-parent node))
      (setq node (treesit-node-parent node)))
    node))

(defun combobulate-elixir--elements (node)
  "Return the named children of NODE, with `keywords' replaced by its pairs.

Comments are left out because Combobulate never navigates to them."
  (mapcan (lambda (child)
            (pcase (treesit-node-type child)
              ("keywords" (treesit-node-children child t))
              ("comment" nil)
              (_ (list child))))
          (treesit-node-children node t)))

(defun combobulate-elixir--do-keyword-p (arguments)
  "Return non-nil if ARGUMENTS has a `do:' pair, as in `def f, do: x'."
  (let ((keywords (combobulate-elixir--child-of-type arguments "keywords")))
    (and keywords
         (seq-find (lambda (pair)
                     (equal (string-trim (treesit-node-text
                                          (treesit-node-child-by-field-name pair "key") t))
                            "do:"))
                   (treesit-node-children keywords t)))))

(defun combobulate-elixir--head-p (arguments)
  "Return non-nil if ARGUMENTS is the head of a clause or of a `do' block call.

A call written with a `do:' keyword counts as having a `do' block.
`with' and `for' are excluded because their heads hold clauses that
are worth navigating between."
  (let ((owner (treesit-node-parent arguments)))
    (or (equal (treesit-node-type owner) "stab_clause")
        (and (equal (treesit-node-type owner) "call")
             (or (combobulate-elixir--do-block owner)
                 (combobulate-elixir--do-keyword-p arguments))
             (not (member (treesit-node-text (treesit-node-child-by-field-name owner "target") t)
                          '("with" "for")))))))

(defun combobulate-elixir--point-at (pos)
  "Return POS, or the line's first non-blank position if POS is in indentation."
  (save-excursion
    (goto-char pos)
    (when (looking-back "^[ \t]*" (line-beginning-position))
      (skip-chars-forward " \t"))
    (point)))

(defun combobulate-elixir--node-at (pos)
  "Return the smallest named node at POS, skipping leading indentation."
  (let ((pos (combobulate-elixir--point-at pos)))
    (treesit-node-descendant-for-range (combobulate-buffer-root-node 'elixir) pos pos t)))

(defun combobulate-elixir--outermost-at (node)
  "Return the largest node that starts where NODE starts and is not a wrapper."
  (let ((start (treesit-node-start node))
        (best (unless (combobulate-elixir--type-p node combobulate-elixir--wrappers) node)))
    (while (and (setq node (treesit-node-parent node))
                (= (treesit-node-start node) start))
      (unless (combobulate-elixir--type-p node combobulate-elixir--wrappers)
        (setq best node)))
    best))

(defun combobulate-elixir--siblings (pos)
  "Return the nodes that are siblings of the node at POS."
  (let* ((at (combobulate-elixir--node-at pos))
         (node (or (combobulate-elixir--outermost-at at) at))
         (parent))
    (catch 'done
      (while (setq parent (treesit-node-parent node))
        (cond
         ((combobulate-elixir--pipe-p parent)
          (throw 'done (combobulate-elixir--pipe-stages parent)))
         ((and (equal (treesit-node-type parent) "arguments")
               (combobulate-elixir--head-p parent))
          (setq node (treesit-node-parent parent)))
         ((equal (treesit-node-type parent) "keywords")
          (throw 'done (combobulate-elixir--elements (treesit-node-parent parent))))
         ((combobulate-elixir--type-p parent (append combobulate-elixir--sequences
                                                     combobulate-elixir--blocks))
          (throw 'done (combobulate-elixir--elements parent)))
         (t (setq node parent)))))))

(defun combobulate-elixir--thing-at (pos)
  "Return the node at POS, grown from a call's target to the whole call."
  (let ((node (combobulate-elixir--node-at pos))
        (parent))
    (while (and (setq parent (treesit-node-parent node))
                (pcase (treesit-node-type parent)
                  ((or "call" "access_call")
                   (treesit-node-eq node (treesit-node-child-by-field-name parent "target")))
                  ((or "dot" "unary_operator") t)))
      (setq node parent))
    node))

(defun combobulate-elixir--inside (node)
  "Return the nodes directly inside NODE that navigating down can land on."
  (pcase (treesit-node-type node)
    ("call"
     (let ((container (or (combobulate-elixir--do-block node)
                          (combobulate-elixir--child-of-type node "arguments"))))
       (and container (combobulate-elixir--elements container))))
    ("stab_clause"
     (let ((body (treesit-node-child-by-field-name node "right")))
       (and body (combobulate-elixir--elements body))))
    ("binary_operator"
     (if (combobulate-elixir--pipe-p node)
         (combobulate-elixir--pipe-stages node)
       (list (treesit-node-child-by-field-name node "right"))))
    ("unary_operator"
     (let ((operand (treesit-node-child-by-field-name node "operand")))
       (if (equal (treesit-node-type operand) "call")
           (combobulate-elixir--inside operand)
         (list operand))))
    ("pair" (list (treesit-node-child-by-field-name node "value")))
    ("map"
     (let ((content (combobulate-elixir--child-of-type node "map_content")))
       (and content (combobulate-elixir--elements content))))
    ((or "string" "charlist" "sigil" "quoted_atom" "quoted_keyword") nil)
    (_ (combobulate-elixir--elements node))))

(defun combobulate-elixir--down-target (pos)
  "Return the node that navigating down from POS lands on."
  (let* ((pos (combobulate-elixir--point-at pos))
         (at (combobulate-elixir--node-at pos))
         (thing (combobulate-elixir--thing-at pos)))
    (cl-flet ((first-after (nodes)
                (seq-find (lambda (n) (> (treesit-node-start n) pos)) nodes)))
      (or (first-after (combobulate-elixir--inside thing))
          ;; On a leaf that starts an assignment, pipeline or clause, enter that instead.
          (and (= (treesit-node-start at) pos)
               (let ((outermost (combobulate-elixir--outermost-at at)))
                 (and outermost (first-after (combobulate-elixir--inside outermost)))))
          ;; On a leaf in a function or clause head, enter the body the head introduces.
          (let ((node thing) (target))
            (while (and (not target) (setq node (treesit-node-parent node)))
              (let ((body (pcase (treesit-node-type node)
                            ("call" (combobulate-elixir--do-block node))
                            ("stab_clause" (treesit-node-child-by-field-name node "right")))))
                (when (and body (< pos (treesit-node-start body)))
                  (setq target (first-after (combobulate-elixir--elements body))))))
            target)))))

(defvar-local combobulate-elixir--cache nil
  "The last navigation result, as (KIND POINT TICK RESULT STARTS).

STARTS is a hash table of the start positions of the nodes in
RESULT, so the `:pred' predicates can reject most nodes cheaply.")

(defun combobulate-elixir--cached (kind fn)
  "Return FN applied to point and a table of its start positions.

The `:pred' predicates run once per candidate node, so the answer is
computed once and reused while point and the buffer are unchanged."
  (pcase-let ((`(,k ,pt ,tick . ,rest) combobulate-elixir--cache))
    (if (and (eq k kind) (eql pt (point)) (eql tick (buffer-chars-modified-tick)))
        rest
      (let* ((result (funcall fn (point)))
             (starts (make-hash-table)))
        (dolist (node (ensure-list result))
          (puthash (treesit-node-start node) t starts))
        (setq combobulate-elixir--cache
              (list kind (point) (buffer-chars-modified-tick) result starts))
        (list result starts)))))

(defun combobulate-elixir--sibling-p (node)
  (pcase-let ((`(,siblings ,starts) (combobulate-elixir--cached 'sibling #'combobulate-elixir--siblings)))
    (and (gethash (treesit-node-start node) starts)
         (seq-find (lambda (sibling) (treesit-node-eq sibling node)) siblings))))

(defun combobulate-elixir--down-p (node)
  "Match the down target, or, when there is none, the nodes around point.

Matching the nodes around point when there is no target stops the
procedure from retrying the query on every ancestor; navigation then
ignores them because they do not start after point."
  (pcase-let ((`(,target ,starts) (combobulate-elixir--cached 'down #'combobulate-elixir--down-target)))
    (if target
        (and (gethash (treesit-node-start node) starts)
             (treesit-node-eq target node))
      (<= (treesit-node-start node) (point) (treesit-node-end node)))))

(defun combobulate-elixir--grows-p (node parent backward)
  "Return non-nil if the sexp NODE extends to PARENT, which shares its edge."
  (let ((field (treesit-node-field-name node)))
    (pcase (treesit-node-type parent)
      ("call" (or (equal field "target")
                  (and backward
                       (or (equal (treesit-node-type node) "do_block")
                           (and (equal (treesit-node-type node) "arguments")
                                (equal (treesit-node-type (treesit-node-child node 0)) "("))))))
      ("access_call" (or backward (equal field "target")))
      ("dot" t)
      ("unary_operator" backward)
      ("arguments" (and (not backward)
                        (equal (treesit-node-type (treesit-node-parent parent)) "stab_clause")))
      ("stab_clause" (and (not backward) (equal field "left"))))))

(defun combobulate-elixir--sexp-at (pos backward)
  "Return the expression that starts at POS, or ends at POS if BACKWARD."
  (let* ((root (combobulate-buffer-root-node 'elixir))
         (node (if backward
                   (and (> pos (point-min))
                        (treesit-node-descendant-for-range root (1- pos) pos t))
                 (treesit-node-descendant-for-range root pos pos t)))
         (edge (lambda (n) (if backward (treesit-node-end n) (treesit-node-start n))))
         (parent))
    (when (and node (= (funcall edge node) pos))
      (while (and (setq parent (treesit-node-parent node))
                  (= (funcall edge parent) pos)
                  (combobulate-elixir--grows-p node parent backward))
        (setq node parent))
      node)))

(defun combobulate-elixir-forward-sexp (&optional arg)
  "Move forward over ARG Elixir expressions, or backward if ARG is negative.

From `def' this moves over the whole definition, and from
`Keyword.get' over the whole call.  Where no expression starts,
fall back to `forward-sexp-default-function'.  Inside an embedded
language, such as a `~H' sigil, use Combobulate's navigation for it."
  (setq arg (or arg 1))
  (if (combobulate-embedded-language 'elixir)
      (combobulate-forward-sexp-function arg)
    (let ((backward (< arg 0)))
      (dotimes (_ (abs arg))
        (forward-comment (if backward (- (buffer-size)) (buffer-size)))
        (let ((node (combobulate-elixir--sexp-at (point) backward)))
          (if node
              (goto-char (if backward (treesit-node-start node) (treesit-node-end node)))
            (forward-sexp-default-function (if backward -1 1))))))))

(defun combobulate-elixir--skip-indentation ()
  "Move to the line's first node when point is in indentation.

Combobulate otherwise resolves indentation to the enclosing block."
  (when (looking-back "^[ \t]*" (line-beginning-position))
    (skip-chars-forward " \t")))

(defun combobulate-elixir--anchor ()
  "Return the start of the smallest non-wrapper node at point.

This is the position Combobulate's sibling navigation works from.
`else', `rescue', `catch' and `after' blocks count as nodes here
because they are siblings of the statements before them."
  (let ((node (combobulate-elixir--node-at (point))))
    (while (and node (combobulate-elixir--type-p
                      node '("source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content")))
      (setq node (treesit-node-parent node)))
    (if node (treesit-node-start node) (point))))

(defun combobulate-elixir--navigate (arg fallback find)
  "Move ARG times to the node FIND returns, or run FALLBACK if point is embedded.

The commands below compute their targets directly instead of going
through the procedure queries, which walk the whole enclosing block
and get slow in large modules."
  (combobulate-elixir--skip-indentation)
  (unless (combobulate-run-embedded-command 'elixir fallback arg)
    (dotimes (_ (or arg 1))
      (combobulate-visual-move-to-node (funcall find)))))

(defun combobulate-elixir-navigate-next (&optional arg)
  "Move to the next sibling ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-next
   (lambda ()
     (skip-chars-forward combobulate-skip-prefix-regexp)
     (let ((anchor (combobulate-elixir--anchor)))
       (seq-find (lambda (node) (> (treesit-node-start node) anchor))
                 (combobulate-elixir--siblings anchor))))))

(defun combobulate-elixir-navigate-previous (&optional arg)
  "Move to the previous sibling ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-previous
   (lambda ()
     (combobulate-elixir--skip-indentation)
     (let ((anchor (combobulate-elixir--anchor)))
       (car (last (seq-filter (lambda (node) (< (treesit-node-start node) anchor))
                              (combobulate-elixir--siblings anchor))))))))

(defun combobulate-elixir--kind (node)
  "Return a string naming the kind of NODE for same-kind navigation.

Calls are grouped by keyword, with private forms such as `defp'
counting as their public form.  Module attributes and sigils are
grouped by name and binary operators by operator."
  (let ((field-text (lambda (n field)
                      (treesit-node-text (treesit-node-child-by-field-name n field) t))))
    (pcase (treesit-node-type node)
      ("call"
       (let ((name (funcall field-text node "target")))
         (if (string-match (rx bos (group "def" (* alpha)) "p" eos) name)
             (match-string 1 name)
           name)))
      ("unary_operator"
       (let ((operand (treesit-node-child-by-field-name node "operand")))
         (concat (funcall field-text node "operator")
                 (if (equal (treesit-node-type operand) "call")
                     (funcall field-text operand "target")
                   (treesit-node-text operand t)))))
      ("binary_operator" (concat "binary_operator " (funcall field-text node "operator")))
      ("sigil" (concat "~" (treesit-node-text (combobulate-elixir--child-of-type node "sigil_name") t)))
      (type type))))

(defun combobulate-elixir--same-kind-target (direction)
  "Return the nearest sibling in DIRECTION of the same kind as the one at point."
  (let* ((anchor (combobulate-elixir--anchor))
         (siblings (combobulate-elixir--siblings anchor))
         (current (seq-find (lambda (node)
                              (and (<= (treesit-node-start node) anchor)
                                   (< anchor (treesit-node-end node))))
                            siblings))
         (kind (and current (combobulate-elixir--kind current)))
         (same (seq-filter (lambda (node) (equal (combobulate-elixir--kind node) kind))
                           siblings)))
    (when kind
      (if (eq direction 'next)
          (seq-find (lambda (node) (> (treesit-node-start node) anchor)) same)
        (car (last (seq-filter (lambda (node) (< (treesit-node-start node) anchor)) same)))))))

(defun combobulate-elixir--same-kind-fallback (heex-command command)
  "Return HEEX-COMMAND if point is in HEEx, else COMMAND.

Only HEEx has same-kind navigation, so the other embedded languages
get plain sibling navigation."
  (combobulate-elixir--skip-indentation)
  (if (eq (combobulate-embedded-language 'elixir) 'heex) heex-command command))

(defun combobulate-elixir-navigate-next-same-kind (&optional arg)
  "Move to the next sibling of the same kind ARG times.

From `def' this skips `@doc', `@spec' and other statements to reach
the next `def' or `defp'; from `@doc' it reaches the next `@doc'."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg (combobulate-elixir--same-kind-fallback #'combobulate-heex-navigate-next-same-kind
                                              #'combobulate-navigate-next)
   (lambda ()
     (skip-chars-forward combobulate-skip-prefix-regexp)
     (combobulate-elixir--same-kind-target 'next))))

(defun combobulate-elixir-navigate-previous-same-kind (&optional arg)
  "Move to the previous sibling of the same kind ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg (combobulate-elixir--same-kind-fallback #'combobulate-heex-navigate-previous-same-kind
                                              #'combobulate-navigate-previous)
   (lambda () (combobulate-elixir--same-kind-target 'previous))))

(defun combobulate-elixir--occurrence-target (direction)
  "Return the nearest node in DIRECTION of the same kind as the one at point.

Unlike `combobulate-elixir--same-kind-target', this searches the whole buffer."
  (let* ((node (combobulate-elixir--thing-at (point)))
         (node (if (equal (treesit-node-type node) "sigil_name") (treesit-node-parent node) node))
         (kind (combobulate-elixir--kind node))
         (start (treesit-node-start node))
         (query `((,(intern (treesit-node-type node))) @node))
         (same (seq-filter (lambda (candidate) (equal (combobulate-elixir--kind candidate) kind))
                           (treesit-query-capture (combobulate-buffer-root-node 'elixir) query nil nil t))))
    (if (eq direction 'next)
        (seq-find (lambda (candidate) (> (treesit-node-start candidate) start)) same)
      (car (last (seq-filter (lambda (candidate) (< (treesit-node-start candidate) start)) same))))))

(defun combobulate-elixir-navigate-next-occurrence (&optional arg)
  "Move to the next node of the same kind in the buffer ARG times.

Unlike \\[combobulate-elixir-navigate-next-same-kind], this is not
limited to siblings: from `Repo.query!' it reaches the next
`Repo.query!' in any function, and from `~SQL' the next `~SQL'."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg (combobulate-elixir--same-kind-fallback #'combobulate-heex-navigate-next-same-kind
                                              #'combobulate-navigate-next)
   (lambda ()
     (skip-chars-forward combobulate-skip-prefix-regexp)
     (combobulate-elixir--occurrence-target 'next))))

(defun combobulate-elixir-navigate-previous-occurrence (&optional arg)
  "Move to the previous node of the same kind in the buffer ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg (combobulate-elixir--same-kind-fallback #'combobulate-heex-navigate-previous-same-kind
                                              #'combobulate-navigate-previous)
   (lambda () (combobulate-elixir--occurrence-target 'previous))))

(defun combobulate-elixir--pipeline-stages-at-point ()
  "Return the head and stages of the innermost pipeline around point."
  (combobulate-elixir--skip-indentation)
  (combobulate-elixir--pipe-stages (or (combobulate-elixir--pipeline-at (point))
                                       (user-error "No pipeline at point"))))

(defun combobulate-elixir-navigate-pipeline-head ()
  "Move to the expression that the pipeline at point starts from."
  (interactive "^")
  (combobulate-visual-move-to-node (car (combobulate-elixir--pipeline-stages-at-point))))

(defun combobulate-elixir-navigate-pipeline-last-stage ()
  "Move to the last stage of the pipeline at point."
  (interactive "^")
  (combobulate-visual-move-to-node (car (last (combobulate-elixir--pipeline-stages-at-point)))))

(defun combobulate-elixir--function-attribute-p (node)
  (member (combobulate-elixir--kind node) '("@doc" "@spec" "@impl" "@deprecated")))

(defun combobulate-elixir-navigate-function-attributes ()
  "Move between the function at point and the `@doc' and `@spec' above it.

From any clause this moves to the first of the `@doc', `@spec',
`@impl' and `@deprecated' attributes above the first clause; from
one of those attributes it moves to the function they describe."
  (interactive "^")
  (combobulate-elixir--skip-indentation)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         (lambda (node)
                                           (or (combobulate-elixir--function-attribute-p node)
                                               (combobulate-elixir--signature node)))
                                         t)
                   (user-error "No function or function attribute at point")))
         (siblings (seq-remove (lambda (sibling) (equal (treesit-node-type sibling) "comment"))
                               (treesit-node-children (treesit-node-parent node) t)))
         (index (seq-position siblings node #'treesit-node-eq)))
    (combobulate-visual-move-to-node
     (if (combobulate-elixir--function-attribute-p node)
         (let ((next (seq-find (lambda (sibling) (not (combobulate-elixir--function-attribute-p sibling)))
                               (nthcdr index siblings))))
           (or (and next (combobulate-elixir--signature next) next)
               (user-error "No function after these attributes")))
       (let ((name-and-arity (cdr (combobulate-elixir--signature node)))
             (first-attribute))
         (while (and (> index 0)
                     (equal (cdr (combobulate-elixir--signature (nth (1- index) siblings))) name-and-arity))
           (setq index (1- index)))
         (while (and (> index 0) (combobulate-elixir--function-attribute-p (nth (1- index) siblings)))
           (setq index (1- index)
                 first-attribute (nth index siblings)))
         (or first-attribute (user-error "No `@doc' or `@spec' above this function")))))))

(defun combobulate-elixir--function-extra-p (node)
  "Return non-nil if NODE is a comment or attribute that can sit above a function."
  (or (equal (treesit-node-type node) "comment")
      (combobulate-elixir--function-attribute-p node)))

(defun combobulate-elixir--function-after (node)
  "Return the first sibling from NODE on that is not a comment or attribute."
  (let ((node node))
    (while (and node (combobulate-elixir--function-extra-p node))
      (setq node (treesit-node-next-sibling node t)))
    node))

(defun combobulate-elixir--blank-line-between-p (above below)
  (> (line-number-at-pos (treesit-node-start below))
     (1+ (line-number-at-pos (max (treesit-node-start above) (1- (treesit-node-end above)))))))

(defun combobulate-elixir--function-group-at (pos)
  "Return the nodes that make up the function at POS.

These are its clauses and the `@doc', `@spec', `@impl', `@deprecated'
and comments above them.  A comment with a blank line below it heads
a section rather than the function, so it is left out."
  (let* ((at (combobulate-elixir--node-at pos))
         (node (or (and (equal (treesit-node-type at) "comment")
                        (combobulate-elixir--signature (combobulate-elixir--function-after at))
                        at)
                   (treesit-parent-until at (lambda (node)
                                              (or (combobulate-elixir--function-attribute-p node)
                                                  (combobulate-elixir--signature node)))
                                         t)))
         (clause (and node (combobulate-elixir--function-after node)))
         (name-and-arity (and clause (cdr (combobulate-elixir--signature clause)))))
    (unless name-and-arity
      (user-error "No function at point"))
    (let* ((siblings (treesit-node-children (treesit-node-parent clause) t))
           (index (seq-position siblings clause #'treesit-node-eq))
           (clause-p (lambda (node) (equal (cdr (combobulate-elixir--signature node)) name-and-arity)))
           (member-p (lambda (node) (or (funcall clause-p node) (combobulate-elixir--function-extra-p node))))
           (first index)
           (last index))
      (while (and (> first 0) (funcall member-p (nth (1- first) siblings)))
        (setq first (1- first)))
      (while (and (< (1+ last) (length siblings)) (funcall member-p (nth (1+ last) siblings)))
        (setq last (1+ last)))
      (while (not (funcall clause-p (nth last siblings)))
        (setq last (1- last)))
      (while (and (< first index)
                  (equal (treesit-node-type (nth first siblings)) "comment")
                  (combobulate-elixir--blank-line-between-p (nth first siblings) (nth (1+ first) siblings)))
        (setq first (1+ first)))
      (seq-subseq siblings first (1+ last)))))

(defun combobulate-elixir-mark-function ()
  "Mark the function at point, with all its clauses and what documents it.

See `combobulate-elixir--function-group-at' for what the function
includes.  The region covers whole lines, so killing and yanking it
moves the function cleanly."
  (interactive)
  (let* ((group (combobulate-elixir--function-group-at (point)))
         (start (save-excursion (goto-char (treesit-node-start (car group))) (line-beginning-position)))
         (end (save-excursion (goto-char (treesit-node-end (car (last group))))
                              (min (point-max) (1+ (line-end-position))))))
    (push-mark end nil t)
    (goto-char start)))

(defun combobulate-elixir--neighbour-function (group direction)
  "Return the nodes of the function next to GROUP in DIRECTION.

Comments that head a section rather than a function are skipped."
  (let ((node (if (eq direction 'up)
                  (treesit-node-prev-sibling (car group) t)
                (treesit-node-next-sibling (car (last group)) t))))
    (while (and node (equal (treesit-node-type node) "comment"))
      (setq node (if (eq direction 'up)
                     (treesit-node-prev-sibling node t)
                   (treesit-node-next-sibling node t))))
    (let ((clause (and node (combobulate-elixir--function-after node))))
      (unless (and clause (combobulate-elixir--signature clause))
        (user-error "No function %s this one to swap with" (if (eq direction 'up) "above" "below")))
      (combobulate-elixir--function-group-at (treesit-node-start clause)))))

(defun combobulate-elixir--drag-function (direction)
  "Swap the function at point with the one in DIRECTION and move to it."
  (let* ((self (combobulate-elixir--function-group-at (point)))
         (other (combobulate-elixir--neighbour-function self direction))
         (range (lambda (group) (cons (treesit-node-start (car group)) (treesit-node-end (car (last group))))))
         (self-range (funcall range self))
         (other-range (funcall range other)))
    (if (eq direction 'up)
        (progn (transpose-subr-1 other-range self-range)
               (goto-char (car other-range)))
      (transpose-subr-1 self-range other-range)
      (goto-char (- (cdr other-range) (- (cdr self-range) (car self-range)))))))

(defun combobulate-elixir-drag-function-up (&optional arg)
  "Swap the function at point with the one above it ARG times.

A function includes all its clauses and what documents it; see
`combobulate-elixir--function-group-at'."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (combobulate-elixir--drag-function 'up)))

(defun combobulate-elixir-drag-function-down (&optional arg)
  "Swap the function at point with the one below it ARG times."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (combobulate-elixir--drag-function 'down)))

(defun combobulate-elixir--trimmed-range (node)
  "Return the range of NODE without trailing whitespace.

The grammar ends the last clause of a `case' or `fn' after the
newline before `end', so swapping untrimmed ranges moves that
newline."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-backward " \t\n" (treesit-node-start node))
    (cons (treesit-node-start node) (point))))

(defun combobulate-elixir--drag (direction)
  "Swap the sibling at point with its neighbour in DIRECTION.

Return the position where the sibling at point now starts."
  (let* ((anchor (combobulate-elixir--anchor))
         (siblings (combobulate-elixir--siblings anchor))
         (self (seq-find (lambda (node)
                           (and (<= (treesit-node-start node) anchor)
                                (< anchor (treesit-node-end node))))
                         siblings))
         (other (and self
                     (if (eq direction 'up)
                         (car (last (seq-filter (lambda (node) (< (treesit-node-start node)
                                                                  (treesit-node-start self)))
                                                siblings)))
                       (seq-find (lambda (node) (> (treesit-node-start node) (treesit-node-start self)))
                                 siblings)))))
    (unless self
      (user-error "Nothing to drag at point"))
    (unless other
      (user-error "No sibling to swap with in that direction"))
    (when (xor (equal (treesit-node-type self) "pair") (equal (treesit-node-type other) "pair"))
      (user-error "Keyword pairs must stay after the other elements"))
    (pcase-let* ((`(,first ,second) (if (eq direction 'up) (list other self) (list self other)))
                 (first-range (combobulate-elixir--trimmed-range first))
                 (second-range (combobulate-elixir--trimmed-range second))
                 (self-length (- (cdr (combobulate-elixir--trimmed-range self))
                                 (treesit-node-start self))))
      (transpose-subr-1 first-range second-range)
      (if (eq direction 'up)
          (car first-range)
        (- (cdr second-range) self-length)))))

(defun combobulate-elixir-drag-up (&optional arg)
  "Swap the sibling at point with the previous one ARG times.

Uses the same siblings as \\[combobulate-elixir-navigate-previous]."
  (interactive "^p")
  (combobulate-elixir--drag-command arg 'up #'combobulate-drag-up))

(defun combobulate-elixir-drag-down (&optional arg)
  "Swap the sibling at point with the next one ARG times.

Uses the same siblings as \\[combobulate-elixir-navigate-next]."
  (interactive "^p")
  (combobulate-elixir--drag-command arg 'down #'combobulate-drag-down))

(defun combobulate-elixir--drag-command (arg direction fallback)
  (combobulate-elixir--skip-indentation)
  (unless (combobulate-run-embedded-command 'elixir fallback arg)
    (dotimes (_ (or arg 1))
      (let ((start (combobulate-elixir--drag direction)))
        (combobulate-visual-move-to-node
         (combobulate-elixir--outermost-at (combobulate-elixir--node-at start)))))))

(defun combobulate-elixir-kill-node-dwim (&optional arg)
  "Like `combobulate-kill-node-dwim', but keep the node's trailing whitespace.

The last clause of a `case' or `fn' includes the newline before
`end', and killing it would pull `end' onto the previous line."
  (interactive "p")
  (unless (combobulate-run-embedded-command 'elixir #'combobulate-kill-node-dwim arg)
    (dotimes (_ (or arg 1))
      (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
        (when-let* ((nearest (save-excursion
                               (combobulate-skip-whitespace-forward t)
                               (combobulate--get-nearest-navigable-node)))
                    (node (or (combobulate-nav-get-self-sibling nearest) nearest))
                    (range (combobulate-elixir--trimmed-range node))
                    (proxy (combobulate-proxy-node-make-from-range (car range) (cdr range))))
          (unless (combobulate-node-on-or-after-point-p proxy)
            (error "No node to kill"))
          (let ((text (combobulate--consume-node proxy t)))
            (if (memq last-command '(combobulate-kill-node-dwim combobulate-elixir-kill-node-dwim))
                (kill-append text nil)
              (kill-new text)))
          (combobulate-message "Killed node" proxy))))))

(defun combobulate-elixir-clone-node-dwim (&optional arg)
  "Clone the sibling at point ARG times.

This is the node \\[combobulate-elixir-drag-down] moves, so a keyword
such as `def' is never cloned on its own."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (unless (combobulate-run-embedded-command 'elixir #'combobulate-clone-node-dwim arg)
    (dotimes (_ (or arg 1))
      (let* ((anchor (combobulate-elixir--anchor))
             (node (or (seq-find (lambda (node)
                                   (and (<= (treesit-node-start node) anchor)
                                        (< anchor (treesit-node-end node))))
                                 (combobulate-elixir--siblings anchor))
                       (user-error "Nothing to clone at point"))))
        (combobulate-message "Cloning" node)
        (combobulate--clone-node node (treesit-node-start node))))))

(defconst combobulate-elixir--unsplicable
  '("do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "stab_clause" "keywords" "map_content")
  "Node types that splicing must not replace.

Replacing a `do' block with its contents drops `do' and `end', and
replacing a clause drops its `->', which leaves invalid code.")

(defun combobulate-elixir--splice (command arg)
  "Run the splice COMMAND with ARG, offering only choices that keep valid code."
  (combobulate-elixir--skip-indentation)
  (let* ((anchor (combobulate-elixir--anchor))
         (current (seq-find (lambda (node)
                              (and (<= (treesit-node-start node) anchor)
                                   (< anchor (treesit-node-end node))))
                            (combobulate-elixir--siblings anchor))))
    (when (equal (treesit-node-type current) "stab_clause")
      (user-error "Clauses cannot live outside their `case', `cond' or `fn'")))
  (let ((proffer (symbol-function 'combobulate-proffer-choices)))
    (cl-letf (((symbol-function 'combobulate-proffer-choices)
               (lambda (nodes &rest args)
                 (apply proffer
                        (or (seq-remove (lambda (node)
                                          (combobulate-elixir--type-p node combobulate-elixir--unsplicable))
                                        nodes)
                            (user-error "Nothing to splice here without breaking the code"))
                        args))))
      (funcall command arg))))

(defun combobulate-elixir-splice-up (&optional arg)
  "Like `combobulate-splice-up', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-up arg))

(defun combobulate-elixir-splice-down (&optional arg)
  "Like `combobulate-splice-down', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-down arg))

(defun combobulate-elixir-splice-self (&optional arg)
  "Like `combobulate-splice-self', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-self arg))

(defun combobulate-elixir-splice-parent (&optional arg)
  "Like `combobulate-splice-parent', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-parent arg))

(defun combobulate-elixir-navigate-up (&optional arg)
  "Like `combobulate-navigate-up', but skipping leading indentation."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (combobulate-navigate-up arg))

(defun combobulate-elixir-navigate-down (&optional arg)
  "Move into the node at point ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-down
   (lambda ()
     (combobulate-elixir--skip-indentation)
     (combobulate-elixir--down-target (point)))))

(defun combobulate-elixir--keywords (node)
  "Return the start positions of the keywords that delimit NODE.

For a call with a `do' block these are the call's target, `do',
any `else', `rescue', `catch' or `after', and `end'.  For an
anonymous function they are `fn' and `end'."
  (let ((keywords (lambda (parent)
                    (seq-keep (lambda (child)
                                (and (member (treesit-node-type child)
                                             '("do" "else" "rescue" "catch" "after" "fn" "end"))
                                     (treesit-node-start child)))
                              (treesit-node-children parent)))))
    (pcase (treesit-node-type node)
      ("call"
       (let ((do-block (combobulate-elixir--do-block node)))
         (and do-block
              (append (list (treesit-node-start node))
                      (mapcan (lambda (child)
                                (if (member (treesit-node-type child)
                                            '("else_block" "rescue_block" "catch_block" "after_block"))
                                    (funcall keywords child)
                                  (and (member (treesit-node-type child) '("do" "end"))
                                       (list (treesit-node-start child)))))
                              (treesit-node-children do-block))))))
      ("anonymous_function" (funcall keywords node)))))

(defun combobulate-elixir--sequence-target (direction)
  "Return the next keyword position in DIRECTION among the constructs around point."
  (let ((node (combobulate-node-at (point) 'elixir))
        (target))
    (while (and node (not target))
      (let ((positions (combobulate-elixir--keywords node)))
        (setq target (if (eq direction 'next)
                         (seq-find (lambda (pos) (> pos (point))) positions)
                       (car (last (seq-filter (lambda (pos) (< pos (point))) positions))))))
      (setq node (treesit-node-parent node)))
    target))

(defun combobulate-elixir-navigate-sequence-next (&optional arg)
  "Move to the next keyword of the construct at point ARG times.

From `def' this visits `do', then `end'; from `with' also `else'.
Outside any construct, fall back to `combobulate-navigate-sequence-next'."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (dotimes (_ (or arg 1))
    (let ((target (and (not (combobulate-embedded-language 'elixir))
                       (combobulate-elixir--sequence-target 'next))))
      (if target
          (goto-char target)
        (setq this-command 'combobulate-navigate-sequence-next)
        (combobulate-navigate-sequence-next)))))

(defun combobulate-elixir-navigate-sequence-previous (&optional arg)
  "Move to the previous keyword of the construct at point ARG times.

Outside any construct, fall back to `combobulate-navigate-sequence-previous'."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (dotimes (_ (or arg 1))
    (let ((target (and (not (combobulate-embedded-language 'elixir))
                       (combobulate-elixir--sequence-target 'previous))))
      (if target
          (goto-char target)
        (setq this-command 'combobulate-navigate-sequence-previous)
        (combobulate-navigate-sequence-previous)))))

(defun combobulate-elixir-dbg-pipe ()
  "Append `|> dbg()' to the pipeline at point."
  (interactive)
  (let ((pipeline (or (combobulate-elixir--pipeline-at (point))
                      (user-error "No pipeline at point"))))
    (save-excursion
      (goto-char (treesit-node-end pipeline))
      (if (string-search "\n" (treesit-node-text pipeline t))
          (progn (newline)
                 (insert "|> dbg()")
                 (indent-according-to-mode))
        (insert " |> dbg()")))))

(defun combobulate-elixir--without-guard (node)
  "Return the left side of NODE if it is a `when' guard, else NODE."
  (if (and (equal (treesit-node-type node) "binary_operator")
           (equal (treesit-node-text (treesit-node-child-by-field-name node "operator") t) "when"))
      (treesit-node-child-by-field-name node "left")
    node))

(defun combobulate-elixir--head-name (head)
  "Return (NAME ARITY) for the function HEAD, where NAME is the node naming it."
  (let ((head (combobulate-elixir--without-guard head)))
    (pcase (treesit-node-type head)
      ("call"
       (let ((arguments (combobulate-elixir--child-of-type head "arguments")))
         (list (treesit-node-child-by-field-name head "target")
               (seq-count (lambda (child) (not (equal (treesit-node-type child) "comment")))
                          (and arguments (treesit-node-children arguments t))))))
      ("identifier" (list head 0)))))

(defun combobulate-elixir--signature (node)
  "Return (KEYWORD NAME ARITY) if NODE defines a function, macro or guard."
  (when-let* (((equal (treesit-node-type node) "call"))
              (keyword (treesit-node-text (treesit-node-child-by-field-name node "target") t))
              ((member keyword '("def" "defp" "defmacro" "defmacrop" "defguard" "defguardp")))
              (name (combobulate-elixir--head-name
                     (treesit-node-child (combobulate-elixir--child-of-type node "arguments") 0 t))))
    (list keyword (treesit-node-text (car name) t) (cadr name))))

(defun combobulate-elixir--named (node)
  "Return (NAME ARITY) if NODE defines a function or is its `@spec'.

NAME is the node naming the function."
  (if (combobulate-elixir--signature node)
      (combobulate-elixir--head-name
       (treesit-node-child (combobulate-elixir--child-of-type node "arguments") 0 t))
    (when-let* (((equal (combobulate-elixir--kind node) "@spec"))
                (spec (treesit-node-child-by-field-name node "operand"))
                (type (combobulate-elixir--without-guard
                       (treesit-node-child (combobulate-elixir--child-of-type spec "arguments") 0 t)))
                ((equal (treesit-node-text (treesit-node-child-by-field-name type "operator") t) "::")))
      (combobulate-elixir--head-name (treesit-node-child-by-field-name type "left")))))

(defun combobulate-elixir--call-name (node)
  "Return (NAME ARITY) if NODE calls or captures a local function.

NAME is the node naming the function.  A call piped into takes one
more argument than it shows."
  (let* ((parent (treesit-node-parent node))
         (piped (if (and (combobulate-elixir--pipe-p parent)
                         (treesit-node-eq node (treesit-node-child-by-field-name parent "right")))
                    1
                  0)))
    (pcase (treesit-node-type node)
      ("call"
       (let ((target (treesit-node-child-by-field-name node "target"))
             (arguments (combobulate-elixir--child-of-type node "arguments")))
         (and (equal (treesit-node-type target) "identifier")
              (list target (+ piped
                              (if (combobulate-elixir--do-block node) 1 0)
                              (seq-count (lambda (child) (not (equal (treesit-node-type child) "comment")))
                                         (and arguments (treesit-node-children arguments t))))))))
      ("identifier" (and (= piped 1) (list node 1)))
      ("unary_operator"
       (let ((operand (combobulate-elixir--operand node)))
         (and (combobulate-elixir--capture-p node)
              (equal (treesit-node-text (treesit-node-child-by-field-name operand "operator") t) "/")
              (equal (treesit-node-type (treesit-node-child-by-field-name operand "left")) "identifier")
              (equal (treesit-node-type (treesit-node-child-by-field-name operand "right")) "integer")
              (list (treesit-node-child-by-field-name operand "left")
                    (string-to-number (treesit-node-text (treesit-node-child-by-field-name operand "right") t)))))))))

(defun combobulate-elixir--function-names (pos)
  "Return the nodes naming the function at POS in its clauses and `@spec's.

For a private function, which only its own module can call, this also
includes the calls and captures of it, but not those in a nested module."
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at pos) #'combobulate-elixir--named t)
                   (user-error "No function or `@spec' at point")))
         (scope (treesit-node-parent node))
         (key (lambda (named) (and named (list (treesit-node-text (car named) t) (cadr named)))))
         (target (funcall key (combobulate-elixir--named node)))
         (clauses (seq-filter (lambda (sibling) (equal (funcall key (combobulate-elixir--named sibling)) target))
                              (treesit-node-children scope t)))
         (names (mapcar (lambda (clause) (car (combobulate-elixir--named clause))) clauses)))
    (if (not (seq-some (lambda (clause)
                         (string-suffix-p "p" (or (car (combobulate-elixir--signature clause)) "")))
                       clauses))
        names
      (let ((calls (seq-keep
                    (lambda (call)
                      (let ((called (combobulate-elixir--call-name call)))
                        (and (equal (funcall key called) target)
                             (treesit-node-eq (treesit-parent-until
                                               call (lambda (node)
                                                      (or (treesit-node-eq node scope)
                                                          (member (combobulate-elixir--kind node)
                                                                  '("defmodule" "defimpl" "defprotocol")))))
                                              scope)
                             (car called))))
                    (combobulate-elixir--descendants scope #'combobulate-elixir--call-name))))
        (sort (seq-uniq (append names calls) #'treesit-node-eq)
              (lambda (a b) (< (treesit-node-start a) (treesit-node-start b))))))))

(defun combobulate-elixir-edit-function-name (&optional arg)
  "Edit the name of the function at point in all its clauses and `@spec's.

For a private function, also edit its calls and captures in the module.
Point goes to the start of each name; with one prefix ARG, to the
end, and with two, each name is marked.  In an embedded language,
run `combobulate-cursor-edit-sequence-dwim' instead, which in HEEx
edits the tag at point and its closing tag."
  (interactive "P")
  (combobulate-elixir--skip-indentation)
  (unless (combobulate-run-embedded-command 'elixir #'combobulate-cursor-edit-sequence-dwim arg)
    (combobulate-cursor-edit-nodes (combobulate-elixir--function-names (point))
                                   (combobulate-cursor-edit-node-determine-action arg))))

(defun combobulate-elixir-toggle-private ()
  "Switch the function at point between `def' and `defp', in every clause.

Macros switch between `defmacro' and `defmacrop', and guards between
`defguard' and `defguardp'."
  (interactive)
  (let* ((definition (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                               #'combobulate-elixir--signature t)
                         (user-error "No function definition at point")))
         (name-and-arity (cdr (combobulate-elixir--signature definition)))
         (clauses (seq-filter (lambda (node)
                                (equal (cdr (combobulate-elixir--signature node)) name-and-arity))
                              (treesit-node-children (treesit-node-parent definition) t))))
    (save-excursion
      (dolist (clause (reverse clauses))
        (let* ((target (treesit-node-child-by-field-name clause "target"))
               (keyword (treesit-node-text target t)))
          (goto-char (treesit-node-start target))
          (delete-region (treesit-node-start target) (treesit-node-end target))
          (insert (if (string-suffix-p "p" keyword) (substring keyword 0 -1) (concat keyword "p"))))))))

(defun combobulate-elixir--block-call-p (node)
  "Return non-nil if NODE is a call with a `do' block or a `do:' keyword."
  (and (equal (treesit-node-type node) "call")
       (or (combobulate-elixir--do-block node)
           (let ((arguments (combobulate-elixir--child-of-type node "arguments")))
             (and arguments (combobulate-elixir--do-keyword-p arguments))))))

(defun combobulate-elixir--sole-expression (block)
  "Return the text of the only expression in BLOCK, or signal a `user-error'."
  (let ((children (seq-remove (lambda (child) (equal (treesit-node-type child) "else_block"))
                              (treesit-node-children block t))))
    (unless (and (= (length children) 1)
                 (not (member (treesit-node-type (car children)) '("comment" "stab_clause"))))
      (user-error "Only a block with a single expression fits in a keyword"))
    (treesit-node-text (car children) t)))

(defun combobulate-elixir--keyword-form (call)
  "Return the text of CALL, which has a `do' block, written with `do:'."
  (let ((do-block (combobulate-elixir--do-block call)))
    (when (seq-some (lambda (child)
                      (member (treesit-node-type child) '("rescue_block" "catch_block" "after_block")))
                    (treesit-node-children do-block t))
      (user-error "Only `do' and `else' blocks fit in a keyword"))
    (let* ((else-block (combobulate-elixir--child-of-type do-block "else_block"))
           (arguments (combobulate-elixir--child-of-type call "arguments"))
           (keywords (concat "do: " (combobulate-elixir--sole-expression do-block)
                             (and else-block
                                  (concat ", else: " (combobulate-elixir--sole-expression else-block)))))
           (start (treesit-node-start call)))
      (cond
       ((and arguments (equal (treesit-node-type (treesit-node-child arguments 0)) "("))
        (concat (buffer-substring-no-properties start (treesit-node-start (car (last (treesit-node-children arguments)))))
                (and (treesit-node-child arguments 0 t) ", ")
                keywords ")"))
       (arguments (concat (buffer-substring-no-properties start (treesit-node-end arguments)) ", " keywords))
       (t (concat (buffer-substring-no-properties start (treesit-node-end (treesit-node-child-by-field-name call "target")))
                  " " keywords))))))

(defun combobulate-elixir--block-form (call)
  "Return the text of CALL, which has a `do:' keyword, written with a `do' block."
  (let* ((arguments (combobulate-elixir--child-of-type call "arguments"))
         (keywords (combobulate-elixir--child-of-type arguments "keywords"))
         (pairs (treesit-node-children keywords t))
         (key (lambda (pair) (string-trim (treesit-node-text (treesit-node-child-by-field-name pair "key") t))))
         (value (lambda (name)
                  (when-let* ((pair (seq-find (lambda (pair) (equal (funcall key pair) name)) pairs)))
                    (treesit-node-text (treesit-node-child-by-field-name pair "value") t))))
         (positional (seq-remove (lambda (child) (treesit-node-eq child keywords))
                                 (treesit-node-children arguments t)))
         (else (funcall value "else:")))
    (unless (seq-every-p (lambda (pair) (member (funcall key pair) '("do:" "else:"))) pairs)
      (user-error "Only `do:' and `else:' keywords can become blocks"))
    (concat (buffer-substring-no-properties
             (treesit-node-start call)
             (treesit-node-end (or (car (last positional)) (treesit-node-child-by-field-name call "target"))))
            (and positional (equal (treesit-node-type (treesit-node-child arguments 0)) "(") ")")
            " do\n" (funcall value "do:") "\n"
            (and else (concat "else\n" else "\n"))
            "end")))

(defun combobulate-elixir-toggle-do-block ()
  "Switch the call at point between `do: ...' and `do ... end'.

An `else' block or `else:' keyword comes along.  A block becomes a
keyword only if each of its blocks holds a single expression."
  (interactive)
  (let* ((call (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         #'combobulate-elixir--block-call-p t)
                   (user-error "No call with a `do' block or `do:' keyword at point")))
         (text (if (combobulate-elixir--do-block call)
                   (combobulate-elixir--keyword-form call)
                 (combobulate-elixir--block-form call)))
         (start (treesit-node-start call)))
    (delete-region start (treesit-node-end call))
    (goto-char start)
    (insert text)
    (indent-region start (point))
    (goto-char start)))

(defun combobulate-elixir--call-arguments (call)
  "Return the arguments CALL passes in parentheses, unless it has a `do' block."
  (let ((arguments (combobulate-elixir--child-of-type call "arguments")))
    (and arguments
         (not (combobulate-elixir--do-block call))
         (equal (treesit-node-type (treesit-node-child arguments 0)) "(")
         (seq-remove (lambda (child) (equal (treesit-node-type child) "comment"))
                     (treesit-node-children arguments t)))))

(defun combobulate-elixir--pipeable-p (node)
  (and (equal (treesit-node-type node) "call")
       (let ((first (car (combobulate-elixir--call-arguments node))))
         (and first (not (equal (treesit-node-type first) "keywords"))))))

(defun combobulate-elixir--pipe-head-text (node)
  "Return the text of NODE as a pipeline head, with parentheses if needed."
  (let ((text (treesit-node-text node t)))
    (if (or (and (equal (treesit-node-type node) "binary_operator")
                 (not (combobulate-elixir--pipe-p node)))
            (and (equal (treesit-node-type node) "unary_operator")
                 (not (equal (treesit-node-text (treesit-node-child-by-field-name node "operator") t) "@"))))
        (concat "(" text ")")
      text)))

(defun combobulate-elixir--stage-text (call)
  "Return the text of CALL without its first argument, as a pipeline stage."
  (let ((rest (cdr (combobulate-elixir--call-arguments call))))
    (concat (treesit-node-text (treesit-node-child-by-field-name call "target") t) "("
            (and rest (buffer-substring-no-properties (treesit-node-start (car rest))
                                                      (treesit-node-end (car (last rest)))))
            ")")))

(defun combobulate-elixir--piped (call)
  "Return the text of CALL with its first argument piped into it."
  (concat (combobulate-elixir--pipe-head-text (car (combobulate-elixir--call-arguments call)))
          " |> " (combobulate-elixir--stage-text call)))

(defun combobulate-elixir--with-first-argument (stage text)
  "Return the text of the pipeline STAGE called with TEXT as its first argument."
  (let ((arguments (combobulate-elixir--child-of-type stage "arguments"))
        (rest (combobulate-elixir--call-arguments stage)))
    (cond
     ((equal (treesit-node-type stage) "identifier")
      (concat (treesit-node-text stage t) "(" text ")"))
     ((or (not (equal (treesit-node-type stage) "call"))
          (combobulate-elixir--do-block stage)
          (and arguments (not (equal (treesit-node-type (treesit-node-child arguments 0)) "("))))
      (user-error "Only a stage that calls with parentheses can take the piped value"))
     ((not arguments) (concat (treesit-node-text stage t) "(" text ")"))
     (t (concat (treesit-node-text (treesit-node-child-by-field-name stage "target") t) "(" text
                (and rest (concat ", " (buffer-substring-no-properties (treesit-node-start (car rest))
                                                                       (treesit-node-end (car (last rest))))))
                ")")))))

(defun combobulate-elixir--unpiped (pipe)
  "Return the text of PIPE with its left side as the first argument of its stage."
  (combobulate-elixir--with-first-argument (treesit-node-child-by-field-name pipe "right")
                                           (treesit-node-text (treesit-node-child-by-field-name pipe "left") t)))

(defun combobulate-elixir-toggle-pipe (&optional chain)
  "Switch the call at point between `foo(x, y)' and `x |> foo(y)'.

In a pipeline, the stage at point takes the value piped into it as
its first argument; on the pipeline's head, the first stage does.
Elsewhere, the innermost call with arguments pipes in its first one.
With a prefix argument CHAIN, run `combobulate-elixir-toggle-pipe-chain'."
  (interactive "P")
  (if chain
      (combobulate-elixir-toggle-pipe-chain)
    (let ((node (combobulate-elixir--node-at (point)))
          (edit))
      (while (and node (not edit))
        (let ((parent (treesit-node-parent node)))
          (cond
           ((and (combobulate-elixir--pipe-p parent)
                 (or (treesit-node-eq node (treesit-node-child-by-field-name parent "right"))
                     (not (combobulate-elixir--pipe-p node))))
            (setq edit (cons parent #'combobulate-elixir--unpiped)))
           ((combobulate-elixir--pipeable-p node)
            (setq edit (cons node #'combobulate-elixir--piped))))
          (setq node parent)))
      (unless edit
        (user-error "No call with arguments or pipeline at point"))
      (combobulate-elixir--replace-with (car edit) (funcall (cdr edit) (car edit))))))

(defun combobulate-elixir--replace-with (node text)
  "Replace NODE with TEXT, indent it and move to its start."
  (let ((start (treesit-node-start node)))
    (delete-region start (treesit-node-end node))
    (goto-char start)
    (insert text)
    (indent-region start (point))
    (goto-char start)))

(defun combobulate-elixir--pipe-chain (call)
  "Return the text of CALL as one pipeline.

The pipeline goes through the calls nested in CALL as first arguments."
  (let ((node call)
        (stages))
    (while (combobulate-elixir--pipeable-p node)
      (push (combobulate-elixir--stage-text node) stages)
      (setq node (car (combobulate-elixir--call-arguments node))))
    (string-join (cons (combobulate-elixir--pipe-head-text node) stages) " |> ")))

(defun combobulate-elixir--unpipe-chain (pipe)
  "Return the text of the pipeline PIPE with every stage called on the one before."
  (pcase-let* ((`(,head . ,stages) (combobulate-elixir--pipe-stages pipe))
               (inner (and (equal (treesit-node-type head) "block")
                           (seq-remove (lambda (child) (equal (treesit-node-type child) "comment"))
                                       (treesit-node-children head t))))
               (text (treesit-node-text (if (= (length inner) 1) (car inner) head) t)))
    (dolist (stage stages)
      (setq text (combobulate-elixir--with-first-argument stage text)))
    text))

(defun combobulate-elixir--nest-top (call)
  "Return the outermost call that CALL is nested in through first arguments.

A call that is itself a pipeline stage is not climbed into, because
its first argument is the piped value."
  (let ((node call)
        (done))
    (while (not done)
      (let* ((arguments (treesit-node-parent node))
             (owner (treesit-node-parent arguments))
             (pipe (treesit-node-parent owner)))
        (if (and (equal (treesit-node-type arguments) "arguments")
                 (combobulate-elixir--pipeable-p owner)
                 (treesit-node-eq node (car (combobulate-elixir--call-arguments owner)))
                 (not (and (combobulate-elixir--pipe-p pipe)
                           (treesit-node-eq owner (treesit-node-child-by-field-name pipe "right")))))
            (setq node owner)
          (setq done t))))
    node))

(defun combobulate-elixir-toggle-pipe-chain ()
  "Switch the calls at point between `c(b(a(x)))' and `x |> a() |> b() |> c()'.

In a pipeline, every stage takes the value piped into it as its
first argument.  Elsewhere, the calls nested through their first
argument around point become one pipeline."
  (interactive)
  (let ((node (combobulate-elixir--node-at (point)))
        (edit))
    (while (and node (not edit))
      (let ((parent (treesit-node-parent node)))
        (cond
         ((combobulate-elixir--pipe-p parent)
          (while (combobulate-elixir--pipe-p (treesit-node-parent parent))
            (setq parent (treesit-node-parent parent)))
          (setq edit (cons parent #'combobulate-elixir--unpipe-chain)))
         ((combobulate-elixir--pipeable-p node)
          (setq edit (cons (combobulate-elixir--nest-top node) #'combobulate-elixir--pipe-chain))))
        (setq node parent)))
    (unless edit
      (user-error "No call with arguments or pipeline at point"))
    (combobulate-elixir--replace-with (car edit) (funcall (cdr edit) (car edit)))))

(defun combobulate-elixir--alias-parts (node)
  "Return (PREFIX NAMES MULTI) if NODE aliases modules and has no options.

For `alias A.B.C' this is (\"A.B\" (\"C\") nil), and for
`alias A.B.{C, D}' it is (\"A.B\" (\"C\" \"D\") t)."
  (when-let* (((equal (treesit-node-type node) "call"))
              ((equal (treesit-node-text (treesit-node-child-by-field-name node "target") t) "alias"))
              (arguments (treesit-node-children (combobulate-elixir--child-of-type node "arguments") t))
              ((= (length arguments) 1))
              (module (car arguments)))
    (pcase (treesit-node-type module)
      ("alias"
       (let ((name (treesit-node-text module t)))
         (when (string-match (rx bos (group (+ anychar)) "." (group (+ (not (any ".")))) eos) name)
           (list (match-string 1 name) (list (match-string 2 name)) nil))))
      ("dot"
       (let* ((tuple (treesit-node-child-by-field-name module "right"))
              (elements (treesit-node-children tuple t)))
         (when (and (equal (treesit-node-type tuple) "tuple")
                    (not (seq-some (lambda (element) (equal (treesit-node-type element) "comment"))
                                   elements)))
           (list (treesit-node-text (treesit-node-child-by-field-name module "left") t)
                 (mapcar (lambda (element) (treesit-node-text element t)) elements)
                 t)))))))

(defun combobulate-elixir--statement-range (node)
  "Return the range of NODE, with its whole line if nothing else is on it."
  (save-excursion
    (let ((start (treesit-node-start node))
          (end (treesit-node-end node)))
      (goto-char start)
      (when (and (looking-back "^[ \t]*" (line-beginning-position))
                 (progn (goto-char end) (looking-at-p "[ \t]*$")))
        (setq start (progn (goto-char start) (line-beginning-position))
              end (progn (goto-char end) (min (point-max) (1+ (line-end-position))))))
      (cons start end))))

(defun combobulate-elixir-toggle-multi-alias ()
  "Split `alias A.{B, C}' into one alias per module, or merge into that form.

On a single alias, the aliases around it that share its prefix merge
into the first of them.  Only the unbroken run of `alias' statements
around point is searched."
  (interactive)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         #'combobulate-elixir--alias-parts t)
                   (user-error "No alias with a module prefix at point")))
         (start (treesit-node-start node)))
    (pcase-let ((`(,prefix ,names ,multi) (combobulate-elixir--alias-parts node)))
      (if multi
          (let ((indentation (make-string (save-excursion (goto-char start) (current-column)) ?\s)))
            (delete-region start (treesit-node-end node))
            (goto-char start)
            (insert (mapconcat (lambda (name) (concat "alias " prefix "." name)) names
                               (concat "\n" indentation))))
        (let* ((siblings (seq-remove (lambda (sibling) (equal (treesit-node-type sibling) "comment"))
                                     (treesit-node-children (treesit-node-parent node) t)))
               (index (seq-position siblings node #'treesit-node-eq))
               (first index)
               (last index))
          (while (and (> first 0) (combobulate-elixir--alias-parts (nth (1- first) siblings)))
            (setq first (1- first)))
          (while (and (< (1+ last) (length siblings))
                      (combobulate-elixir--alias-parts (nth (1+ last) siblings)))
            (setq last (1+ last)))
          (let* ((same (seq-filter (lambda (sibling)
                                     (equal (car (combobulate-elixir--alias-parts sibling)) prefix))
                                   (seq-subseq siblings first (1+ last))))
                 (merged (concat "alias " prefix ".{"
                                 (mapconcat (lambda (sibling)
                                              (string-join (cadr (combobulate-elixir--alias-parts sibling)) ", "))
                                            same ", ")
                                 "}"))
                 (first-range (cons (treesit-node-start (car same)) (treesit-node-end (car same))))
                 (other-ranges (mapcar #'combobulate-elixir--statement-range (cdr same))))
            (when (< (length same) 2)
              (user-error "No other alias of %s next to this one" prefix))
            (dolist (range (reverse other-ranges))
              (delete-region (car range) (cdr range)))
            (delete-region (car first-range) (cdr first-range))
            (goto-char (car first-range))
            (insert merged)
            (setq start (car first-range)))))
      (goto-char start))))

(defun combobulate-elixir--collection-p (node)
  (or (combobulate-elixir--type-p node '("list" "tuple" "map" "bitstring"))
      (and (equal (treesit-node-type node) "arguments")
           (equal (treesit-node-type (treesit-node-child node 0)) "("))))

(defun combobulate-elixir-split-or-join ()
  "Put each element of the collection at point on its own line, or all on one.

Lists, tuples, maps, structs, bitstrings and parenthesized arguments
are collections.  A collection whose first element starts on the line
after its opening delimiter is joined; any other is split."
  (interactive)
  (let* ((collection (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                               #'combobulate-elixir--collection-p t)
                         (user-error "No collection at point")))
         (container (if (equal (treesit-node-type collection) "map")
                        (combobulate-elixir--child-of-type collection "map_content")
                      collection))
         (elements (and container (combobulate-elixir--elements container)))
         (start (treesit-node-start collection)))
    (unless elements
      (user-error "No elements to split or join"))
    (when (seq-some (lambda (node) (and node (combobulate-elixir--child-of-type node "comment")))
                    (list container (combobulate-elixir--child-of-type container "keywords")))
      (user-error "Splitting or joining would lose the comments in this collection"))
    (let* ((open (string-trim-right (buffer-substring-no-properties start (treesit-node-start (car elements)))))
           (close (treesit-node-text (car (last (treesit-node-children collection))) t))
           (texts (mapcar (lambda (element) (treesit-node-text element t)) elements))
           (text (if (string-search "\n" (buffer-substring-no-properties start (treesit-node-start (car elements))))
                     (concat open (string-join texts ", ") close)
                   (concat open "\n" (string-join texts ",\n") "\n" close))))
      (delete-region start (treesit-node-end collection))
      (goto-char start)
      (insert text)
      (indent-region start (point))
      (goto-char start))))

(defun combobulate-elixir-toggle-keyword-map ()
  "Switch the collection at point between `[a: 1]' and `%{a: 1}'.

Only the delimiters change, so the layout and comments stay.  A map
becomes a keyword list only if all its keys use the `a: 1' form."
  (interactive)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         (lambda (node) (combobulate-elixir--type-p node '("list" "map")))
                                         t)
                   (user-error "No keyword list or map at point")))
         (map (equal (treesit-node-type node) "map"))
         (container (if map (combobulate-elixir--child-of-type node "map_content") node))
         (elements (and container (combobulate-elixir--elements container)))
         (start (treesit-node-start node)))
    (unless (and (seq-some (lambda (element) (equal (treesit-node-type element) "pair")) elements)
                 (not (combobulate-elixir--child-of-type node "struct"))
                 (seq-every-p (lambda (element) (member (treesit-node-type element) '("pair" "comment")))
                              elements))
      (user-error "Only a keyword list or a map with `a: 1' keys can switch"))
    (goto-char (treesit-node-end node))
    (delete-char -1)
    (insert (if map "]" "}"))
    (goto-char start)
    (delete-char (if map 2 1))
    (insert (if map "[" "%{"))
    (goto-char start)))

(defun combobulate-elixir--map-key (element)
  "Return (KIND NAME START VALUE-START) for the key of the map ELEMENT.

KIND is `atom' or `string', and NAME is the key as written between
its quotes.  START is where the key starts and VALUE-START where the
value starts.  Return nil for any other key, such as a variable or
an interpolated string."
  (let* ((pair (equal (treesit-node-type element) "pair"))
         (key (if pair
                  (treesit-node-child-by-field-name element "key")
                (and (equal (treesit-node-type element) "binary_operator")
                     (equal (treesit-node-text (treesit-node-child-by-field-name element "operator") t) "=>")
                     (treesit-node-child-by-field-name element "left"))))
         (kind (pcase (treesit-node-type key)
                 ((or "keyword" "quoted_keyword" "atom" "quoted_atom") 'atom)
                 ("string" 'string)))
         (bare (and key (string-remove-suffix ":" (string-remove-prefix ":" (string-trim (treesit-node-text key t))))))
         (name (cond
                ((or (not kind) (combobulate-elixir--child-of-type key "interpolation")) nil)
                ((member (treesit-node-type key) '("keyword" "atom")) bare)
                ((string-match (rx bos "\"" (group (* anything)) "\"" eos) bare) (match-string 1 bare)))))
    (when name
      (list kind name (treesit-node-start key)
            (treesit-node-start (treesit-node-child-by-field-name element (if pair "value" "right")))))))

(defun combobulate-elixir-toggle-map-keys ()
  "Switch the keys of the map at point between `a: 1' and `\"a\" => 1'.

Only the keys change, so the layout, comments and values stay.  A
string that is not a plain atom name becomes a quoted key, such as
`\"a-b\": 1'."
  (interactive)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         (lambda (node) (equal (treesit-node-type node) "map"))
                                         t)
                   (user-error "No map at point")))
         (content (combobulate-elixir--child-of-type node "map_content"))
         (keys (mapcar #'combobulate-elixir--map-key
                       (seq-remove (lambda (element) (equal (treesit-node-type element) "comment"))
                                   (and content (combobulate-elixir--elements content)))))
         (start (treesit-node-start node)))
    (when (or (null keys)
              (memq nil keys)
              (combobulate-elixir--child-of-type node "struct")
              (cdr (seq-uniq (mapcar #'car keys))))
      (user-error "Only a map whose keys are all atoms or all strings can switch"))
    (dolist (key (reverse keys))
      (pcase-let ((`(,kind ,name ,key-start ,value-start) key))
        (delete-region key-start value-start)
        (goto-char key-start)
        (insert (cond
                 ((eq kind 'atom) (format "\"%s\" => " name))
                 ((string-match-p (rx bos (any "a-zA-Z_") (* (any "a-zA-Z0-9_@")) (? (any "?!")) eos) name)
                  (format "%s: " name))
                 (t (format "\"%s\": " name))))))
    (goto-char start)))

(defun combobulate-elixir--escaped-quotes (node start end)
  "Return the text of the string NODE from START to END, quotes escaped.

Only plain quotes are escaped, so escape sequences and interpolations
stay as they are."
  (let ((pos start)
        (parts nil))
    (dolist (child (treesit-node-children node t))
      (let ((child-start (max start (treesit-node-start child)))
            (child-end (min end (treesit-node-end child))))
        (when (and (equal (treesit-node-type child) "quoted_content") (< child-start child-end))
          (push (buffer-substring pos child-start) parts)
          (push (string-replace "\"" "\\\"" (buffer-substring child-start child-end)) parts)
          (setq pos child-end))))
    (push (buffer-substring pos end) parts)
    (apply #'concat (nreverse parts))))

(defun combobulate-elixir-toggle-heredoc ()
  "Switch the string or sigil at point between inline and heredoc.

A heredoc ends its content with a newline, so the switch adds or
drops one.  Only a heredoc with one line of content can become
inline.  A string then escapes its quotes, and a sigil takes the
first delimiter that its content lacks."
  (interactive)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         (lambda (node) (combobulate-elixir--type-p node '("string" "sigil")))
                                         t)
                   (user-error "No string or sigil at point")))
         (sigil (equal (treesit-node-type node) "sigil"))
         (start (treesit-node-start node))
         (open (if sigil (treesit-node-end (combobulate-elixir--child-of-type node "sigil_name")) start))
         (modifiers (combobulate-elixir--child-of-type node "sigil_modifiers"))
         (end (if modifiers (treesit-node-start modifiers) (treesit-node-end node)))
         (heredoc (car (member (buffer-substring open (min end (+ open 3))) '("\"\"\"" "'''")))))
    (if heredoc
        (let* ((indent (save-excursion (goto-char (- end 3)) (current-column)))
               (text (if sigil
                         (buffer-substring (+ open 3) (- end 3))
                       (combobulate-elixir--escaped-quotes node (+ open 3) (- end 3))))
               (lines (butlast (cdr (split-string text "\n"))))
               (content (and (= (length lines) 1)
                             (replace-regexp-in-string (format "\\`[ \t]\\{0,%d\\}" indent) "" (car lines))))
               (pairs '("\"\"" "''" "()" "[]" "{}" "<>" "//" "||"))
               (pair (and content
                          (if sigil
                              (seq-find (lambda (pair) (not (seq-intersection pair content)))
                                        (if (equal heredoc "'''") (cons "''" pairs) pairs))
                            "\"\""))))
          (unless pair
            (user-error "Only a heredoc with one line of content can become inline"))
          (delete-region open end)
          (goto-char open)
          (insert (substring pair 0 1) content (substring pair 1)))
      (let* ((delimiter (if (eq (char-after open) ?') "'''" "\"\"\""))
             (close (char-before end))
             (raw (buffer-substring (1+ open) (1- end)))
             ;; An escaped quote keeps its backslash in an uppercase sigil, any other escaped delimiter does not.
             (content (if (memq close '(?\" ?')) raw (string-replace (string ?\\ close) (string close) raw)))
             (padding (make-string (save-excursion (goto-char start) (current-indentation)) ?\s)))
        (when (string-search delimiter content)
          (user-error "The content already holds %s" delimiter))
        (delete-region open end)
        (goto-char open)
        (insert delimiter "\n" (replace-regexp-in-string "^" padding content) "\n" padding delimiter)))
    (goto-char start)))

(defun combobulate-elixir--capture-operator-p (node)
  (and (equal (treesit-node-type node) "unary_operator")
       (equal (treesit-node-text (treesit-node-child-by-field-name node "operator") t) "&")))

(defun combobulate-elixir--operand (node)
  "Return the operand of the unary operator NODE.

In `&(x)' the grammar also puts the `operand' field on the parentheses."
  (seq-find (lambda (child) (equal (treesit-node-field-name child) "operand"))
            (treesit-node-children node t)))

(defun combobulate-elixir--capture-argument-p (node)
  "Return non-nil if NODE is a capture argument such as `&1'."
  (and (combobulate-elixir--capture-operator-p node)
       (equal (treesit-node-type (combobulate-elixir--operand node)) "integer")))

(defun combobulate-elixir--capture-p (node)
  "Return non-nil if NODE is a capture such as `&foo/1' rather than `&1'."
  (and (combobulate-elixir--capture-operator-p node)
       (not (combobulate-elixir--capture-argument-p node))))

(defun combobulate-elixir--descendants (node pred)
  "Return NODE and the nodes inside it that satisfy PRED, in buffer order."
  (flatten-tree (treesit-induce-sparse-tree node pred)))

(defun combobulate-elixir--text-replacing (node replacements)
  "Return the text of NODE with REPLACEMENTS, a list of (DESCENDANT . TEXT)."
  (let ((start (treesit-node-start node))
        (text (treesit-node-text node t)))
    (dolist (replacement (sort replacements (lambda (a b) (> (treesit-node-start (car a))
                                                             (treesit-node-start (car b))))))
      (setq text (concat (substring text 0 (- (treesit-node-start (car replacement)) start))
                         (cdr replacement)
                         (substring text (- (treesit-node-end (car replacement)) start)))))
    text))

(defun combobulate-elixir--parenthesized-call-p (node)
  (and (equal (treesit-node-type node) "call")
       (not (combobulate-elixir--do-block node))
       (equal (treesit-node-type (treesit-node-child (combobulate-elixir--child-of-type node "arguments") 0)) "(")))

(defun combobulate-elixir--captured (fn)
  "Return the text of the anonymous function FN written as a capture."
  (let ((clauses (treesit-node-children fn t)))
    (unless (= (length clauses) 1)
      (user-error "Only a function with one clause can become a capture"))
    (let* ((head (treesit-node-child-by-field-name (car clauses) "left"))
           (params (and head (treesit-node-children head t)))
           (names (mapcar (lambda (param) (treesit-node-text param t)) params))
           (expressions (treesit-node-children (treesit-node-child-by-field-name (car clauses) "right") t))
           (expression (car expressions)))
      (when (and head (not (equal (treesit-node-type head) "arguments")))
        (user-error "A function with a guard cannot become a capture"))
      (unless (seq-every-p (lambda (param)
                             (and (equal (treesit-node-type param) "identifier")
                                  (not (string-prefix-p "_" (treesit-node-text param t)))))
                           params)
        (user-error "Only plain parameters can become capture arguments"))
      (unless (= (length expressions) 1)
        (user-error "Only a function with a single expression can become a capture"))
      (when (combobulate-elixir--descendants expression
                                             (lambda (node)
                                               (or (equal (treesit-node-type node) "anonymous_function")
                                                   (combobulate-elixir--capture-operator-p node))))
        (user-error "A function holding another function or capture cannot become a capture"))
      (let* ((target (treesit-node-child-by-field-name expression "target"))
             (uses (combobulate-elixir--descendants expression
                                                    (lambda (node)
                                                      (and (equal (treesit-node-type node) "identifier")
                                                           (member (treesit-node-text node t) names))))))
        (cond
         ((and (combobulate-elixir--parenthesized-call-p expression)
               (equal (mapcar (lambda (argument) (treesit-node-text argument t))
                              (combobulate-elixir--call-arguments expression))
                      names)
               (not (and (equal (treesit-node-type target) "dot")
                         (null (treesit-node-child-by-field-name target "right")))))
          (format "&%s/%d" (treesit-node-text target t) (length names)))
         ((or (null names)
              (seq-some (lambda (name) (not (seq-find (lambda (use) (equal (treesit-node-text use t) name)) uses)))
                        names))
          (user-error "A capture must use every parameter"))
         (t
          (let ((captured (combobulate-elixir--text-replacing
                           expression
                           (mapcar (lambda (use)
                                     (cons use (format "&%d" (1+ (seq-position names (treesit-node-text use t))))))
                                   uses))))
            (if (combobulate-elixir--parenthesized-call-p expression)
                (concat "&" captured)
              (concat "&(" captured ")")))))))))

(defun combobulate-elixir--uncaptured (capture)
  "Return the text of CAPTURE written as an anonymous function."
  (let ((operand (combobulate-elixir--operand capture)))
    (if (and (equal (treesit-node-type operand) "binary_operator")
             (equal (treesit-node-text (treesit-node-child-by-field-name operand "operator") t) "/")
             (equal (treesit-node-type (treesit-node-child-by-field-name operand "right")) "integer"))
        (let* ((arity (string-to-number (treesit-node-text (treesit-node-child-by-field-name operand "right") t)))
               (names (mapcar (lambda (i) (format "arg%d" i)) (number-sequence 1 arity))))
          (concat "fn" (and names (concat " " (string-join names ", "))) " -> "
                  (treesit-node-text (treesit-node-child-by-field-name operand "left") t)
                  "(" (string-join names ", ") ") end"))
      (let* ((arguments (combobulate-elixir--descendants operand #'combobulate-elixir--capture-argument-p))
             (index (lambda (argument)
                      (string-to-number (treesit-node-text (combobulate-elixir--operand argument) t))))
             (arity (apply #'max 0 (mapcar index arguments)))
             (names (mapcar (lambda (i) (format "arg%d" i)) (number-sequence 1 arity))))
        (concat "fn " (string-join names ", ") " -> "
                (combobulate-elixir--text-replacing
                 operand
                 (mapcar (lambda (argument) (cons argument (nth (1- (funcall index argument)) names)))
                         arguments))
                " end")))))

(defun combobulate-elixir-toggle-capture ()
  "Switch between `fn x -> foo(x) end' and `&foo/1', or `&foo(&1, y)'.

An anonymous function becomes a capture only if it has one clause
without a guard, plain parameters that its single expression all uses,
and no function or capture inside.  A capture's arguments are named
`arg1', `arg2' and so on."
  (interactive)
  (let* ((node (or (treesit-parent-until (combobulate-elixir--node-at (point))
                                         (lambda (node)
                                           (or (equal (treesit-node-type node) "anonymous_function")
                                               (combobulate-elixir--capture-p node)))
                                         t)
                   (user-error "No anonymous function or capture at point")))
         (text (if (equal (treesit-node-type node) "anonymous_function")
                   (combobulate-elixir--captured node)
                 (combobulate-elixir--uncaptured node)))
         (start (treesit-node-start node)))
    (delete-region start (treesit-node-end node))
    (goto-char start)
    (insert text)
    (goto-char start)))

(defun combobulate-elixir--operator (node)
  "Return the operator of NODE if it is a binary operator."
  (and (equal (treesit-node-type node) "binary_operator")
       (treesit-node-text (treesit-node-child-by-field-name node "operator") t)))

(defun combobulate-elixir--variables (node)
  "Return the identifiers in NODE that name variables, in buffer order.

Call targets, fields such as `b' in `a.b' and attributes such as
`@a' are left out."
  (combobulate-elixir--descendants
   node (lambda (id)
          (let ((parent (treesit-node-parent id)))
            (and (equal (treesit-node-type id) "identifier")
                 (not (pcase (treesit-node-type parent)
                        ("call" (treesit-node-eq id (treesit-node-child-by-field-name parent "target")))
                        ("dot" (treesit-node-eq id (treesit-node-child-by-field-name parent "right")))
                        ("unary_operator"
                         (equal (treesit-node-text (treesit-node-child-by-field-name parent "operator") t)
                                "@")))))))))

(defun combobulate-elixir--pattern-p (node stop)
  "Return non-nil if NODE is in a pattern or a head below STOP.

Patterns are the left sides of `=' and `<-', and heads are clause
heads and function heads."
  (let ((found))
    (while (and (not found) node (not (treesit-node-eq node stop)))
      (let ((parent (treesit-node-parent node)))
        (setq found (or (and (member (combobulate-elixir--operator parent) '("=" "<-"))
                             (treesit-node-eq node (treesit-node-child-by-field-name parent "left")))
                        (and (equal (treesit-node-type parent) "stab_clause")
                             (equal (treesit-node-field-name node) "left"))
                        (and (equal (treesit-node-type parent) "arguments")
                             (combobulate-elixir--signature (treesit-node-parent parent))))
              node parent)))
    found))

(defun combobulate-elixir--pinned-p (id)
  "Return non-nil if the variable ID is pinned, as in `^x'."
  (equal (treesit-node-text (treesit-node-child-by-field-name (treesit-node-parent id) "operator") t) "^"))

(defun combobulate-elixir--bound-variables (node)
  "Return the variables that the patterns and heads in NODE bind.

Pinned variables such as `^x' are left out, because they are read."
  (seq-filter (lambda (id)
                (and (combobulate-elixir--pattern-p id node)
                     (not (combobulate-elixir--pinned-p id))))
              (combobulate-elixir--variables node)))

(defconst combobulate-elixir--statement-blocks
  '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block" "body" "block")
  "Node types whose children are statements.")

(defun combobulate-elixir--statement (node)
  "Return the statement NODE belongs to, which may be NODE itself."
  (treesit-parent-until node (lambda (node)
                               (combobulate-elixir--type-p (treesit-node-parent node)
                                                           combobulate-elixir--statement-blocks))
                        t))

(defun combobulate-elixir--expression-at-point ()
  "Return the expression the region covers, or the one at point."
  (if (use-region-p)
      (let* ((start (save-excursion (goto-char (region-beginning)) (skip-chars-forward " \t\n") (point)))
             (end (save-excursion (goto-char (region-end)) (skip-chars-backward " \t\n") (point)))
             (node (treesit-node-descendant-for-range (combobulate-buffer-root-node 'elixir) start end t)))
        (or (and node (= (treesit-node-start node) start) (= (treesit-node-end node) end) node)
            (user-error "The region does not cover a single expression")))
    (combobulate-elixir--skip-indentation)
    (combobulate-elixir--thing-at (point))))

(defun combobulate-elixir--extract-problem (expression statement)
  "Return why binding EXPRESSION above STATEMENT would change the code, or nil."
  (let ((node expression)
        (problem))
    (while (and (not problem) (not (treesit-node-eq node statement)))
      (let ((parent (treesit-node-parent node)))
        (setq problem
              (cond
               ((or (and (member (combobulate-elixir--operator parent) '("&&" "||" "and" "or"))
                         (treesit-node-eq node (treesit-node-child-by-field-name parent "right")))
                    (and (equal (treesit-node-type parent) "pair")
                         (member (string-trim (treesit-node-text (treesit-node-child-by-field-name parent "key") t))
                                 '("do:" "else:" "after:" "rescue:" "catch:"))))
                "The expression only runs sometimes, so it cannot run before its statement")
               ((and (treesit-node-eq node expression)
                     (combobulate-elixir--pipe-p parent)
                     (treesit-node-eq node (treesit-node-child-by-field-name parent "right")))
                "A pipeline stage cannot leave its pipeline"))
              node parent)))
    (or problem
        (and (combobulate-elixir--pattern-p expression statement)
             "A pattern or a head cannot hold a variable")
        (let* ((start (treesit-node-start expression))
               (bound (seq-keep (lambda (id) (and (<= (treesit-node-end id) start) (treesit-node-text id t)))
                                (combobulate-elixir--bound-variables statement)))
               (used (seq-find (lambda (id) (member (treesit-node-text id t) bound))
                               (combobulate-elixir--variables expression))))
          (and used
               (let ((name (treesit-node-text used t)))
                 (format "`%s' is bound earlier in the same statement" name)))))))

(defun combobulate-elixir-extract-variable (name)
  "Bind the expression at point to NAME, and use NAME in its place.

With an active region, extract the expression it covers.  The
binding goes above the statement in the innermost block around the
expression, and that statement must start its own line.  Refuse
when the move would change what the code does: in a pattern or a
head, in a branch that only runs sometimes, or when NAME is already
a variable further on."
  (interactive (list (read-string "Variable name: ")))
  (unless (let ((case-fold-search nil))
            (string-match-p (rx bos (any "a-z_") (* (any "a-zA-Z0-9_")) eos) name))
    (user-error "`%s' is not a variable name" name))
  (let* ((expression (or (combobulate-elixir--expression-at-point) (user-error "No expression at point")))
         (statement (or (combobulate-elixir--statement expression)
                        (user-error "No statement around the expression")))
         (problem (combobulate-elixir--extract-problem expression statement))
         (start (treesit-node-start expression))
         (end (treesit-node-end expression))
         (statement-start (treesit-node-start statement))
         (text (treesit-node-text expression t)))
    (when (combobulate-elixir--type-p expression (append combobulate-elixir--wrappers
                                                         '("keyword" "quoted_keyword" "pair" "stab_clause" "comment")))
      (user-error "Only an expression can become a variable"))
    (when problem
      (user-error "%s" problem))
    (unless (save-excursion (goto-char statement-start) (looking-back "^[ \t]*" (line-beginning-position)))
      (user-error "Only a statement on its own line can have a binding put above it"))
    (when (seq-find (lambda (id)
                      (and (equal (treesit-node-text id t) name)
                           (>= (treesit-node-start id) statement-start)
                           (not (and (>= (treesit-node-start id) start) (<= (treesit-node-end id) end)))))
                    (combobulate-elixir--variables (treesit-node-parent statement)))
      (user-error "`%s' is already a variable further on" name))
    (deactivate-mark)
    (delete-region start end)
    (goto-char start)
    (insert name)
    (goto-char statement-start)
    (insert name " = " text "\n")
    (indent-region statement-start (line-end-position))
    (goto-char statement-start)))

(defun combobulate-elixir--binding-at (pos)
  "Return the statement `name = value' around POS."
  (treesit-parent-until (combobulate-elixir--node-at pos)
                        (lambda (node)
                          (and (equal (combobulate-elixir--operator node) "=")
                               (equal (treesit-node-type (treesit-node-child-by-field-name node "left"))
                                      "identifier")
                               (combobulate-elixir--type-p (treesit-node-parent node)
                                                           combobulate-elixir--statement-blocks)))
                        t))

(defun combobulate-elixir-inline-variable ()
  "Replace the use of the variable bound at point with its value.

The binding is removed and the value runs where the variable was
used.  Refuse unless the variable has exactly one use, outside any
`fn' or `for', it is not bound again further on, and the variables
its value reads are not bound again before that use."
  (interactive)
  (combobulate-elixir--skip-indentation)
  (let* ((binding (or (combobulate-elixir--binding-at (point))
                      (user-error "No `name = value' binding at point")))
         (name (treesit-node-text (treesit-node-child-by-field-name binding "left") t))
         (value (treesit-node-child-by-field-name binding "right"))
         (block (treesit-node-parent binding))
         (later (seq-filter (lambda (node) (> (treesit-node-start node) (treesit-node-start binding)))
                            (treesit-node-children block t)))
         (bound (mapcan #'combobulate-elixir--bound-variables later))
         (uses (seq-filter (lambda (id) (and (equal (treesit-node-text id t) name)
                                             (not (seq-find (lambda (other) (treesit-node-eq other id)) bound))))
                           (mapcan #'combobulate-elixir--variables later)))
         (use (car uses))
         (parent (treesit-node-parent use)))
    (when (seq-find (lambda (id) (equal (treesit-node-text id t) name)) bound)
      (user-error "`%s' is bound again further on" name))
    (unless uses
      (user-error "`%s' is not used after its binding" name))
    (when (cdr uses)
      (user-error "`%s' is used more than once, so its value would run more than once" name))
    (unless (treesit-node-eq block (treesit-parent-until use (lambda (node)
                                                               (or (treesit-node-eq node block)
                                                                   (equal (treesit-node-type node) "anonymous_function")
                                                                   (equal (combobulate-elixir--kind node) "for")))))
      (user-error "`%s' is used in a `fn' or `for', so its value would run more than once" name))
    (when (equal (treesit-node-text (treesit-node-child-by-field-name parent "operator") t) "^")
      (user-error "A pinned variable cannot take a value"))
    (when-let* ((reads (mapcar (lambda (id) (treesit-node-text id t)) (combobulate-elixir--variables value)))
                (rebound (seq-find (lambda (id) (and (<= (treesit-node-end id) (treesit-node-start use))
                                                     (member (treesit-node-text id t) reads)))
                                   bound)))
      (user-error "`%s' is bound again before the use" (treesit-node-text rebound t)))
    (let* ((text (treesit-node-text value t))
           (text (if (and (equal (treesit-node-type value) "binary_operator")
                          (combobulate-elixir--type-p parent '("binary_operator" "unary_operator" "dot" "access_call"))
                          (not (and (combobulate-elixir--pipe-p parent)
                                    (combobulate-elixir--pipe-p value)
                                    (treesit-node-eq use (treesit-node-child-by-field-name parent "left")))))
                     (concat "(" text ")")
                   text))
           (start (copy-marker (treesit-node-start use)))
           (binding-range (combobulate-elixir--statement-range binding)))
      (delete-region start (treesit-node-end use))
      (goto-char start)
      (insert text)
      (indent-region start (point))
      (delete-region (car binding-range) (cdr binding-range))
      (goto-char start)
      (set-marker start nil))))

(defun combobulate-elixir--extract-function-parts ()
  "Return (DEFINITION NODES STATEMENTS) for the code in the region.

NODES are the statements the region covers, or the one expression it
covers, and STATEMENTS is non-nil in the first case.  DEFINITION is
the function around them."
  (unless (use-region-p)
    (user-error "Mark the statements or the expression to extract"))
  (let* ((start (save-excursion (goto-char (region-beginning)) (skip-chars-forward " \t\n") (point)))
         (end (save-excursion (goto-char (region-end)) (skip-chars-backward " \t\n") (point)))
         (node (treesit-node-descendant-for-range (combobulate-buffer-root-node 'elixir) start end t))
         (block (treesit-parent-until node (lambda (node)
                                             (combobulate-elixir--type-p node combobulate-elixir--statement-blocks))
                                      t))
         (statements (and block (seq-filter (lambda (child)
                                              (and (>= (treesit-node-start child) start)
                                                   (<= (cdr (combobulate-elixir--trimmed-range child)) end)))
                                            (treesit-node-children block t))))
         (whole (and statements
                     (= (treesit-node-start (car statements)) start)
                     (= (cdr (combobulate-elixir--trimmed-range (car (last statements)))) end)))
         (nodes (cond
                 (whole statements)
                 ((and (= (treesit-node-start node) start) (= (treesit-node-end node) end)) (list node))
                 (t (user-error "The region must cover whole statements or one expression"))))
         (definition (or (treesit-parent-until (car nodes) #'combobulate-elixir--signature)
                         (user-error "Only code inside a function can move to a new function"))))
    (list definition nodes whole)))

(defun combobulate-elixir--extract-function-io (definition nodes)
  "Return (PARAMETERS OUTPUTS) for moving NODES out of DEFINITION.

PARAMETERS are the variables NODES read that are bound before them,
and OUTPUTS the variables NODES bind that the statements after them
read, each in the order they first appear."
  (let* ((start (treesit-node-start (car nodes)))
         (end (treesit-node-end (car (last nodes))))
         (name (lambda (id) (treesit-node-text id t)))
         (binds-p (lambda (id) (and (combobulate-elixir--pattern-p id definition)
                                    (not (combobulate-elixir--pinned-p id)))))
         (before (seq-keep (lambda (id) (and (<= (treesit-node-end id) start) (funcall name id)))
                           (combobulate-elixir--bound-variables definition)))
         (statement (combobulate-elixir--statement (car nodes)))
         (after (seq-filter (lambda (node) (>= (treesit-node-start node) end))
                            (treesit-node-children (treesit-node-parent statement) t)))
         (read-after (seq-keep (lambda (id) (and (not (funcall binds-p id)) (funcall name id)))
                               (mapcan #'combobulate-elixir--variables after)))
         (parameters)
         (bound))
    (dolist (id (mapcan #'combobulate-elixir--variables nodes))
      (let ((variable (funcall name id)))
        (if (funcall binds-p id)
            (unless (member variable bound)
              (push variable bound))
          (when (and (member variable before) (not (member variable bound)) (not (member variable parameters)))
            (push variable parameters)))))
    (list (nreverse parameters)
          (seq-filter (lambda (variable) (member variable read-after)) (nreverse bound)))))

(defun combobulate-elixir-extract-function (name &optional parameters)
  "Move the statements or the expression in the region to a new function NAME.

The new function is private and goes after the function the code
came from, and a call to it takes the code's place.  PARAMETERS is
the text of its parameter list.  It defaults to the variables the
code reads that are bound before it, which is a guess worth
checking, so interactively it is offered for editing.  Variables
the code binds and the statements after it read come back from the
function, in a tuple when there are several."
  (interactive
   (let ((name (read-string "Function name: ")))
     (pcase-let ((`(,definition ,nodes) (combobulate-elixir--extract-function-parts)))
       (list name (read-string "Parameters: "
                               (string-join (car (combobulate-elixir--extract-function-io definition nodes))
                                            ", "))))))
  (unless (let ((case-fold-search nil))
            (string-match-p (rx bos (any "a-z_") (* (any "a-zA-Z0-9_")) (? (any "?!")) eos) name))
    (user-error "`%s' is not a function name" name))
  (pcase-let* ((`(,definition ,nodes ,statements) (combobulate-elixir--extract-function-parts))
               (`(,computed ,outputs) (combobulate-elixir--extract-function-io definition nodes))
               (parameters (string-trim (or parameters (string-join computed ", "))))
               (expression (car nodes))
               (parent (treesit-node-parent expression))
               (start (treesit-node-start (car nodes)))
               (end (treesit-node-end (car (last nodes)))))
    (unless statements
      (cond
       ((combobulate-elixir--pattern-p expression definition)
        (user-error "A pattern or a head cannot call a function"))
       ((and (combobulate-elixir--pipe-p parent)
             (treesit-node-eq expression (treesit-node-child-by-field-name parent "right")))
        (user-error "A pipeline stage cannot leave its pipeline"))
       (outputs
        (user-error "The expression binds variables that the code after it reads"))))
    (when (seq-some (lambda (node)
                      (seq-find #'combobulate-elixir--capture-argument-p
                                (combobulate-elixir--descendants node #'combobulate-elixir--capture-argument-p)))
                    nodes)
      (user-error "Arguments such as `&1' cannot leave their capture"))
    (let* ((call (concat name "(" parameters ")"))
           (returned (and outputs (if (cdr outputs)
                                      (concat "{" (string-join outputs ", ") "}")
                                    (car outputs))))
           (group (combobulate-elixir--function-group-at (treesit-node-start definition)))
           (after (treesit-node-end (car (last group))))
           (indentation (make-string (save-excursion (goto-char (treesit-node-start definition)) (current-column))
                                     ?\s))
           (head (if (string-empty-p parameters) name call))
           (body (concat (buffer-substring-no-properties start end) (and returned (concat "\n" returned)))))
      (save-excursion
        (goto-char after)
        (insert "\n\n" indentation "defp " head " do\n" body "\n" indentation "end")
        (indent-region after (point)))
      (deactivate-mark)
      (delete-region start end)
      (goto-char start)
      (insert (if returned (concat returned " = " call) call))
      (goto-char start))))

(defun combobulate-elixir-pretty-print-node-name (node _default-name)
  "Pretty printer for Elixir nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("call"
       (let ((target (combobulate-node-child-by-field node "target"))
             (arguments (combobulate-node-child node 1)))
         (if (and arguments (equal (combobulate-node-type arguments) "arguments"))
             (concat (combobulate-node-text target) " "
                     (combobulate-node-text (combobulate-node-child arguments 0)))
           (combobulate-node-text target))))
      ("stab_clause"
       (concat (combobulate-node-text (combobulate-node-child-by-field node "left")) " ->"))
      (_ (car (split-string (combobulate-node-text node) "\n")))))
   40))

(eval-and-compile
  (defvar combobulate-elixir-definitions
    '((context-nodes
       '("identifier" "alias" "atom" "keyword"))
      (plausible-separators '("," "\n"))
      (pretty-print-node-name-function #'combobulate-elixir-pretty-print-node-name)
      (navigate-down-into-lists nil)
      (envelope-indent-region-function #'indent-region)
      (envelope-procedure-shorthand-alist
       '((expressions
          . ((:activation-nodes ((:nodes ((exclude (rule "arguments") "keywords")))))))
         (statements
          . ((:activation-nodes
              ((:nodes ((exclude (rule "arguments") "keywords"))
                       :has-parent ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                    "after_block" "body" "block"))))))))
      (envelope-list
       '((:description
          "dbg(...)"
          :key "d"
          :mark-node t
          :shorthand expressions
          :name "dbg"
          :template ("dbg(" r ")"))
         (:description
          "case ... do ... end"
          :key "c"
          :mark-node t
          :shorthand expressions
          :name "case"
          :template ("case " r " do" n> @ n "end" >))
         (:description
          "with {:ok, ...} <- ... do ... end"
          :key "w"
          :mark-node t
          :shorthand expressions
          :name "with"
          :template ("with {:ok, " (p result "Result") "} <- " r " do" n> (f result) @ n "end" >))
         (:description
          "{:ok, ...}"
          :key "o"
          :mark-node t
          :shorthand expressions
          :name "ok-tuple"
          :template ("{:ok, " r "}"))
         (:description
          "{:error, ...}"
          :key "e"
          :mark-node t
          :shorthand expressions
          :name "error-tuple"
          :template ("{:error, " r "}"))
         (:description
          "assert [... =] ..."
          :key "a"
          :mark-node t
          :shorthand expressions
          :name "assert"
          :template ("assert "
                     ;; An empty answer arrives as the tag name, which is never a valid pattern.
                     (p PATTERN "Pattern (empty for none)"
                        (lambda (text) (if (equal text "PATTERN") "" (concat text " = "))))
                     r))
         (:description
          "{:noreply, ...}"
          :key "n"
          :mark-node t
          :shorthand expressions
          :name "noreply"
          :template ("{:noreply, " r "}"))
         (:description
          "if ... do ... end"
          :key "i"
          :mark-node t
          :shorthand statements
          :name "if"
          :template ("if " (p condition "Condition") " do" n> r> n "end" >))
         (:description
          "fn -> ... end"
          :key "f"
          :mark-node t
          :shorthand statements
          :name "fn"
          :template ("fn " @ "->" n> r> n "end" >))
         (:description
          "def ...() do ... end"
          :key "D"
          :mark-node t
          :shorthand statements
          :name "def"
          :template ((save-column
                      (choice* :name "def" :rest ("def"))
                      (choice* :name "defp" :rest ("defp"))
                      " " (p name "Name") "(" @ ") do" n> r> n)
                     "end"))
         (:description
          "try do ... rescue ... end"
          :key "t"
          :mark-node t
          :shorthand statements
          :name "try"
          :template ((save-column "try do" n> r> n)
                     (save-column "rescue" n> "e -> " @ n)
                     "end"))
         (:description
          "describe \"...\" do ... end"
          :key "T"
          :mark-node t
          :shorthand statements
          :name "describe"
          :template ((save-column "describe \"" (p description "Description") "\" do" n> r> n)
                     "end"))
         (:description
          "for ... <- ... do ... end"
          :key "F"
          :mark-node t
          :shorthand expressions
          :name "for"
          :template ((save-column "for " (p variable "Variable") " <- " r " do" n)
                     ;; The unfilled head does not parse, so indent the body by hand.
                     (save-column "  " @ n)
                     "end"))))
      (highlight-queries-default
       '(;; highlight debugging calls left in the code
         (((call target: (identifier) @hl.fiery) (:match "^dbg$" @hl.fiery)))
         (((call target: (dot left: (alias) @_module right: (identifier) @_function) @hl.fiery)
           (:match "^IO$" @_module) (:match "^inspect$" @_function)))
         (((call target: (dot left: (alias) @_module right: (identifier) @_function) @hl.fiery)
           (:match "^IEx$" @_module) (:match "^pry$" @_function)))
         ;; and test tags that focus or skip tests
         (((unary_operator operand: (call target: (identifier) @_name (arguments (atom) @_value))) @hl.fiery
           (:match "^\\(tag\\|describetag\\|moduletag\\)$" @_name) (:match "^:\\(focus\\|skip\\)$" @_value)))))
      (procedures-sibling
       '(;; Statements, definitions and clauses when point is at their start.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :position at
                   :has-parent ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent :match-children t))
         ;; Elements of argument lists and collections.  Plain children
         ;; carry the `@match' marks that splicing and dragging expect.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :has-parent ("arguments" "list" "tuple" "map_content" "keywords" "bitstring")))
          :selector (:choose parent :match-children t))
         ;; Everything else, including heads, pipelines and keyword lists.
         ;; The query runs on the nearest block so it stays cheap.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :has-ancestor ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                  "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--sibling-p @match)))
                                     :engine treesitter)))))
      (procedures-hierarchy
       '(;; The target is usually inside the node at point, so query that first.
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :position at))
          :selector (:choose node
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--down-p @match)))
                                     :engine treesitter)))
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :has-ancestor ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                  "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--down-p @match)))
                                     :engine treesitter)))
         ;; Lists the node types that navigating up may stop at.
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name elixir
 :major-modes (elixir-ts-mode)
 :custom combobulate-elixir-definitions
 :setup-fn combobulate-elixir-setup)

(defun combobulate-elixir-setup (_)
  (setq-local forward-sexp-function #'combobulate-elixir-forward-sexp)
  (define-key (combobulate-read envelope-map) "|" #'combobulate-elixir-dbg-pipe)
  (let ((map (combobulate-read map)))
    (define-key map [remap combobulate-navigate-beginning-of-defun] #'treesit-beginning-of-defun)
    (define-key map [remap combobulate-navigate-end-of-defun] #'treesit-end-of-defun)
    (define-key map [remap combobulate-mark-defun] #'mark-defun)
    (define-key map [remap combobulate-navigate-next] #'combobulate-elixir-navigate-next)
    (define-key map [remap combobulate-navigate-previous] #'combobulate-elixir-navigate-previous)
    (define-key map [remap combobulate-navigate-up] #'combobulate-elixir-navigate-up)
    (define-key map [remap combobulate-navigate-down] #'combobulate-elixir-navigate-down)
    (define-key map [remap combobulate-navigate-sequence-next] #'combobulate-elixir-navigate-sequence-next)
    (define-key map [remap combobulate-navigate-sequence-previous]
                #'combobulate-elixir-navigate-sequence-previous)
    (define-key map [remap combobulate-drag-up] #'combobulate-elixir-drag-up)
    (define-key map [remap combobulate-drag-down] #'combobulate-elixir-drag-down)
    (define-key map [remap combobulate-kill-node-dwim] #'combobulate-elixir-kill-node-dwim)
    (define-key map [remap combobulate-clone-node-dwim] #'combobulate-elixir-clone-node-dwim)
    (define-key map [remap combobulate-cursor-edit-sequence-dwim] #'combobulate-elixir-edit-function-name)
    (define-key map [remap combobulate-splice-up] #'combobulate-elixir-splice-up)
    (define-key map [remap combobulate-splice-down] #'combobulate-elixir-splice-down)
    (define-key map [remap combobulate-splice-self] #'combobulate-elixir-splice-self)
    (define-key map [remap combobulate-splice-parent] #'combobulate-elixir-splice-parent)))

(provide 'combobulate-elixir)
;;; combobulate-elixir.el ends here
