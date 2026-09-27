;;; combobulate-sql.el --- SQL support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the DerekStride tree-sitter-sql grammar, as used by
;; `sql-ts-mode'.
;;
;; The grammar gives every keyword a named node, such as
;; `keyword_select', so the procedures discard them everywhere.
;;
;; `where', `join', `order_by' and the other clauses after `FROM' are
;; children of the `from' node, so they are siblings of each other
;; rather than of `select' and `from'.  Dragging refuses to swap two
;; different clauses, such as a join and `WHERE'.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-sql nil
  "Configuration switches for SQL"
  :group 'combobulate
  :prefix "combobulate-sql-")

(defun combobulate-sql-pretty-print-node-name (node _default-name)
  "Pretty printer for SQL nodes"
  (combobulate-string-truncate
   (string-trim (car (split-string (combobulate-node-text node) "\n")))
   40))

(defun combobulate-sql--operator (node)
  "Return the node type of the `AND' or `OR' that NODE joins two conditions with."
  (and node
       (equal (treesit-node-type node) "binary_expression")
       (car (member (treesit-node-type (treesit-node-child-by-field-name node "operator"))
                    '("keyword_and" "keyword_or")))))

(defun combobulate-sql--conditions (node)
  "Return the conditions that the `AND' or `OR' of NODE joins, however nested."
  (let ((operator (combobulate-sql--operator node)))
    (mapcan (lambda (child)
              (if (equal (combobulate-sql--operator child) operator)
                  (combobulate-sql--conditions child)
                (list child)))
            (list (treesit-node-child-by-field-name node "left")
                  (treesit-node-child-by-field-name node "right")))))

(defun combobulate-sql--conditions-at-point ()
  "Return the condition at point and those the same `AND' or `OR' joins it with.

`a AND b AND c' nests as `(a AND b) AND c', but all three are
siblings.  In `a AND b OR c', `a' is a sibling of `b' only."
  (when-let* ((condition (car (seq-sort-by (lambda (node) (- (treesit-node-end node) (treesit-node-start node)))
                                           #'<
                                           (seq-filter (lambda (node)
                                                         (combobulate-sql--operator (treesit-node-parent node)))
                                                       (combobulate-all-nodes-at-point))))))
    (let* ((root (treesit-node-parent condition))
           (operator (combobulate-sql--operator root)))
      (while (equal (combobulate-sql--operator (treesit-node-parent root)) operator)
        (setq root (treesit-node-parent root)))
      (combobulate-sql--conditions root))))

(defun combobulate-sql--condition-p (node)
  (seq-some (lambda (condition) (treesit-node-eq condition node))
            (combobulate-sql--conditions-at-point)))

(defun combobulate-sql--statement-p (node)
  "Return non-nil if NODE is a top-level statement with other statements around it.

A lone statement, as in a `~SQL' sigil, is left to the procedures
for its clauses."
  (let ((parent (treesit-node-parent node)))
    (and (member (treesit-node-type parent) '("program" "block" "transaction"))
         (> (length (treesit-filter-child parent (lambda (child)
                                                   (equal (treesit-node-type child) "statement"))))
            1))))

(defun combobulate-sql--values-row-p (node)
  (and (equal (treesit-node-type node) "list")
       (equal (treesit-node-type (treesit-node-parent node)) "insert")
       (member (treesit-node-type (treesit-node-prev-sibling node t)) '("keyword_values" "list"))))

(defun combobulate-sql--row-p (node)
  "Return non-nil if NODE is a row of `VALUES', and point is at one too.

The column list of `INSERT' is a `list' as well, and must not be
dragged among the rows."
  (and (combobulate-sql--values-row-p node)
       (seq-some #'combobulate-sql--values-row-p (combobulate-all-nodes-at-point))))

(eval-and-compile
  (defconst combobulate-sql--keywords
    (seq-filter (lambda (type) (string-prefix-p "keyword_" type)) combobulate-rules-sql-types)
    "Node types of the keywords.")

  (defconst combobulate-sql--clauses
    '("select" "from" "insert" "update" "delete" "returning" "set_operation")
    "Node types of the clauses of a statement.")

  (defconst combobulate-sql--collections
    '("select_expression" "list" "column_definitions" "index_fields" "group_by" "order_by"
      "function_arguments" "partition_by" "from" "update")
    "Node types whose children are all siblings of each other.")

  (defconst combobulate-sql--predicates
    '("binary_expression" "between_expression" "exists" "unary_expression"
      "parenthesized_expression")
    "Node types of the conditions that `AND' and `OR' join.")

  (defvar combobulate-sql-definitions
    `((context-nodes '("identifier" "literal" "parameter"))
      (procedure-discard-rules '("comment" "marginalia" ,@combobulate-sql--keywords))
      (pretty-print-node-name-function #'combobulate-sql-pretty-print-node-name)
      (plausible-separators '("," ";"))
      (navigate-down-into-lists nil)
      (procedures-defun '((:activation-nodes ((:nodes ("statement"))))))
      (procedures-sibling
       ;; Procedures are tried in order, each against point's node and
       ;; all its ancestors, so the ones that need point at the start
       ;; of their node come first.
       '((:activation-nodes
          ((:nodes ("statement") :position at :has-parent ("program" "block" "transaction")))
          :selector (:choose parent
                             :match-query (:query (((statement) @match
                                                    (:pred combobulate-sql--statement-p @match)))
                                                  :engine treesitter)))
         (:activation-nodes
          ((:nodes ,combobulate-sql--clauses :position at :has-parent ("statement" "subquery")))
          :selector (:choose parent :match-children (:match-rules ,combobulate-sql--clauses)))
         (:activation-nodes
          ((:nodes ("cte") :position at :has-parent ("statement")))
          :selector (:choose parent :match-children (:match-rules ("cte"))))
         (:activation-nodes
          ((:nodes ("term") :position at :has-parent ("invocation")))
          :selector (:choose parent :match-children (:match-rules ("term"))))
         (:activation-nodes
          ((:nodes ,combobulate-sql--predicates :position at :has-ancestor ("statement" "subquery")))
          :selector (:choose parent
                             :match-query (:query (((_) @match (:pred combobulate-sql--condition-p @match)))
                                                  :engine treesitter)))
         (:activation-nodes
          ((:nodes ("assignment") :position at :has-parent ("update" "assignment_list")))
          :selector (:choose parent :match-children (:match-rules ("assignment"))))
         (:activation-nodes
          ((:nodes ("list") :position at :has-parent ("insert")))
          :selector (:choose parent
                             :match-query (:query (((list) @match (:pred combobulate-sql--row-p @match)))
                                                  :engine treesitter)))
         (:activation-nodes
          ((:nodes ((all)) :has-parent ,combobulate-sql--collections))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("cte") :position at))
          :selector (:choose node
                             :match-query (:query (cte (statement (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("select" "returning") :position at))
          :selector (:choose node
                             :match-query (:query (_ (select_expression (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("create_table") :position at))
          :selector (:choose node
                             :match-query (:query (create_table (column_definitions (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((all)) :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name sql
 :major-modes (sql-ts-mode)
 :custom combobulate-sql-definitions
 :setup-fn combobulate-sql-setup)

(defun combobulate-sql-setup (_))

(defconst combobulate-sql--joins '("join" "cross_join" "lateral_join" "lateral_cross_join")
  "Node types of the joins, which can be reordered among themselves.")

(defun combobulate-sql--kind (node)
  "Return what NODE is, for deciding whether it can swap places with a sibling."
  (let ((type (combobulate-node-type node)))
    (cond
     ((equal (combobulate-node-type (combobulate-node-parent node)) "binary_expression") 'operand)
     ((member type combobulate-sql--joins) 'join)
     (t type))))

(defun combobulate-sql--drag (command arg direction)
  "Run the drag COMMAND with ARG, unless it would swap two different clauses.

`WHERE' is a sibling of the joins, and `FROM' of `SELECT', but
swapping them makes invalid SQL."
  (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
    (when-let* ((real (lambda (node) (if (combobulate-proxy-node-p node)
                                         (combobulate-proxy-node-to-real-node node)
                                       node)))
                (nearest (combobulate--get-nearest-navigable-node))
                (self (funcall real (or (combobulate-nav-get-self-sibling nearest) nearest)))
                (siblings (mapcar real (combobulate-nav-get-siblings self)))
                (index (seq-position siblings self #'combobulate-node-eq))
                (neighbor (and (or (eq direction 'down) (> index 0))
                               (nth (if (eq direction 'down) (1+ index) (1- index)) siblings))))
      (unless (equal (combobulate-sql--kind self) (combobulate-sql--kind neighbor))
        (user-error "Cannot swap %s with %s"
                    (combobulate-pretty-print-node self) (combobulate-pretty-print-node neighbor)))))
  (funcall command arg))

(defun combobulate-sql-drag-up (&optional arg)
  "Like `combobulate-drag-up', but refuse to swap two different clauses."
  (interactive "^p")
  (combobulate-sql--drag #'combobulate-drag-up arg 'up))

(defun combobulate-sql-drag-down (&optional arg)
  "Like `combobulate-drag-down', but refuse to swap two different clauses."
  (interactive "^p")
  (combobulate-sql--drag #'combobulate-drag-down arg 'down))

;; Bound here rather than in the setup function so the Elixir commands
;; find them for SQL in `~SQL' sigils.
(define-key combobulate-sql-map [remap combobulate-drag-up] #'combobulate-sql-drag-up)
(define-key combobulate-sql-map [remap combobulate-drag-down] #'combobulate-sql-drag-down)

(provide 'combobulate-sql)
;;; combobulate-sql.el ends here
