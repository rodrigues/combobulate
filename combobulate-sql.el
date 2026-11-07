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
;; rather than of `select' and `from'.

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
          ;; Matches nothing in a lone statement, as in a `~SQL' sigil, so its clauses are next.
          :selector (:choose parent
                             :match-query (:query (((_ (statement) @match (statement)))
                                                   ((_ (statement) (statement) @match)))
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
          ((:nodes ,combobulate-sql--predicates :position at :has-parent ("binary_expression")))
          :selector (:choose parent :match-children (:match-rules ,combobulate-sql--predicates)))
         (:activation-nodes
          ((:nodes ("assignment") :position at :has-parent ("update" "assignment_list")))
          :selector (:choose parent :match-children (:match-rules ("assignment"))))
         (:activation-nodes
          ((:nodes ("list") :position at :has-parent ("insert")))
          :selector (:choose parent
                             :match-query (:query (((insert (keyword_values) (list) @match)))
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

(provide 'combobulate-sql)
;;; combobulate-sql.el ends here
