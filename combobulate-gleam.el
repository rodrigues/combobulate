;;; combobulate-gleam.el --- gleam support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the gleam-lang tree-sitter-gleam grammar, as used by
;; `gleam-ts-mode'.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-gleam nil
  "Configuration switches for Gleam"
  :group 'combobulate
  :prefix "combobulate-gleam-")

(defun combobulate-gleam-pretty-print-node-name (node default-name)
  "Pretty printer for Gleam nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ((or "function" "external_function")
       (format "fn %s" (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ((or "type_definition" "type_alias" "external_type")
       (format "type %s" (combobulate-node-text (combobulate-node-child-by-field
                                                 (combobulate-node-child node 0 "type_name")
                                                 "name"))))
      (_ default-name)))
   40))

(eval-and-compile
  (defconst combobulate-gleam--containers
    '("source_file" "block" "arguments" "function_parameters" "case_clauses"
      "case_subjects" "data_constructors" "data_constructor_arguments"
      "record_pattern_arguments" "record_update_arguments" "unqualified_imports"
      "use_assignments" "type_arguments" "type_parameters" "function_parameter_types"
      "list" "tuple" "list_pattern" "tuple_pattern" "tuple_type"
      "bit_array" "bit_array_pattern" "bit_array_segment_options")
    "Node types whose children are siblings of each other.")

  (defconst combobulate-gleam--comments
    '("comment" "statement_comment" "module_comment")
    "Node types of comments, which navigation skips.")

  (defconst combobulate-gleam--wrappers
    '("block" "arguments" "function_parameters" "case_clauses" "case_subjects"
      "case_clause_patterns" "case_clause_pattern" "data_constructors"
      "data_constructor_arguments" "record_pattern_arguments" "record_update_arguments"
      "unqualified_imports" "use_assignments" "type_arguments" "type_parameters"
      "function_parameter_types" "bit_array_segment_options")
    "Node types that only group other nodes, so up skips them.")

  (defvar combobulate-gleam-definitions
    `((context-nodes
       '("identifier" "label" "constructor_name" "type_identifier"
         "integer" "float" "string" "discard"))
      (pretty-print-node-name-function #'combobulate-gleam-pretty-print-node-name)
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function" "external_function" "type_definition" "type_alias"
                    "external_type" "constant"))))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ("case_clause") :position at :has-parent ("case_clauses")))
          :selector (:choose parent :match-children (:match-rules ("case_clause"))))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-gleam--comments))
                   :has-parent ,combobulate-gleam--containers))
          :selector (:choose parent
                             :match-children (:discard-rules ,combobulate-gleam--comments)))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("function" "anonymous_function") :position at))
          :selector (:choose node
                             :match-query (:query (_ (block (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("case") :position at))
          :selector (:choose node
                             :match-query (:query (case (case_clauses (case_clause)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("function_call" "record") :position at))
          :selector (:choose node
                             :match-query (:query (_ (arguments (argument)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("data_constructor") :position at))
          :selector (:choose node
                             :match-query (:query (data_constructor (data_constructor_arguments (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("record_pattern") :position at))
          :selector (:choose node
                             :match-query (:query (record_pattern (record_pattern_arguments (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("import") :position at))
          :selector (:choose node
                             :match-query (:query (import (unqualified_imports (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("type_definition") :position at))
          :selector (:choose node
                             :match-query (:query (type_definition (data_constructors (data_constructor)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-gleam--wrappers)) :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name gleam
 :major-modes (gleam-ts-mode)
 :custom combobulate-gleam-definitions
 :setup-fn combobulate-gleam-setup)

(defun combobulate-gleam-setup (_))

(provide 'combobulate-gleam)
;;; combobulate-gleam.el ends here
