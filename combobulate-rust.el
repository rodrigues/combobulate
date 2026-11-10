;;; combobulate-rust.el --- rust support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the tree-sitter-rust grammar, as used by `rust-ts-mode'.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-rust nil
  "Configuration switches for Rust"
  :group 'combobulate
  :prefix "combobulate-rust-")

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ((or "function_item" "function_signature_item")
       (format "fn %s" (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ((and (or "struct_item" "enum_item" "union_item" "trait_item" "mod_item" "type_item") type)
       (format "%s %s"
               (string-remove-suffix "_item" type)
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("impl_item"
       (format "impl %s" (combobulate-node-text (combobulate-node-child-by-field node "type"))))
      (_ default-name)))
   40))

(eval-and-compile
  (defconst combobulate-rust--containers
    '("source_file" "block" "declaration_list" "field_declaration_list"
      "ordered_field_declaration_list" "enum_variant_list" "field_initializer_list"
      "match_block" "arguments" "parameters" "closure_parameters" "use_list"
      "array_expression" "tuple_expression" "tuple_pattern" "tuple_type"
      "tuple_struct_pattern" "struct_pattern" "slice_pattern"
      "type_arguments" "type_parameters")
    "Node types whose children are siblings of each other.")

  (defconst combobulate-rust--comments
    '("line_comment" "block_comment")
    "Node types of comments, which navigation skips.")

  (defconst combobulate-rust--wrappers
    '("block" "declaration_list" "field_declaration_list" "ordered_field_declaration_list"
      "enum_variant_list" "field_initializer_list" "match_block" "arguments" "parameters"
      "closure_parameters" "use_list" "type_arguments" "type_parameters")
    "Node types that only group other nodes, so up skips them.")

  (defvar combobulate-rust-definitions
    `((context-nodes
       '("identifier" "field_identifier" "type_identifier" "primitive_type"
         "integer_literal" "float_literal" "string_literal" "char_literal"
         "boolean_literal" "self"))
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_item" "function_signature_item" "struct_item" "enum_item"
                    "union_item" "impl_item" "trait_item" "mod_item" "type_item"
                    "const_item" "static_item" "macro_definition"))))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-rust--comments))
                   :has-parent ,combobulate-rust--containers))
          :selector (:choose parent
                             :match-children (:discard-rules ,combobulate-rust--comments)))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("function_item" "closure_expression" "if_expression" "else_clause"
                    "loop_expression" "while_expression" "for_expression" "unsafe_block"
                    "async_block")
                   :position at))
          :selector (:choose node
                             :match-query (:query (_ (block (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("impl_item" "trait_item" "mod_item") :position at))
          :selector (:choose node
                             :match-query (:query (_ (declaration_list (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("match_expression") :position at))
          :selector (:choose node
                             :match-query (:query (match_expression (match_block (match_arm)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("call_expression") :position at))
          :selector (:choose node
                             :match-query (:query (call_expression (arguments (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("struct_item" "enum_variant") :position at))
          :selector (:choose node
                             :match-query (:query (_ (field_declaration_list (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("enum_item") :position at))
          :selector (:choose node
                             :match-query (:query (enum_item (enum_variant_list (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("struct_expression") :position at))
          :selector (:choose node
                             :match-query (:query (struct_expression (field_initializer_list (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-rust--wrappers)) :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name rust
 :major-modes (rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
