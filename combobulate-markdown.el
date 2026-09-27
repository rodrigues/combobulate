;;; combobulate-markdown.el --- markdown support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the block grammar of tree-sitter-markdown, as used by
;; `markdown-ts-mode'.
;;
;; `markdown-ts-mode' parses the text inside each block with a second
;; parser, `markdown-inline'.  Combobulate has no `markdown-inline'
;; language, so the commands use the block tree there too, and
;; emphasis, links and code spans are not navigable.
;;
;; Inside a fenced code block in a language Combobulate supports, the
;; commands navigate that language instead.
;;
;; Sections are siblings of sections, and the other blocks of a
;; section are siblings of each other.  Keeping the two apart means
;; dragging a paragraph never moves it into a subsection.  Likewise
;; the rows of a table leave out its header, and point at the start
;; of a table is on the table rather than on the header.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)
(eval-when-compile (require 'cl-lib))

(defgroup combobulate-markdown nil
  "Configuration switches for Markdown"
  :group 'combobulate
  :prefix "combobulate-markdown-")

(defun combobulate-markdown-pretty-print-node-name (node _default-name)
  "Pretty printer for Markdown nodes"
  (combobulate-string-truncate
   (string-trim
    (car (split-string (combobulate-node-text node) "\n")))
   40))

(defun combobulate-markdown--trimmed-range (node)
  "Return the range of NODE without the blank lines the grammar ends it with."
  (save-excursion
    (goto-char (combobulate-node-end node))
    (skip-chars-backward " \t\n" (combobulate-node-start node))
    (cons (combobulate-node-start node) (point))))

(defun combobulate-markdown--drag (command arg)
  "Run the drag COMMAND with ARG, leaving blank lines between the nodes in place.

Sections and the last item of a list end after the blank lines that
follow them, so swapping their whole ranges moves those lines.  Point
first moves to the start of the sibling it is in, because it is
usually in a block's text, where drag would take the paragraph."
  (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
    (when-let* ((node (combobulate--get-nearest-navigable-node))
                (self (seq-find #'combobulate-point-in-node-range-p
                                (combobulate-nav-get-siblings node))))
      (goto-char (combobulate-node-start self))))
  (cl-letf (((symbol-function 'combobulate--swap-node-regions)
             (lambda (node-a node-b)
               (transpose-subr-1 (combobulate-markdown--trimmed-range node-a)
                                 (combobulate-markdown--trimmed-range node-b)))))
    (funcall command arg)))

(defun combobulate-markdown-drag-up (&optional arg)
  "Like `combobulate-drag-up', but leave the blank lines between nodes in place."
  (interactive "^p")
  (combobulate-markdown--drag #'combobulate-drag-up arg))

(defun combobulate-markdown-drag-down (&optional arg)
  "Like `combobulate-drag-down', but leave the blank lines between nodes in place."
  (interactive "^p")
  (combobulate-markdown--drag #'combobulate-drag-down arg))

(eval-and-compile
  (defconst combobulate-markdown--blocks
    '("paragraph" "list" "pipe_table" "block_quote" "fenced_code_block" "indented_code_block"
      "html_block" "thematic_break" "setext_heading" "link_reference_definition"
      "minus_metadata" "plus_metadata")
    "Node types of the blocks inside a section, a block quote or a list item.")

  (defvar combobulate-markdown-definitions
    `((context-nodes '("inline" "language"))
      (envelope-list nil)
      (pretty-print-node-name-function #'combobulate-markdown-pretty-print-node-name)
      (highlight-queries-default nil)
      (navigate-down-into-lists nil)
      (procedures-defun '((:activation-nodes ((:nodes ("section"))))))
      (procedures-sexp '((:activation-nodes ((:nodes ("section" "list_item" ,@combobulate-markdown--blocks))))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ("pipe_table_cell") :has-parent ("pipe_table_header" "pipe_table_row")))
          :selector (:choose parent :match-children (:match-rules ("pipe_table_cell"))))
         (:activation-nodes
          ((:nodes ("pipe_table_row") :has-parent ("pipe_table")))
          :selector (:choose parent :match-children (:match-rules ("pipe_table_row"))))
         (:activation-nodes
          ((:nodes ("list_item") :has-parent ("list")))
          :selector (:choose parent :match-children (:match-rules ("list_item"))))
         (:activation-nodes
          ((:nodes ,combobulate-markdown--blocks :has-parent ("document" "section" "block_quote")))
          :selector (:choose parent :match-children (:match-rules ,combobulate-markdown--blocks)))
         (:activation-nodes
          ((:nodes ("section") :has-parent ("document" "section")))
          :selector (:choose parent :match-children (:match-rules ("section"))))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("pipe_table_row") :position at))
          :selector (:choose node :match-children (:match-rules ("pipe_table_cell"))))
         (:activation-nodes
          ((:nodes ("list_item") :position at))
          :selector (:choose node :match-children
                             (:match-rules ,(remove "paragraph" combobulate-markdown--blocks))))
         (:activation-nodes
          ((:nodes ("section" "block_quote") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("section" ,@combobulate-markdown--blocks))))
         (:activation-nodes
          ((:nodes ("pipe_table") :position at))
          :selector (:choose node :match-children (:match-rules ("pipe_table_row"))))))
      (procedures-logical '((:activation-nodes ((:nodes (all)))))))))

(define-combobulate-language
 :name markdown
 :major-modes (markdown-ts-mode)
 :custom combobulate-markdown-definitions
 :setup-fn combobulate-markdown-setup)

(defun combobulate-markdown-setup (_))

;; Bound here rather than in the setup function so the Elixir and Erlang
;; commands find them for Markdown doc strings in their own buffers.
(define-key combobulate-markdown-map [remap combobulate-drag-up] #'combobulate-markdown-drag-up)
(define-key combobulate-markdown-map [remap combobulate-drag-down] #'combobulate-markdown-drag-down)

(provide 'combobulate-markdown)
;;; combobulate-markdown.el ends here
