;;; combobulate-iex.el --- iex session support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the tree-sitter-iex grammar, which splits an IEx session,
;; such as a doctest, into evaluation blocks of prompt lines and a
;; result.
;;
;; No major mode uses the grammar on its own.  It is embedded, for
;; instance in the indented code blocks of Markdown doc strings, and
;; embeds Elixir in turn for the expressions and results.  So point is
;; in `iex' only on a block's first prompt, where next and previous move
;; between evaluation blocks.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-iex nil
  "Configuration switches for iex"
  :group 'combobulate
  :prefix "combobulate-iex-")

(eval-and-compile
  (defvar combobulate-iex-definitions
    '((context-nodes '("prompt"))
      (envelope-list nil)
      (highlight-queries-default nil)
      (navigate-down-into-lists nil)
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ("evaluation_block") :has-parent ("source")))
          :selector (:choose parent :match-children (:match-rules ("evaluation_block"))))))
      ;; Lists the node types that navigating up may stop at.
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("evaluation_block") :position at))
          :selector (:choose node :match-children (:match-rules ("result"))))))
      (procedures-logical '((:activation-nodes ((:nodes (all)))))))))

(define-combobulate-language
 :name iex
 :major-modes nil
 :custom combobulate-iex-definitions
 :setup-fn combobulate-iex-setup)

(defun combobulate-iex-setup (_))

(provide 'combobulate-iex)
;;; combobulate-iex.el ends here
