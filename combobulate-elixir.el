;;; combobulate-elixir.el --- elixir-specific features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
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

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-rules)

;; (declare-function combobulate--mark-node "combobulate-manipulation")
;; (declare-function combobulate-indent-region "combobulate-manipulation")

;; (defvar combobulate-elixir-indent--direction nil)

;; (defgroup combobulate-elixir nil
;;   "Configuration switches for Elixir."
;;   :group 'combobulate
;;   :prefix "combobulate-elixir-")

;; (defcustom combobulate-elixir-smart-indent t
;;   "Smarter indentation handling.

;; Greatly improves indentation handling when you indent
;; regions. Combobulate will automatically indent the region and
;; preserve the existing indentation. You can then cycle through
;; indentation levels to pick the right one.

;; If `combobulate-elixir-indent-blocks-dwim' is non-nil, then
;; Combobulate will automatically pick the code block immediately
;; ahead of point. You can use this to cycle through the indentation
;; of blocks of code: functions, for statements, etc.

;; This works by remapping `indent-for-tab-command' to
;; `combobulate-elixir-indent-for-tab-command'."
;;   :type 'boolean
;;   :group 'combobulate-elixir)

;; (defcustom combobulate-elixir-indent-mark-region nil
;;   "Mark the region when indenting and leave it enabled after.

;; When non-nil, Combobulate will mark the region when indenting
;; with `combobulate-elixir-indent-for-tab-command'.

;; When nil, the mark is instead deactivated after indenting."
;;   :type 'boolean
;;   :group 'combobulate-elixir)

;; (defcustom combobulate-elixir-indent-blocks-dwim t
;;   "Indent a whole block if point is on it instead of the line.

;; With point at the beginning of a block -- such as a for statement
;; -- Combobulate will instead indent the block instead of just the
;; line when you press
;; \\[combobulate-elixir-indent-for-tab-command]."
;;   :type 'boolean)

;; (defun combobulate-elixir--get-definition (node)
;;   (string-join
;;    (combobulate-query-node-text
;;     (pcase (combobulate-node-type node)
;;       ("function_definition"
;;        '((_) name: (_) @name parameters: (_) @args))
;;       ("class_definition"
;;        '((_) name: (_) @name superclasses: (_) @args)))
;;     node
;;     t)
;;    ""))

;; (defun combobulate-elixir-pretty-print-node-name (node default-name)
;;   "Pretty printer for Elixir nodes"
;;   (combobulate-string-truncate
;;    (replace-regexp-in-string
;;     (rx (| (>= 2 " ") "\n")) ""
;;     (pcase (combobulate-node-type node)
;;       ("function_definition" (concat "def " (combobulate-elixir--get-definition node)))
;;       ("class_definition" (concat "class " (combobulate-elixir--get-definition node)))
;;       (_ default-name)))
;;    40))

;; (defun combobulate-elixir--display-indicator ()
;;   (let* ((levels (elixir-indent-calculate-levels))
;;          (total-levels (length levels))
;;          (matched-level (member (current-indentation) levels))
;;          (current-level (abs (- (length matched-level) total-levels))))
;;     (combobulate-display-indicator current-level total-levels)))

;; (defvar-local combobulate-elixir-indent-cycle nil)

;; (defun combobulate-elixir-indent-region (start end &optional columns)
;;   "Indent a elixir region between START and END to COLUMNS.

;; This function is designed to be called from
;; `indent-region-function' and (indirectly) through
;; \\[combobulate-elixir-indent-for-tab-command]."
;;   (let ((deactivate-mark nil))
;;     (save-mark-and-excursion
;;       (when (and (use-region-p) (> (point) (mark)))
;;         (exchange-point-and-mark))
;;       (skip-chars-backward combobulate-skip-prefix-regexp
;;                            (line-beginning-position))
;;       (when (bolp)
;;         (setq start (point)))
;;       (let ((next-level (combobulate-elixir-indent-determine-next-level)))
;;         (pcase (cons combobulate-elixir-indent--direction next-level)
;;           ((and `(forward . (,prev . nil)))
;;            (setq combobulate-elixir-indent--direction 'backward
;;                  columns prev))
;;           ((and `(forward . (,_ . ,next)))
;;            (setq combobulate-elixir-indent--direction 'forward
;;                  columns next))
;;           ((and `(backward . (nil . ,next)))
;;            (setq combobulate-elixir-indent--direction 'forward
;;                  columns next))
;;           ((and `(backward . (,prev . ,_)))
;;            (setq combobulate-elixir-indent--direction 'backward
;;                  columns prev))
;;           (_ (setq columns nil))))
;;       (when columns
;;         (combobulate-indent-region
;;          start end columns nil t))
;;       (setq this-command 'combobulate-elixir-indent-for-tab-command))))

;; (defvar-local combobulate-elixir-indent-cycle nil
;;   "List of indentation columns to cycle through with
;; \\[combobulate-elixir-indent-for-tab-command].")

;; (defun combobulate-elixir-maybe-indent-block-at-point ()
;;   "Maybe indent the block at point.

;; Returns a non-nil value to indicate the indentation took place."
;;   (with-navigation-nodes
;;       (:nodes (append
;;                ;; rules that trigger indentation
;;                (combobulate-production-rules-get "_simple_statement")
;;                (combobulate-production-rules-get "_compound_statement"))
;;               ;; do not skip prefix if we have a region active. the
;;               ;; reason for that is that skipping forward with a
;;               ;; marked region can bork the indentation mechanism as
;;               ;; we can only effectively indent with whole lines.
;;               :skip-prefix (not (use-region-p))
;;               :skip-newline nil)
;;     (when-let ((node (combobulate--get-nearest-navigable-node)))
;;       (when (and (not (use-region-p))
;;                  combobulate-elixir-indent-blocks-dwim
;;                  (combobulate-point-at-beginning-of-node-p
;;                   (combobulate--get-nearest-navigable-node)))
;;         (combobulate--mark-node node t t)))))

;; (defun combobulate-elixir-calculate-indent (pos)
;;   (let ((calculated-indentation (save-excursion
;;                                   (goto-char pos)
;;                                   (combobulate-filter-nodes
;;                                    (combobulate-get-parents
;;                                     (combobulate-node-at-point))
;;                                    :keep-types
;;                                    '("block"
;;                                      ;; required because, for some inexplicable reason, the
;;                                      ;; elixir grammar does not consider a match-case statement
;;                                      ;; to consist of a case clause and a block clause unlike
;;                                      ;; literally everything else.
;;                                      "case_clause")))))
;;     (if (null calculated-indentation)
;;         (current-indentation)
;;       (* elixir-indent-offset (length calculated-indentation)))))


;; (defun combobulate-elixir-indent-determine-next-level ()
;;   "Determine the next indentation level.

;; This function returns a cons cell of the form (PREV . NEXT) where
;; PREV is the relative change to indentation level to deindent one
;; step. NEXT is the next indentation level. If there is no previous
;; or next indentation level, the corresponding value is nil."
;;   (cons
;;    (and (car (last (seq-filter (lambda (n) (< n (current-indentation)))
;;                                (elixir-indent-calculate-levels))))
;;         (- elixir-indent-offset))
;;    (and (seq-filter (lambda (n) (> n (current-indentation)))
;;                     (elixir-indent-calculate-levels))
;;         elixir-indent-offset)))

;; (defun combobulate-elixir-indent-for-tab-command (&optional arg)
;;   "Proxy command for `indent-for-tab-command' that keeps region active.

;; This command preserves the region if it is active. Subsequent
;; indent commands cycle through all valid indentation stops."
;;   (interactive "P")
;;   (let ((region-manually-activated (use-region-p)))
;;     (save-excursion
;;       (combobulate-elixir-maybe-indent-block-at-point)
;;       (if (use-region-p)
;;           (progn
;;             ;; work out the correct indentation point to use as a baseline:
;;             ;; the top-most place in the region.
;;             (save-mark-and-excursion
;;               (when (> (point) (mark))
;;                 (exchange-point-and-mark))
;;               (unless (eq this-command last-command)
;;                 (setq combobulate-elixir-indent--direction
;;                       ;; figure out the direction to cycle in.
;;                       (if (cdr (combobulate-elixir-indent-determine-next-level))
;;                           'forward
;;                         'backward)))
;;               (indent-for-tab-command arg)
;;               (combobulate-message
;;                (concat (combobulate-elixir--display-indicator)
;;                        " "
;;                        (substitute-command-keys
;;                         "Press \\[combobulate-elixir-indent-for-tab-command] \
;; again to cycle indentation.")))))
;;         (indent-for-tab-command arg)))
;;     (if region-manually-activated
;;         (progn
;;           (setq deactivate-mark nil)
;;           (activate-mark))
;;       (setq deactivate-mark t))
;;     ;; HACK: handle the case where we might be indenting on a
;;     ;; blank/whitespace-only line. Ordinarily, that would take us to
;;     ;; the farthest indentation point, but `save-excursion' will not
;;     ;; preserve the point in that case. So we need to do it manually.
;;     (when (and (not region-manually-activated)
;;                (save-excursion
;;                  (beginning-of-line)
;;                  (looking-at-p "[[:space:]]*$")))
;;       (indent-for-tab-command arg))))

(defun combobulate-elixir-setup (_)
  ;; do not indent envelopes.
  ;; (setq combobulate-envelope-indent-region-function nil)
  ;; (when combobulate-elixir-smart-indent
  ;;   ;; Override `indent-for-tab-command'
  ;;   (local-set-key [remap indent-for-tab-command] #'combobulate-elixir-indent-for-tab-command))
  ;; (setq indent-region-function 'combobulate-elixir-indent-region)
  ;; (setq combobulate-manipulation-indent-after-edit nil)
  ;; (setq combobulate-pretty-print-node-name-function #'combobulate-elixir-pretty-print-node-name)
  ;; (setq combobulate-manipulation-splicing-procedures
  ;;       `(
  ;;         (:activation-nodes
  ;;          ((:node
  ;;            ,(append (combobulate-production-rules-get "_simple_statement")
  ;;                     (combobulate-production-rules-get "_compound_statement")
  ;;                     (combobulate-production-rules-get "if_statement")
  ;;                     (combobulate-production-rules-get "try_statement")
  ;;                     '("case_clause"))
  ;;            :find-base-rule-parent t
  ;;            :position at-or-in))
  ;;          :match-siblings (:keep-parent nil))))

  ;; (let ((statement-nodes
  ;;        (append (combobulate-production-rules-get "_compound_statement")
  ;;                '("block"))))
  ;;   (setq combobulate-manipulation-envelopes
  ;;         `((:description
  ;;            "( ... )"
  ;;            :key "("
  ;;            :extra-key "M-("
  ;;            :mark-node t
  ;;            :nodes ,(append (combobulate-production-rules-get "primary_expression")
  ;;                            (combobulate-production-rules-get "expression"))
  ;;            :name "wrap-parentheses"
  ;;            :template (@ "(" r ")"))
  ;;           (:description
  ;;            "Decorate class or function"
  ;;            :key "@"
  ;;            :mark-node nil
  ;;            :nodes ("function_definition" "class_definition")
  ;;            :name "decorate"
  ;;            :template ((p @decorator "Decorator name"
  ;;                          (lambda (text)
  ;;                            (if (string-prefix-p "@" text)
  ;;                                text
  ;;                              (concat "@" text))))
  ;;                       n>))
  ;;           (:description
  ;;            "if ...:"
  ;;            :key "bi"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-if"
  ;;            :template
  ;;            ("if " @ ":" n>
  ;;             r>))
  ;;           (:description
  ;;            "try ... except ...: ..."
  ;;            :key "bte"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-try-except"
  ;;            :template
  ;;            (@ (save-column "try:" n>
  ;;                            r> n>)
  ;;               "except " (p Exception "Exception") ":" n>
  ;;               "pass"))
  ;;           (:description
  ;;            "try ... finally: ..."
  ;;            :key "btf"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-try-finally"
  ;;            :template
  ;;            (@ (save-column "try:" n>
  ;;                            r> n>)
  ;;               "finally:" n>
  ;;               "pass"))
  ;;           (:description
  ;;            "def ...():"
  ;;            :key "bd"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-def"
  ;;            :template
  ;;            ("def " (p name "Name") "(" @ ")" ":" n>
  ;;             r>))
  ;;           (:description
  ;;            "for ...:"
  ;;            :key "bf"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-for"
  ;;            :template
  ;;            ("for " @ ":" n>
  ;;             r>))
  ;;           (:description
  ;;            "with ...:"
  ;;            :key "bW"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-with"
  ;;            :template
  ;;            ("with " @ ":" n>
  ;;             r>))
  ;;           (:description
  ;;            "while ...:"
  ;;            :key "bw"
  ;;            :mark-node t
  ;;            :nodes ,statement-nodes
  ;;            :name "nest-while"
  ;;            :template
  ;;            ("while " @ ":" n>
  ;;             r>)))))

  ;; (add-to-list 'elixir-indent-trigger-commands 'combobulate-elixir-indent-for-tab-command)

  ;; (setq combobulate-manipulation-edit-procedures
  ;;       '(;; edit comments in blocks
  ;;         (:activation-nodes
  ;;          ((:node "comment" :find-parent ("block") :position at-or-in))
  ;;          :match-query (block (comment)+ @match))
  ;;         ;; edit pairs in dictionaries
  ;;         (:activation-nodes
  ;;          ((:node "pair" :find-parent "dictionary" :position at-or-in)
  ;;           (:node "dictionary" :position at-or-in))
  ;;          :match-query (dictionary (pair)+ @match)
  ;;          :remove-types ("comment"))
  ;;         ;; edit parameters in functions
  ;;         (:activation-nodes
  ;;          ((:node "function_definition" :position at-or-in))
  ;;          :match-query (function_definition (parameters (_)+ @match))
  ;;          :remove-types ("comment"))
  ;;         ;; edit elements in containers and blocks
  ;;         (:activation-nodes
  ;;          ((:node ("block" "tuple_pattern" "set" "list" "tuple") :position at-or-in))
  ;;          :match-query ((_) (_)+ @match)
  ;;          ;; :match-children t
  ;;          :remove-types ("comment"))
  ;;         ;; edit arguments in calls
  ;;         (:activation-nodes
  ;;          ((:node "argument_list" :position at-or-in))
  ;;          :match-query ((argument_list) (_)+ @match)
  ;;          :remove-types ("comment"))
  ;;         ;; edit imports
  ;;         (:activation-nodes
  ;;          ((:node "import_from_statement" :position at-or-in :find-parent "module"))
  ;;          :match-query (import_from_statement name: (dotted_name)+ @match))))

  ;; (setq combobulate-manipulation-indent-method 'first)
  ;; (setq combobulate-calculate-indent-function #'combobulate-elixir-calculate-indent)

  ;; call(target: [identifier: "def" | "defn"])
  (setq combobulate-navigation-defun-nodes '("call"))
  (setq combobulate-navigation-sexp-nodes '("call"))
  (setq combobulate-navigation-drag-parent-nodes '("call" "do_block"))
  (setq combobulate-navigation-sibling-procedures
        `((:activation-nodes
           ((:node
             ,(append
               (combobulate-production-rules-get "call")
               (combobulate-production-rules-get "identifier")
               (combobulate-production-rules-get "binary_operator"))
             :position at-or-in
             :find-immediate-parent ("do_block" "list" "arguments")))
           :match-children t
           :remove-types ("comment"))))

  ;; (combobulate-production-rules-set '("argument_list"
  ;;                                     :included-fields (:*unnamed*)
  ;;                                     :expand-rules (("expression" :all t)
  ;;                                                    ("primary_expression" :all t))))
  ;; (combobulate-production-rules-set '("function_definition"
  ;;                                     :included-fields (:body)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("class_definition"
  ;;                                     :included-fields (:body)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("parameters"
  ;;                                     :included-fields (:*unnamed*)
  ;;                                     :expand-rules (("parameter" :all t))))
  ;; (combobulate-production-rules-set '("if_statement"
  ;;                                     :included-fields (:consequence :alternative)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("for_statement"
  ;;                                     :included-fields (:body)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("match_statement"
  ;;                                     :included-fields (:alternative)))
  ;; (combobulate-production-rules-set '("try_statement"
  ;;                                     :included-fields (:body :*unnamed*)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("while_statement"
  ;;                                     :included-fields (:body)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("with_statement"
  ;;                                     :included-fields (:body)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("case_clause"
  ;;                                     :included-fields (:consequence)
  ;;                                     :expand-nodes (("block" :all t))))
  ;; (combobulate-production-rules-set '("import_from_statement"
  ;;                                     :included-fields (:name)
  ;;                                     :expand-rules (("dotted_name" :all t))))
  ;; (combobulate-production-rules-set '("list"
  ;;                                     :all t
  ;;                                     :expand-rules (("expression" :all t)
  ;;                                                    ("primary_expression" :all t))))
  ;; (combobulate-production-rules-set '("tuple"
  ;;                                     :all t
  ;;                                     :expand-rules (("expression" :all t)
  ;;                                                    ("primary_expression" :all t))))

  (setq combobulate-navigation-parent-child-nodes
        '("call" "arguments" "identifier" "alias"))

  (setq combobulate-navigation-logical-nodes
        (append
         (combobulate-production-rules-get "do_block")
         (combobulate-production-rules-get "binary_operator")
         combobulate-navigation-default-nodes))

  (setq combobulate-navigation-default-nodes
        (seq-uniq (append
                   combobulate-navigation-logical-nodes
                   combobulate-navigation-parent-child-nodes))))

(provide 'combobulate-elixir)
;;; combobulate-elixir.el ends here
