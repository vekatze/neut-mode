;;; neut-mode.el --- A major mode for Neut -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; URL: https://github.com/vekatze/neut-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A major mode for the Neut programming language.

;;; Code:

;; indentation

(defvar neut-mode-indent-offset 2
  "Indentation offset for `neut-mode'.")

(defconst neut--open-parens
  '(?\( ?\{ ?\[))

(defconst neut--close-parens
  '(?\) ?\} ?\]))

(defconst neut--opening-delims
  `(,@neut--open-parens ?= ?<))

(defconst neut--closing-delims
  `(,@neut--close-parens ?\; ?>))

(defun neut--in-string-or-comment-p (pos)
  "Return non-nil if POS is in a string or comment."
  (let ((ppss (syntax-ppss pos)))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun neut--arrow-p (pos)
  "Return non-nil if character before POS is = or - (i.e., part of => or ->)."
  (and
   (eq (char-after pos) ?>)
   (memq (char-after (1- pos)) '(?= ?-))))

(defun neut--colon-equal-p (pos)
  "Return non-nil if character before POS is :=."
  (and
   (eq (char-after pos) ?=)
   (eq (char-after (1- pos)) ?:)))

(defun neut--offset-from-eol ()
  "Return the number of characters between point and end of line."
  (- (line-end-position) (point)))

(defun neut--get-indentation-of (pos)
  "Return indentation of the line containing POS, or negative offset if nil."
  (if pos
      (save-excursion
        (goto-char pos)
        (current-indentation))
    (* -1 neut-mode-indent-offset)))

(defun neut--should-dedent-p (pt)
  "Return non-nil if the line at PT starts with a closing delimiter."
  (save-excursion
    (goto-char pt)
    (skip-chars-forward " ")
    (memq (char-after) '(?\) ?\] ?\} ?> ?|))))

(defun neut--match-delim-p (open close)
  "Return non-nil if OPEN and CLOSE form a matching delimiter pair."
  (pcase (cons open close)
    (`(?\( . ?\)) t)
    (`(?\{ . ?\}) t)
    (`(?\[ . ?\]) t)
    (`(?=  . ?\;) t)
    (`(?<  . ?>)  t)
    (_ nil)))

(defun neut--skip-comment (point)
  "Return the buffer position of the leftmost '//' on the current line, or nil
if not found."
  (save-excursion
    (goto-char point)
    (goto-char (line-beginning-position))
    (let ((line-end (line-end-position)))
      (if (search-forward "//" line-end t)
            (match-beginning 0)
        point))))

(defun neut--find-innermost-enclosing-paren (cursor)
  "Find the position of the innermost opening delimiter enclosing CURSOR.
Skips delimiters inside strings/comments and handles ( { [ = < … ) } ] ; >."
  (let ((pos cursor)
        (limit (point-min))
        (stack '())
        (result nil))
    (while (and (not result) (> pos limit))
      (setq pos (1- pos))
      (let ((ch (char-after pos)))
        (cond
         ((eq ch ?\n)
          (setq pos (neut--skip-comment pos)))
         ((eq ch ?\")
          (let ((open (scan-sexps (1+ pos) -1)))
            (setq pos open)))
         ((memq ch neut--close-parens)
          (let ((open (ignore-errors (scan-sexps (1+ pos) -1))))
            (if (not open)
                (push ch stack)
              (push ch stack)
              (setq pos (1+ open)))))
         ;; skip "->" and "=>"
         ((neut--arrow-p pos)
          (setq pos (1- pos)))
         ;; skip ":="
         ((neut--colon-equal-p pos)
          (setq pos (1- pos)))
         ((memq ch neut--closing-delims)
          (push ch stack))
         ((memq ch neut--opening-delims)
          (while (and stack (eq (car stack) ?\;) (not (eq ch ?=)))
            (pop stack))
          (if (and stack (neut--match-delim-p ch (car stack)))
              (pop stack)
            (setq result pos))))))
    result))

(defun neut--calculate-indentation ()
  "Return the desired indentation for the current line."
  (if (neut--in-string-or-comment-p (line-beginning-position))
      (neut--get-indentation-of (point))
    (let* ((parent-pos (save-excursion
                         (neut--find-innermost-enclosing-paren (line-beginning-position))))
           (parent-indent (neut--get-indentation-of parent-pos)))
      (if (neut--should-dedent-p (point))
          parent-indent
        (+ parent-indent neut-mode-indent-offset)))))

(defun neut-mode-indent-line ()
  "Indent the current line appropriately for `neut-mode'."
  (interactive)
  (let ((original-offset (neut--offset-from-eol)))
    (indent-line-to (neut--calculate-indentation))
    ;; Preserve relative cursor position from end of line
    (when (< original-offset (neut--offset-from-eol))
      (goto-char (- (line-end-position) original-offset)))))

(defun neut--electric-indent-p (_)
  "Return non-nil if the current line should be indented now.

Intended to be used with `electric-indent-functions'."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:space:]")
    (looking-at "in")))

;; utils

(defun neut--line-empty-p ()
  "Return non-nil if the current line is empty."
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun neut--insert-bar ()
  "Insert '| '."
  (interactive)
  (cond
   ((bolp)
    (insert "| "))
   ((not (neut--line-empty-p))
    (insert "|"))
   (t
    (insert "| ")
    (indent-according-to-mode))))

;; main

;;;###autoload
(define-derived-mode neut-mode prog-mode "neut"
  "A major mode for Neut."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?/ ". 12" syntax-table)
     (modify-syntax-entry ?\n ">" syntax-table)
     (modify-syntax-entry ?- "_" syntax-table)
     (modify-syntax-entry ?_ "_" syntax-table)
     (modify-syntax-entry ?. "_" syntax-table)
     (modify-syntax-entry ?< "_" syntax-table)
     (modify-syntax-entry ?> "_" syntax-table)
     (modify-syntax-entry ?: "." syntax-table)
     (modify-syntax-entry ?+ "_" syntax-table)
     (modify-syntax-entry ?? "." syntax-table)
     (modify-syntax-entry ?* "." syntax-table)
     (modify-syntax-entry ?& "." syntax-table)
     (modify-syntax-entry ?` "\"" syntax-table)
     syntax-table))
  (setq-local indent-line-function #'neut-mode-indent-line)
  (setq-local xref-prompt-for-identifier nil)
  (add-hook 'electric-indent-functions #'neut--electric-indent-p nil 'local)
  (define-key neut-mode-map "|" #'neut--insert-bar)
  (setq font-lock-defaults
        `(,`(("^=.*" . font-lock-doc-face)
             (,(regexp-opt '("meta" "pointer" "rune" "thread" "type" "void") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("attach" "bind" "box" "case" "data" "default" "define" "detach" "do" "else" "else-if" "exact" "external" "foreign" "function" "if" "import" "inline" "introspect" "let" "letbox" "letbox-T" "match" "nominal" "of" "on" "pin" "quote" "resource" "tie" "try" "use" "when" "with") 'symbols)
              . font-lock-keyword-face)
             (,(regexp-opt '("->" "=" "=>" "_") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("assert" "magic" "include-text" "static") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("admit") 'symbols)
              . font-lock-warning-face)
             (,(regexp-opt '("this") 'symbols)
              . font-lock-constant-face)
             ("\\<define\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<inline\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<function\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<data\\> +\\([^[:space:]\s(\s)]+?\\)[ \n\s(\s)]"
              . (1 font-lock-function-name-face))
             ("\\<resource\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\_<\\(_?\\.?\[A-Z\]\[-A-Za-z0-9_\]\*\\)\\_>"
              . (1 font-lock-type-face))
             ("\\_<\\(_?\\.?\[A-Z\]\[-A-Za-z0-9_\]\*\\)\\."
              . (1 font-lock-type-face))
             ("\\_<\\.\\.\\.\\_>"
              . font-lock-constant-face)
             ("*"
              . font-lock-builtin-face)
             ("+"
              . font-lock-builtin-face)
             ("\\\\"
              . font-lock-builtin-face)
             ("|"
              . font-lock-builtin-face)
             ("!"
              . font-lock-builtin-face)
             (","
              . font-lock-builtin-face)
             (";"
              . font-lock-builtin-face)
             ("?"
              . font-lock-builtin-face)
             ("&"
              . font-lock-builtin-face)
             (":"
              . font-lock-builtin-face)
             ("@"
              . font-lock-builtin-face)))))

;;;###autoload
(when (require 'lsp-mode nil t)
  (defvar lsp-language-id-configuration)
  (add-to-list 'lsp-language-id-configuration '(neut-mode . "neut"))
  (add-to-list 'lsp-language-id-configuration '(neut-mode . "neut"))
  (when (and
         (fboundp 'lsp-register-client)
         (fboundp 'make-lsp-client)
         (fboundp 'lsp-stdio-connection))
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-stdio-connection
                       (lambda() `("neut" "lsp")))
                      :server-id 'lsp-neut
                      :major-modes '(neut-mode)))))

;;;###autoload
(when (featurep 'eglot)
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs
               '(neut-mode . ("neut" "lsp"))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.nt\\'" 'neut-mode))

(provide 'neut-mode)

;;; neut-mode.el ends here
