;;; neut-mode.el --- A major mode for Neut -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; URL: https://github.com/vekatze/neut-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A major mode for the Neut programming language.

;;; Code:

;;;; Customisation group

(defgroup neut nil
  "Major mode for editing Neut source files."
  :group 'languages)

;;;; Delimiter tables

(defconst neut-indent-offset 2
  "Basic indentation step for `neut-mode'.")

(defconst neut--open-parens '(?\( ?\{ ?\[)
  "Opening parenthesis characters recognised by Neut.")

(defconst neut--close-parens '(?\) ?\} ?\])
  "Closing parenthesis characters recognised by Neut.")

(defconst neut--opening-delims
  `(,@neut--open-parens ?= ?<)
  "All opening delimiters, including Neut specific tokens ‘=’ and ‘<’.")

(defconst neut--closing-delims
  `(,@neut--close-parens ?\; ?>)
  "All closing delimiters, including Neut specific tokens ‘;’ and ‘>’.")

;;;; Internal helpers: syntax & text properties

(defun neut--in-string-or-comment-p (pos)
  "Return non-nil if POS lies inside a string or comment."
  (let ((ppss (syntax-ppss pos)))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun neut--arrow-start-p (pos)
  "Return non-nil if the two characters ending at POS form “->” or “=>”."
  (and (eq (char-after pos) ?>)
       (memq (char-after (1- pos)) '(?= ?-))))

(defun neut--colon-equal-start-p (pos)
  "Return non-nil if the two characters ending at POS form “:=”."
  (and (eq (char-after pos) ?=)
       (eq (char-after (1- pos)) ?:)))

(defun neut--line-offset-from-eol ()
  "Return number of characters between point and end-of-line."
  (- (line-end-position) (point)))

(defun neut--indent-of-line (pos)
  "Indentation of the line containing POS, or −`neut-indent-offset' if POS is nil."
  (if pos
      (save-excursion
        (goto-char pos)
        (goto-char (line-beginning-position))
        (skip-chars-forward " |")
        (current-column))
    (- neut-indent-offset)))

(defun neut--line-starts-with-closing-delim-p (pos)
  "Return non-nil if the line at POS begins with a closing delimiter token."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (memq (char-after) '(?\) ?\] ?\} ?> ?|))))

(defun neut--matching-delims-p (open close)
  "Return non-nil if OPEN and CLOSE form a matching delimiter pair."
  (pcase (cons open close)
    (`(?\( . ?\)) t)
    (`(?\{ . ?\}) t)
    (`(?\[ . ?\]) t)
    (`(?=  . ?\;) t)
    (`(?<  . ?>)  t)
    (_ nil)))

(defun neut--comment-start-on-line (pos)
  "Return buffer position of “//” comment on current line, or nil if none."
  (save-excursion
    (goto-char pos)
    (goto-char (line-beginning-position))
    (let ((limit (line-end-position)))
      (when (search-forward "//" limit t)
        (match-beginning 0)))))

;;;; Delimiter search (indent engine core)

(defun neut--innermost-opening-delim (cursor)
  "Return position of innermost opening delimiter enclosing CURSOR.
Skip delimiters inside strings, comments and multi-char tokens like \"->\"."
  (let ((pos cursor)
        (limit (point-min))
        (stack '())
        result)
    (while (and (not result) (> pos limit))
      (setq pos (1- pos))
      (let ((ch (char-after pos)))
        (cond
         ;; Handle comments
         ((eq ch ?\n)
          (setq pos (or (neut--comment-start-on-line pos) pos)))
         ;; Handle strings
         ((eq ch ?\")
          (setq pos (or (scan-sexps (1+ pos) -1) pos)))
         ;; Skip arrows
         ((neut--arrow-start-p pos)
          (setq pos (1- pos)))
         ;; Skip ":="
         ((neut--colon-equal-start-p pos)
          (setq pos (1- pos)))
         ;; Closing delimiters
         ((memq ch neut--closing-delims)
          (push ch stack))
         ;; Opening delimiters
         ((memq ch neut--opening-delims)
          ;; Collapse ; stack frames until a matching opener for = is found
          (while (and stack (eq (car stack) ?\;) (not (eq ch ?=)))
            (pop stack))
          (if (and stack (neut--matching-delims-p ch (car stack)))
              (pop stack)
            (setq result pos))))))
    result))

;;;; Indentation calculation

(defun neut--desired-indentation ()
  "Compute desired indentation for current line."
  (if (neut--in-string-or-comment-p (line-beginning-position))
      (neut--indent-of-line (point))
    (let* ((parent-pos (neut--innermost-opening-delim (line-beginning-position)))
           (base-ind (neut--indent-of-line parent-pos)))
      (if (neut--line-starts-with-closing-delim-p (point))
          base-ind
        (+ base-ind neut-indent-offset)))))

(defun neut-indent-line ()
  "Indent current line according to Neut rules."
  (interactive)
  (let ((orig-eol-offset (neut--line-offset-from-eol)))
    (indent-line-to (neut--desired-indentation))
    ;; Restore cursor’s relative EOL position
    (when (< orig-eol-offset (neut--line-offset-from-eol))
      (goto-char (- (line-end-position) orig-eol-offset)))))

;;;; Minor editing helpers

(defun neut--electric-indent-p (_char)
  "Function for `electric-indent-functions'—indent when line begins with “in”."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "in\\_>")))

(defun neut--line-empty-p ()
  "Return non-nil when current line contains only whitespace."
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun neut-insert-bar ()
  "Smart insertion of “|” respecting Neut layout rules."
  (interactive)
  (cond
   ((bolp)
    (insert "| "))
   ((not (neut--line-empty-p))
    (insert "|"))
   (t
    (insert "| ")
    (indent-according-to-mode))))

;;;; Font-lock

(defconst neut--font-lock-keywords
  (let* ((types    '("meta" "pointer" "rune" "thread" "type" "void"))
         (keywords '("attach" "bind" "box" "case" "data" "default" "define" "detach" "do" "else" "else-if" "exact" "external" "foreign" "function" "if" "import" "inline" "introspect" "let" "letbox" "letbox-T" "match" "nominal" "of" "on" "pin" "quote" "resource" "tie" "try" "use" "when" "with"))
         (builtins '("->" "=" "=>" "_" "assert" "magic" "include-text" "static"))
         (warnings '("admit"))
         (constants '("this"))
         (regexp-sym (lambda (syms) (regexp-opt syms 'symbols))))
    `((,(rx line-start "=" (* nonl))   . font-lock-doc-face)
      (,(funcall regexp-sym types)     . font-lock-type-face)
      (,(funcall regexp-sym keywords)  . font-lock-keyword-face)
      (,(funcall regexp-sym builtins)  . font-lock-builtin-face)
      (,(funcall regexp-sym warnings)  . font-lock-warning-face)
      (,(funcall regexp-sym constants) . font-lock-constant-face)
      ("\\_<\\(?:define\\|inline\\|function\\|resource\\)\\_>[[:space:]]+\\([^[:space:]\n({<\\[)}>\n]+\\)"
       (1 font-lock-function-name-face))
      ("\\_<data\\_>[[:space:]]+\\([^[:space:]\n()]+\\)"
       (1 font-lock-function-name-face))
      ("\\_<\\(?:_?\\.?[A-Z][-A-Za-z0-9_]*\\)\\_>"
       . font-lock-type-face)
      (,(rx (any "*+\\|!,&:@?,;"))
       . font-lock-builtin-face)))
  "Default highlighting expressions for `neut-mode'.")

;;;; Major mode definition

;;;###autoload
(define-derived-mode neut-mode prog-mode "Neut"
  "Major mode for editing Neut source code."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (set-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?/ ". 12" st)
     (modify-syntax-entry ?\n ">" st)
     (dolist (ch (string-to-list "-_.<>+*?&"))
       (modify-syntax-entry ch "_" st))
     (modify-syntax-entry ?` "\"" st)
     st))
  (setq-local indent-line-function #'neut-indent-line)
  (add-hook 'electric-indent-functions #'neut--electric-indent-p nil t)
  (setq-local font-lock-defaults '(neut--font-lock-keywords))
  (define-key neut-mode-map "|" #'neut-insert-bar)
  (setq-local xref-prompt-for-identifier nil))

;;;; LSP / Eglot integration

;;;###autoload
(with-eval-after-load 'lsp-mode
  (defvar lsp-language-id-configuration)
  (add-to-list 'lsp-language-id-configuration '(neut-mode . "neut"))
  (when (and (fboundp 'lsp-register-client)
             (fboundp 'make-lsp-client)
             (fboundp 'lsp-stdio-connection))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () '("neut" "lsp")))
      :server-id 'lsp-neut
      :major-modes '(neut-mode)))))

;;;###autoload
(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs '(neut-mode . ("neut" "lsp"))))

;;;; File associations -------------------------------------------------------

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nt\\'" . neut-mode))

(provide 'neut-mode)

;;; neut-mode.el ends here
