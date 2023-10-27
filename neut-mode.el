;;; neut-mode.el --- neut mode -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>

;;; Commentary:

;; A major mode for Neut.

;;; Code:

(defvar neut-mode-indent-offset 2)

(defconst neut-mode--opening-parens '(?\( ?\{ ?\[))
(defconst neut-mode--closing-parens '(?\) ?\} ?\]))

(defun neut-mode-indent-line ()
  (interactive)
  (let ((original-offset-from-eol (neut-mode--get-offset-from-eol)))
    (indent-line-to (neut-mode--calculate-indentation))
    (when (< original-offset-from-eol (neut-mode--get-offset-from-eol))
      (goto-char (- (line-end-position) original-offset-from-eol)))))

(defun neut-mode--get-offset-from-eol ()
  (- (line-end-position) (point)))

(defun neut-mode--calculate-indentation ()
  (save-excursion
    (if (not (= (forward-line -1) 0))
        0
      (let ((current-indent (neut-mode--get-current-indentation)))
        (let* ((parent-indent (neut-mode--get-parent-indentation))
               (positive-offset (neut-mode--get-positive-offset-of-next-line))
               (negative-offset (neut-mode--get-negative-offset-of-next-line))
               (adjuster (if (> positive-offset 0) (- current-indent parent-indent) 0)))
          (+ parent-indent positive-offset negative-offset adjuster))))))

(defun neut-mode--back-until-nonempty ()
  (when (neut-mode--line-empty-p)
    (forward-line -1)
    (neut-mode--back-until-nonempty)))

(defun neut-mode--get-current-indentation ()
  (save-excursion
    (neut-mode--back-until-nonempty)
    (neut-mode--get-first-char-column)))

(defun neut-mode--get-parent-indentation ()
  (save-excursion
    (neut-mode--back-until-nonempty)
    (goto-char (line-end-position))
    (neut-mode--go-to-parent-line)
    (neut-mode--get-first-char-column)))

(defun neut-mode--get-first-char-column ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward " -")
    (current-column)))

(defun neut-mode--get-positive-offset-of-next-line ()
  (save-excursion
    (neut-mode--back-until-nonempty)
    (let* ((begins-with-hyphen (neut-mode--line-begins-with-hyphen-p))
           (ends-with-equal (neut-mode--line-ends-with-equal-p))
           (ends-with-open-paren (neut-mode--line-ends-with-opening-paren-p))
           (cond-list
            (list
             ends-with-open-paren
             (and ends-with-equal (not begins-with-hyphen)))))
      (apply '+ (mapcar (lambda (b) (if b neut-mode-indent-offset 0)) cond-list)))))

(defun neut-mode--get-negative-offset-of-next-line ()
  (save-excursion
    (if (not (= (forward-line 1) 0))
        0
      (let* ((begins-with-hyphen (neut-mode--line-begins-with-hyphen-p))
             (begins-with-closing-paren (neut-mode--line-begins-with-closing-paren-p))
             (cond-list
              (list
               begins-with-hyphen
               begins-with-closing-paren)))
        (apply '+ (mapcar (lambda (b) (if b (* -1 neut-mode-indent-offset) 0)) cond-list))))))

(defun neut-mode--go-to-parent-line ()
  (interactive)
  (cond
   ((neut-mode--line-empty-p)
    (forward-line -1)
    (neut-mode--go-to-parent-line))
   ((member (char-before) neut-mode--closing-parens)
    (backward-sexp)
    (neut-mode--go-to-parent-line))
   ((string= (neut-mode--get-preceding-symbol) "in")
    (message "found in")
    (neut-mode--jump-back-to-let)
    (neut-mode--go-to-parent-line)
    )
   ((not (= (point) (line-beginning-position)))
    (backward-char)
    (neut-mode--go-to-parent-line))
   ((neut-mode--should-go-up-p)
    (backward-char)
    (neut-mode--go-to-parent-line))))

(defun neut-mode--get-preceding-symbol ()
  (save-excursion
    (backward-word)
    (thing-at-point 'symbol)))

(defun neut-mode--jump-back-to-let ()
  "Jump to the corresponding `let' from `in'."
  (cond
   ((string= (thing-at-point 'symbol) "let")
    nil)
   ((string= (thing-at-point 'symbol) "in")
    (backward-word)
    (neut-mode--jump-back-to-let) ;; skip inner let-in
    (backward-word)
    (neut-mode--jump-back-to-let))
   (t
    (when (backward-word)
      (neut-mode--jump-back-to-let)))))

(defun neut-mode--should-go-up-p ()
  (let ((base-indentation (current-indentation)))
    (save-excursion
      (when (= (forward-line -1) 0)
        (neut-mode--back-until-nonempty)
        (let ((previous-indentation (current-indentation)))
          (and
           (< previous-indentation base-indentation)
           (not (neut-mode--line-ends-with-equal-p))
           (not (neut-mode--line-ends-with-opening-paren-p))
           (not (neut-mode--line-ends-with-in-p))))))))

(defun neut-mode--line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun neut-mode--line-begins-with-closing-paren-p ()
  (save-excursion
    (skip-chars-forward " ")
    (member (char-after) neut-mode--closing-parens)))

(defun neut-mode--line-begins-with-hyphen-p ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward " ")
    (eq (char-after) ?-)))

(defun neut-mode--line-ends-with-opening-paren-p ()
  (save-excursion
    (goto-char (line-end-position))
    (skip-chars-backward " ")
    (member (char-before) neut-mode--opening-parens)))

(defun neut-mode--line-ends-with-equal-p ()
  (save-excursion
    (goto-char (line-end-position))
    (skip-chars-backward " ")
    (eq (char-before) ?=)))

(defun neut-mode--line-ends-with-in-p ()
  (string-match-p "\sin$" (thing-at-point 'line)))

(defun neut-mode--insert-bullet ()
  (interactive)
  (cond
   ((= (point) (line-beginning-position))
    (insert "- ")
    )
   ((not (neut-mode--line-empty-p))
    (insert "-"))
   (t
    (insert "- ")
    (indent-according-to-mode))))

;;;###autoload
(define-derived-mode neut-mode prog-mode "neut"
  "A major mode for neut."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?/ "_ 12" syntax-table)
     (modify-syntax-entry ?\n ">" syntax-table)

     (modify-syntax-entry ?- "_" syntax-table)
     (modify-syntax-entry ?_ "w" syntax-table)
     (modify-syntax-entry ?. "." syntax-table)
     (modify-syntax-entry ?< "_" syntax-table)
     (modify-syntax-entry ?> "_" syntax-table)
     (modify-syntax-entry ?: "." syntax-table)
     (modify-syntax-entry ?+ "_" syntax-table)
     (modify-syntax-entry ?* "_" syntax-table)
     (modify-syntax-entry ?? "w" syntax-table)
     syntax-table))
  ;; (setq-local indent-line-function 'neut-mode-indent-line)

  (define-key neut-mode-map "-" #'neut-mode--insert-bullet)
  (setq font-lock-defaults
        `(,`((,(regexp-opt '("tau" "flow") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("attach" "bind" "case" "data" "declare" "default" "define" "detach" "do" "else" "else-if" "external" "if" "import" "in" "inline" "introspect" "let" "match" "mu" "of" "on" "resource" "then" "tie" "try" "type" "when" "with") 'symbols)
              . font-lock-keyword-face)
             (,(regexp-opt '("-" "->" ":" "=" "=>" "_" "assert" "magic" "target-arch" "target-os" "target-platform" "tuple") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("::") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("admit") 'symbols)
              . font-lock-warning-face)
             (,(regexp-opt '("this" "base") 'symbols)
              . font-lock-constant-face)
             ("\\<\[A-Z\]\[-A-Za-z0-9\]\*\\>"
              . font-lock-type-face)
             ("define +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("inline +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("data +\\([^[:space:]\s(\s)]+?\\)[ \n\s(\s)]"
              . (1 font-lock-function-name-face))
             ("type +\\([^[:space:]\s(\s)]+?\\)[ \n\s(\s)]"
              . (1 font-lock-function-name-face))
             ("*"
              . font-lock-builtin-face)
             ("?"
              . font-lock-type-face)
             ("&"
              . font-lock-type-face)
             (":"
              . font-lock-builtin-face)
             ("@"
              . font-lock-builtin-face)))))

;;;###autoload
(defun neut-mode-setup-lsp-mode ()
  (interactive)
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     (lambda() `("neut" "lsp" "--no-color")))
                    :server-id 'lsp-neut
                    :major-modes '(neut-mode))))

;;;###autoload
(defun neut-mode-setup-eglot ()
  (interactive)
  (setq-local eglot-server-programs
              `((neut-mode . ("neut" "lsp" "--no-color")))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.nt\\'" 'neut-mode))

(provide 'neut-mode)

;;; neut-mode.el ends here
