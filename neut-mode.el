;;; neut-mode.el --- neut mode -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>

;;; Commentary:

;; A major mode for Neut.

;;; Code:

;;
;; indentation
;;

;; the indentation of current line can be calculated as follows:
;;
;;   (*1) find the shallowest part(s) of current line and go there
;;   (*2) backtrack and find the nearest encloser
;;   (*3) (current line's indentation) = (the indentation of the nearest encloser) + offset
;;
;; tips:
;;
;;   1. the indentation of the root encloser should be regarded as (-1) * base-offset
;;   2. `let .. in' is also a parenthesis pair, just like `(..)'


(defvar neut-mode-indent-offset 2)

(defun neut--get-offset-from-eol ()
  (- (line-end-position) (point)))

(defun neut-mode-indent-line ()
  (interactive)
  (let ((original-offset-from-eol (neut--get-offset-from-eol)))
    (indent-line-to (neut--calculate-indentation))
    (when (< original-offset-from-eol (neut--get-offset-from-eol))
      (goto-char (- (line-end-position) original-offset-from-eol)))))

(defun neut--calculate-indentation ()
  (if (or (neut--in-string-p (point))
          (neut--in-string-p (line-beginning-position)))
      (neut--get-indentation-of (point)) ;; leave strings as they are
    (let* ((bullet-offset (neut--get-bullet-offset (point)))
           (child-line-number (line-number-at-pos (point)))
           (child-indentation (neut--get-indentation-of child-line-number))
           (parent-pos-or-none (save-excursion
                                 (neut--goto-indent-base-position) ;; (*1)
                                 (neut--get-parent 0))) ;; (*2)
           (parent-indentation (neut--get-indentation-of parent-pos-or-none))
           (parent-bullet-offset (* -1 (neut--get-bullet-offset parent-pos-or-none))))
      (+ parent-indentation neut-mode-indent-offset bullet-offset parent-bullet-offset)))) ;; (*3)

(defun neut--get-indentation-of (pos)
  (if pos
      (save-excursion
        (neut--goto-line (line-number-at-pos pos))
        (neut--get-first-char-column))
    (* -1 neut-mode-indent-offset)))

(defun neut--goto-indent-base-position ()
  (goto-char (line-end-position))
  (goto-char (neut--find-shallowest-point 0 0 (point))))

(defun neut--goto-line (line-number)
  (goto-char (point-min))
  (forward-line (- line-number 1)))

(defun neut--goto-first-char-column ()
  (goto-char (line-beginning-position))
  (skip-chars-forward " ")
  (current-column))

(defun neut--get-first-char-column ()
  (save-excursion (neut--goto-first-char-column)))

(defun neut--in-string-p (pos)
  (nth 3 (syntax-ppss pos)))

(defun neut--skip-comment (start)
  (interactive)
  (when (re-search-backward "//" (line-beginning-position) t)
    (let ((pos (match-beginning 0)))
      (if (neut--in-string-p pos)
          (goto-char start)
        (neut--skip-comment pos)))))

(defun neut--find-shallowest-point (eval-value max-eval-value shallowest-pos)
  "Backtrack from the end of a line and find (one of) the shallowest point of the line.

The shallowness of a point is evaluated by `eval-value'; This value is incremented when
an open paren is found, and decremented when a closing paren is found."
  (let ((char (preceding-char)))
    (cond
     ((= char 0)
      shallowest-pos)
     ((= char ?\n)
      shallowest-pos)
     ((= char ? )
      (goto-char (- (point) 1))
      (neut--find-shallowest-point eval-value max-eval-value shallowest-pos))
     ;; found an opening paren
     ((neut--opening-paren-p char)
      (goto-char (- (point) 1))
      (neut--backtrack-opening-paren eval-value max-eval-value shallowest-pos))
     ;; found a closing paren
     ((neut--closing-paren-p char)
      (goto-char (- (point) 1))
      (neut--backtrack-closing-paren eval-value max-eval-value shallowest-pos))
     ;; opening angle
     ((neut--opening-angle-p)
      (goto-char (- (point) 1))
      (neut--backtrack-opening-paren eval-value max-eval-value shallowest-pos))
     ;; closing angle angle
     ((neut--genuine-closing-angle-p)
      (goto-char (- (point) 1))
      (neut--backtrack-closing-paren eval-value max-eval-value shallowest-pos))
     ((neut--closing-angle-p)
      (goto-char (- (point) 1))
      (neut--find-shallowest-point eval-value max-eval-value shallowest-pos))
     (t
      (let ((token (neut--get-token (point))))
        (cond
         ;; found a opening paren ("let")
         ((neut--open-token-p token)
          (neut--backtrack-opening-paren eval-value max-eval-value shallowest-pos))
         ;; found a closing paren ("in")
         ((string= token "in")
          (neut--backtrack-closing-paren eval-value max-eval-value shallowest-pos))
         ((string= token "")
          shallowest-pos)
         (t
          (neut--find-shallowest-point eval-value max-eval-value shallowest-pos))))))))

(defun neut--backtrack-opening-paren (eval-value max-eval-value shallowest-pos)
  (let ((next-eval-value (+ eval-value 1)))
    (if (> next-eval-value max-eval-value)
        (neut--find-shallowest-point next-eval-value next-eval-value (point))
      (neut--find-shallowest-point next-eval-value max-eval-value shallowest-pos))))

(defun neut--backtrack-closing-paren (eval-value max-eval-value shallowest-pos)
  (neut--find-shallowest-point (- eval-value 1) max-eval-value shallowest-pos))

(defun neut--get-bullet-offset (point-or-none)
  (if (not point-or-none)
      0
    (save-excursion
      (goto-char point-or-none)
      (goto-char (line-beginning-position))
      (skip-chars-forward " ")
      (if (re-search-forward "- " (+ (point) 2) t)
          (* -1 neut-mode-indent-offset)
        0))))

(defun neut--get-parent (nest-level)
  "Find the nearest encloser of current point by backtracking. Returns nil if the encloser is the file itself.

The `nest-level' is just to handle nested (let .. in).
This function must be called from outside a string."
  (let ((char (preceding-char)))
    (cond
     ((= char 0)
      nil)
     ((neut--skip-p char)
      (goto-char (- (point) 1))
      (neut--get-parent nest-level))
     ((neut--newline-p char)
      (goto-char (- (point) 1))
      (neut--skip-comment (point))
      (neut--get-parent nest-level))
     ((neut--opening-paren-p char)
      (point))
     ((neut--closing-paren-p char)
      (goto-char (scan-sexps (point) -1)) ;; skip a paren-pair
      (neut--get-parent nest-level))
     ((neut--opening-angle-p)
      (if (eq nest-level 0)
          (point)
        (goto-char (- (point) 1))
        (neut--get-parent (- nest-level 1))))
     ((neut--genuine-closing-angle-p)
      (goto-char (- (point) 1))
      (neut--get-parent (+ nest-level 1)))
     ((neut--closing-angle-p) ;; -> or =>
      (goto-char (- (point) 1))
      (neut--get-parent nest-level)
      )
     ((neut--double-quote-p char)
      (goto-char (scan-sexps (point) -1)) ;; skip a string
      (neut--get-parent nest-level))
     (t
      (let ((token (neut--get-token (point))))
        (cond
         ((neut--open-token-p token)
          (cond
           ((eq nest-level 0)
            (point))
           (t
            (neut--get-parent (- nest-level 1))))
          )
         ((neut--close-token-p token)
          (neut--get-parent (+ nest-level 1)))
         (t
          (neut--get-parent nest-level))))))))

(defun neut--get-token (initial-position)
  (let ((char (preceding-char)))
    (cond
     ((eq (point) (line-beginning-position))
      (buffer-substring-no-properties (point) initial-position))
     ((neut--non-token-p char)
      (buffer-substring-no-properties (point) initial-position))
     (t
      (goto-char (- (point) 1))
      (neut--get-token initial-position)))))

(defun neut--closing-angle-p ()
  (let ((char (preceding-char)))
    (equal char ?>)))

(defun neut--genuine-closing-angle-p ()
  (let ((char (preceding-char))
        (str (buffer-substring-no-properties (- (point) 2) (point))))
    (and (equal char ?>)
         (not (equal str "->"))
         (not (equal str "=>")))))

(defun neut--opening-angle-p ()
  (let ((char (preceding-char)))
    (equal char ?<)))

(defun neut--preceding-two-chars ()
  ())

(defun neut--make-hash-table (chars)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (char chars)
      (puthash char t table))
    table))

(defconst neut--opening-paren-char-set
  (neut--make-hash-table (list ?{ ?\( ?\[)))
(defconst neut--closing-paren-char-set
  (neut--make-hash-table (list ?} ?\) ?\])))
(defconst neut--skip-char-set
  (neut--make-hash-table (list ?\s ?, ?: ?\; ?&)))
(defconst neut--newline-char-set
  (neut--make-hash-table (list ?\n)))
(defconst neut--double-quote-char-set
  (neut--make-hash-table (list ?\")))
(defconst neut--non-token-char-set
  (neut--make-hash-table (list ?{ ?} ?\( ?\) ?\[ ?\] ?< ?> ?\s ?\n ?\;)))
(defconst neut--opening-token-set
  (neut--make-hash-table (list "let" "tie" "try" "bind" "use" "<")))
(defconst neut--closing-token-set
  (neut--make-hash-table (list "in" ">")))
(defun neut--opening-paren-p (char)
  (gethash char neut--opening-paren-char-set))
(defun neut--closing-paren-p (char)
  (gethash char neut--closing-paren-char-set))
(defun neut--skip-p (char)
  (gethash char neut--skip-char-set))
(defun neut--newline-p (char)
  (gethash char neut--newline-char-set))
(defun neut--double-quote-p (char)
  (gethash char neut--double-quote-char-set))
(defun neut--non-token-p (char)
  (gethash char neut--non-token-char-set))
(defun neut--open-token-p (char)
  (gethash char neut--opening-token-set))
(defun neut--close-token-p (char)
  (gethash char neut--closing-token-set))

(defun neut--electric-indent-p (_)
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward "[:space:]")
    (looking-at "in")))

;;
;; utils
;;

(defun neut--line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(defun neut--insert-bullet ()
  (interactive)
  (cond
   ((= (point) (line-beginning-position))
    (insert "- ")
    )
   ((not (neut--line-empty-p))
    (insert "-"))
   (t
    (insert "- ")
    (indent-according-to-mode))))

;;
;; defining major mode
;;

;;;###autoload
(define-derived-mode neut-mode prog-mode "neut"
  "A major mode for neut."
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?/ ". 12" syntax-table)
     (modify-syntax-entry ?\n ">" syntax-table)
     ;; (modify-syntax-entry ?- "_" syntax-table)
     ;; (modify-syntax-entry ?- "w" syntax-table)
     (modify-syntax-entry ?_ "w" syntax-table)
     ;; (modify-syntax-entry ?. "." syntax-table) ;; "_" ?
     (modify-syntax-entry ?. "_" syntax-table) ;; "_" ?
     ;; (modify-syntax-entry ?< "_" syntax-table)
     ;; (modify-syntax-entry ?> "_" syntax-table)
     (modify-syntax-entry ?< "_" syntax-table)
     (modify-syntax-entry ?> "_" syntax-table)
     (modify-syntax-entry ?: "." syntax-table)
     (modify-syntax-entry ?+ "_" syntax-table)
     (modify-syntax-entry ?* "_" syntax-table)
     (modify-syntax-entry ?? "w" syntax-table)
     syntax-table))
  (setq-local indent-line-function 'neut-mode-indent-line)
  (setq-local xref-prompt-for-identifier nil)
  (add-hook 'electric-indent-functions #'neut--electric-indent-p nil 'local)
  (define-key neut-mode-map "-" #'neut--insert-bullet)
  (setq font-lock-defaults
        `(,`(("^=.*" . font-lock-doc-face)
             (,(regexp-opt '("tau" "flow" "Pi") 'words)
              . font-lock-type-face)
             (,(regexp-opt '("arrow" "assume" "attach" "bind" "call" "case" "constant" "contract" "data" "default" "define" "detach" "do" "else" "else-if" "exact" "external" "fn" "foreign" "idealize" "if" "import" "in" "inline" "introspect" "lambda" "let" "match" "mutual" "nominal" "of" "on" "resource" "tie" "try" "use" "variadic" "when" "with") 'words)
              . font-lock-keyword-face)
             (,(regexp-opt '("-" "->" "->>" ":" "=" "=>" "_") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("assert" "magic" "target-arch" "target-os" "target-platform" "tuple") 'words)
              . font-lock-builtin-face)
             (,(regexp-opt '("::") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("admit") 'words)
              . font-lock-warning-face)
             (,(regexp-opt '("this") 'words)
              . font-lock-constant-face)
             ("\\_<_?\\(\[A-Z\]\[-A-Za-z0-9\]\*\\)\\_>"
              . (1 font-lock-type-face))
             ("\\_<_?\\(\[A-Z\]\[-A-Za-z0-9\]\*\\)\\."
              . (1 font-lock-type-face))
             ("\\<define\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<inline\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<real\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<realize\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<data\\> +\\([^[:space:]\s(\s)]+?\\)[ \n\s(\s)]"
              . (1 font-lock-function-name-face))
             ("\\<constant\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-constant-face))
             ("\\<resource\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-constant-face))
             ("\\<trope\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-constant-face))
             ("\\<declare\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<nominal\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<assume\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\(/[0-9]+\\)"
              . (1 font-lock-comment-face))
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
             ("#"
              . font-lock-builtin-face)
             ("?"
              . font-lock-type-face)
             ("&"
              . font-lock-builtin-face)
             (":"
              . font-lock-builtin-face)
             ("@"
              . font-lock-builtin-face))))
  )

;;;###autoload
(defun neut-mode-setup-lsp-mode ()
  (interactive)
  (add-to-list 'lsp-language-id-configuration '(neut-mode . "neut"))
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
