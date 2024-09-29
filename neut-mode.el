;;; neut-mode.el --- A major mode for Neut -*- lexical-binding: t; -*-

;; Author: vekatze <vekatze@icloud.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; URL: https://github.com/vekatze/neut-mode
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A major mode for the Neut programming language.

;;; Code:

;; ----------------------
;; calculate indentations
;; ----------------------

(defvar neut-mode-indent-offset 2)

(defun neut--get-offset-from-eol ()
  "Calculate the offset from eol."
  (- (line-end-position) (point)))

(defun neut-mode-indent-line ()
  "Indent current line."
  (interactive)
  (let ((original-offset-from-eol (neut--get-offset-from-eol)))
    (indent-line-to (neut--calculate-indentation))
    (when (< original-offset-from-eol (neut--get-offset-from-eol))
      (goto-char (- (line-end-position) original-offset-from-eol)))))

(defun neut--calculate-indentation ()
  "Calculate the (new) indentation of current line.

The indentation of a line can be calculated as follows:

  (*1) Find the shallowest part(s) of a line and go there
  (*2) Backtrack and find the nearest encloser
  (*3) (indentation) = (the indentation of the nearest encloser) + offset

Tips:

  1. The indentation of the root encloser is (-1) * base-offset
  2. Think of let .. in as a parenthesis pair like (..)"
  (if (or (neut--in-string-p (point))
          (neut--in-string-p (line-beginning-position)))
      (neut--get-indentation-of (point)) ;; leave strings as they are
    (let* ((bullet-offset (neut--get-bullet-offset (point)))
           (parent-pos-or-none (save-excursion
                                 (neut--goto-indent-base-position) ;; (*1)
                                 (neut--get-parent 0))) ;; (*2)
           (parent-indentation (neut--get-indentation-of parent-pos-or-none))
           (parent-bullet-offset (* -1 (neut--get-bullet-offset parent-pos-or-none))))
      (+ parent-indentation neut-mode-indent-offset bullet-offset parent-bullet-offset)))) ;; (*3)

(defun neut--get-indentation-of (pos)
  "Get the (current) indentation of the line in which POS exists."
  (if pos
      (save-excursion
        (neut--goto-line (line-number-at-pos pos))
        (neut--get-first-char-column))
    (* -1 neut-mode-indent-offset)))

(defun neut--goto-indent-base-position ()
  "Find a starting point to calculate the indentation of current line."
  (end-of-line)
  (goto-char (neut--find-shallowest-point 0 0 (point))))

(defun neut--goto-line (line-number)
  "Go to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (- line-number 1)))

(defun neut--goto-first-char-column ()
  "Go to the first non-space character of the current line."
  (beginning-of-line)
  (skip-chars-forward " ")
  (current-column))

(defun neut--get-first-char-column ()
  "Calculate the first column of the first non-space character of the current line."
  (save-excursion (neut--goto-first-char-column)))

(defun neut--in-string-p (pos)
  "Return non-nil value iff POS is inside a string."
  (nth 3 (syntax-ppss pos)))

(defun neut--skip-comment (start)
  "Read backward from START until all the line comments are skipped."
  (interactive)
  (when (re-search-backward "//" (line-beginning-position) t)
    (let ((pos (match-beginning 0)))
      (if (neut--in-string-p pos)
          (goto-char start)
        (neut--skip-comment pos)))))

(defun neut--find-shallowest-point (shallowness shallowest shallowest-pos)
  "Read backward from the eol to find (one of) the shallowest point of the line.

The shallowness of a point is evaluated by SHALLOWNESS.  This value is
incremented when an open paren is found, and decremented when a closing paren
is found.

The argument SHALLOWEST and SHALLOWEST-POS tracks the shallowest point
so far.

The SHALLOWEST-POS will be returned as the result of this function."
  (let ((char (preceding-char)))
    (cond
     ((= char 0)
      shallowest-pos)
     ((= char ?\n)
      shallowest-pos)
     ((= char ? )
      (goto-char (- (point) 1))
      (neut--find-shallowest-point shallowness shallowest shallowest-pos))
     ;; found an opening paren
     ((neut--opening-paren-p char)
      (goto-char (- (point) 1))
      (neut--backtrack-opening-paren shallowness shallowest shallowest-pos))
     ;; found a closing paren
     ((neut--closing-paren-p char)
      (goto-char (- (point) 1))
      (neut--backtrack-closing-paren shallowness shallowest shallowest-pos))
     ;; opening angle
     ((neut--opening-angle-p)
      (goto-char (- (point) 1))
      (neut--backtrack-opening-paren shallowness shallowest shallowest-pos))
     ;; closing angle angle
     ((neut--genuine-closing-angle-p)
      (goto-char (- (point) 1))
      (neut--backtrack-closing-paren shallowness shallowest shallowest-pos))
     ((neut--closing-angle-p)
      (goto-char (- (point) 1))
      (neut--find-shallowest-point shallowness shallowest shallowest-pos))
     (t
      (let ((token (neut--get-token (point))))
        (cond
         ;; found a opening paren ("let")
         ((neut--open-token-p token)
          (neut--backtrack-opening-paren shallowness shallowest shallowest-pos))
         ;; found a closing paren ("in")
         ((string= token "in")
          (neut--backtrack-closing-paren shallowness shallowest shallowest-pos))
         ((string= token "")
          shallowest-pos)
         (t
          (neut--find-shallowest-point shallowness shallowest shallowest-pos))))))))

(defun neut--backtrack-opening-paren (shallowness shallowest shallowest-pos)
  "After updating SHALLOWEST if necessary, continue reading backwards.

The SHALLOWEST must be updated if current SHALLOWNESS is bigger than
SHALLOWEST.  In such a case, SHALLOWEST-POS is also updated to the current
position of the cursor.

This function can be called only when the cursor is just before an opening
parenthesis, like: foo<CURSOR>(bar buz)."
  (let ((next-shallowness (+ shallowness 1)))
    (if (> next-shallowness shallowest)
        (neut--find-shallowest-point next-shallowness next-shallowness (point))
      (neut--find-shallowest-point next-shallowness shallowest shallowest-pos))))

(defun neut--backtrack-closing-paren (shallowness shallowest shallowest-pos)
  "After updating SHALLOWEST if necessary, continue reading backwards.

The SHALLOWEST must be updated if current SHALLOWNESS is bigger than
SHALLOWEST.  In such a case, SHALLOWEST-POS is also updated to the current
position of the cursor.

This function can be called only when the cursor is just after an opening
parenthesis, like: foo(bar buz)<CURSOR>."
  (neut--find-shallowest-point (- shallowness 1) shallowest shallowest-pos))

(defun neut--get-bullet-offset (point-or-none)
  "Return -2 if the line of POINT-OR-NONE begins from '| '.
Otherwise, return 0."
  (if (not point-or-none)
      0
    (save-excursion
      (goto-char point-or-none)
      (beginning-of-line)
      (skip-chars-forward " ")
      (if (re-search-forward "| " (+ (point) 2) t)
          (* -1 neut-mode-indent-offset)
        0))))

(defun neut--get-parent (nest-level)
  "Find the nearest encloser of current point by backtracking.
Return nil if the encloser is the file itself.

The NEST-LEVEL tracks the nest level of let .. in.
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
      (neut--get-parent nest-level))
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
            (neut--get-parent (- nest-level 1)))))
         ((neut--close-token-p token)
          (neut--get-parent (+ nest-level 1)))
         (t
          (neut--get-parent nest-level))))))))

(defun neut--get-token (initial-position)
  "Read backward from INITIAL-POSITION and get a token."
  (let ((char (preceding-char)))
    (cond
     ((bolp)
      (buffer-substring-no-properties (point) initial-position))
     ((neut--non-token-p char)
      (buffer-substring-no-properties (point) initial-position))
     (t
      (goto-char (- (point) 1))
      (neut--get-token initial-position)))))

(defun neut--closing-angle-p ()
  "Return non-nil iff the character on the cursor is >."
  (let ((char (preceding-char)))
    (equal char ?>)))

(defun neut--genuine-closing-angle-p ()
  "Return non-nil iff the cursor is on >, and not on -> or =>."
  (let ((char (preceding-char))
        (str (buffer-substring-no-properties (- (point) 2) (point))))
    (and (equal char ?>)
         (not (equal str "->"))
         (not (equal str "=>")))))

(defun neut--opening-angle-p ()
  "Return non-nil iff the character on the cursor is <."
  (let ((char (preceding-char)))
    (equal char ?<)))

(defun neut--make-hash-table (chars)
  "Create a new hash table using a list of characters CHARS."
  (let ((table (make-hash-table :test #'equal)))
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
  (neut--make-hash-table (list "let" "tie" "try" "bind" "pin" "use" "<" "letbox" "letbox-T" "catch")))
(defconst neut--closing-token-set
  (neut--make-hash-table (list "in" ">")))
(defun neut--opening-paren-p (char)
  "Return non-nil iff CHAR is an opening paren."
  (gethash char neut--opening-paren-char-set))
(defun neut--closing-paren-p (char)
  "Return non-nil iff CHAR is a closing paren."
  (gethash char neut--closing-paren-char-set))
(defun neut--skip-p (char)
  "Return non-nil iff CHAR must be skipped during backtracking."
  (gethash char neut--skip-char-set))
(defun neut--newline-p (char)
  "Return non-nil iff CHAR is a newline."
  (gethash char neut--newline-char-set))
(defun neut--double-quote-p (char)
  "Return non-nil iff CHAR is a double quote."
  (gethash char neut--double-quote-char-set))
(defun neut--non-token-p (char)
  "Return non-nil iff CHAR is a non-token character."
  (gethash char neut--non-token-char-set))
(defun neut--open-token-p (char)
  "Return non-nil iff CHAR is an opening token."
  (gethash char neut--opening-token-set))
(defun neut--close-token-p (char)
  "Return non-nil iff CHAR is a closing token."
  (gethash char neut--closing-token-set))

(defun neut--electric-indent-p (_)
  "Return non-nil if the current line should be indented now.

Intended to be used with `electric-indent-functions'."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "[:space:]")
    (looking-at "in")))

;; -----
;; utils
;; -----

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

;; ---------------------
;; define the major mode
;; ---------------------

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
             (,(regexp-opt '("thread" "type" "meta" "rune") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("attach" "bind" "box" "case" "catch" "constant" "data" "default" "define" "detach" "do" "else" "else-if" "exact" "external" "foreign" "function" "if" "import" "in" "inline" "introspect" "let" "letbox" "letbox-T" "match" "nominal" "of" "on" "pin" "quote" "resource" "tie" "try" "use" "when" "with") 'symbols)
              . font-lock-keyword-face)
             (,(regexp-opt '("-" "->" ":" "=" "=>" "_") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("assert" "magic" "include-text" "asset" "static") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("::") 'symbols)
              . font-lock-type-face)
             (,(regexp-opt '("admit") 'symbols)
              . font-lock-warning-face)
             (,(regexp-opt '("this") 'symbols)
              . font-lock-constant-face)
             ("\\<define\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<inline\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-function-name-face))
             ("\\<data\\> +\\([^[:space:]\s(\s)]+?\\)[ \n\s(\s)]"
              . (1 font-lock-function-name-face))
             ("\\<constant\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
              . (1 font-lock-constant-face))
             ("\\<resource\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ \n\s{(<\\[\s)}>]"
              . (1 font-lock-constant-face))
             ("\\<nominal\\> +\\([^[:space:]\s({<\s)}>]+?\\)[ :\n\s{(<\\[\s)}>]"
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
