;;; mcore-mode.el

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

(setq mcore-builtin-types
      '("()"
        "Int"
        "Float"
        "Bool"
        "Char"
        "String"
        "Tensor"
        ))

(setq mcore-special-types
      '("Unknown"))

;; Please keep this list sorted
(setq mcore-base-keywords
      '(
        "all"
        "case"
        "con"
        "else"
        "end"
        "external"
        "if"
        "in"
        "lam"
        "lang"
        "let"
        "match"
        "recursive"
        "sem"
        "switch"
        "syn"
        "then"
        "type"
        "use"
        "using"
        "utest"
        "with"
        ))

(setq mcore-special-keywords
      '(
        "include"
        "mexpr"
        ))

(setq mcore-extra-keywords
      '(
        "hole"
        "independent"
        "accelerate"
        "loop"
        ))

(setq mcore-constants
      '(
        "false"
        "true"
        "()"
        ))

(setq mcore-special-constants
      '(
        "never"
        ))

(setq mcore-builtin-functions
      '(
        ;; More could be added
        "error"
        ))

(setq mcore-keywords
      (append
       mcore-base-keywords
       mcore-extra-keywords))

(setq mcore-warning
      (append
       mcore-special-keywords
       mcore-special-constants
       mcore-special-types))

(setq mcore-keywords-regexp (regexp-opt mcore-keywords 'symbols))
(setq mcore-builtin-functions-regexp (regexp-opt mcore-builtin-functions 'symbols))
(setq mcore-constants-regexp (regexp-opt mcore-constants 'symbols))
(setq mcore-builtin-types-regexp (regexp-opt mcore-builtin-types 'symbols))
(setq mcore-warning-regexp (regexp-opt mcore-warning 'symbols))

(setq mcore-types-regexp "\\_<[[:upper:]][[:word:]]*\\_>")

(setq mcore-font-lock-keywords
     `(
       (,mcore-keywords-regexp   . font-lock-keyword-face)
       (,mcore-constants-regexp  . font-lock-constant-face)
       (,mcore-builtin-types-regexp . font-lock-type-face)
       (,mcore-builtin-functions-regexp  . font-lock-builtin-face)
       (,mcore-types-regexp      . font-lock-type-face)
       (,mcore-warning-regexp     . font-lock-warning-face)
       )
     )

(defvar mcore-mode-syntax-table nil "Syntax table for `mcore-mode'.")

(setq mcore-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Inline comment "-- ..."
        ;; Block comment "/- ... -/"
        (modify-syntax-entry ?- ". 123" table)
        (modify-syntax-entry ?/ ". 14cn" table)
        (modify-syntax-entry ?\n "> " table)
        (modify-syntax-entry ?' "\"" table)
        table))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter support ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mcore--treesit-font-lock-settings
  (when (fboundp 'treesit-font-lock-rules)
    (treesit-font-lock-rules
     :language 'mlang
     :feature 'type
     '((type_ident) @font-lock-type-face
       (type_param) @font-lock-variable-name-face

       [(unit_type)
        (bool_type)
        (int_type)
        (float_type)
        (char_type)
        (string_type)
        "Tensor"]
       @font-lock-type-face

       (unknown_type) @font-lock-warning-face

       [(lang_ident) (con_ident)] @font-lock-type-face)

     :language 'mlang
     :feature 'function-name
     '((fun_ident) @font-lock-function-name-face
       (let_bind (var_ident) @font-lock-function-name-face (lam_expr))
       (funapp_expr :anchor (var_ident) @font-lock-function-name-face))

     :language 'mlang
     :feature 'variable-name
     '((fun_param) @font-lock-variable-name-face)

     :language 'mlang
     :feature 'builtin
     `((
        (var_ident) @font-lock-builtin-face
        (:match
         ,(concat "^\\(" (string-join mcore-builtin-functions "\\|") "\\)$")
         @font-lock-builtin-face)
        ))

     :language 'mlang
     :feature 'pattern-name
     '((name_pat) @font-lock-variable-name-face)

     :language 'mlang
     :feature 'label-name
     '((label_ident) @font-lock-property-face
       (proj_expr "." :anchor (uint) @font-lock-property-face))

     :language 'mlang
     :feature 'constant
     :override t
     '([(uint) (ufloat)] @font-lock-number-face

       (escape_sequence) @font-lock-escape-face
       (character) @font-lock-string-face
       (string) @font-lock-string-face

       [(unit)
        (bool)]
       @font-lock-constant-face

       (never) @font-lock-warning-face)

     :language 'mlang
     :feature 'keyword
     `([,@mcore-base-keywords] @font-lock-keyword-face
       [,@mcore-special-keywords] @font-lock-warning-face
       (
        (var_ident) @font-lock-keyword-face
        (:match
         ,(concat "^\\(" (string-join mcore-extra-keywords "\\|") "\\)$")
         @font-lock-keyword-face)
        ))

     :language 'mlang
     :feature 'punctuation
     '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face
       ["=" ";" ":" "->" "." "," "&" "|" "!" "+" "++"] @font-lock-delimiter-face)

     :language 'mlang
     :feature 'comment
     '([(line_comment) (block_comment)] @font-lock-comment-face)))
  "Tree-sitter font-lock settings for `mcore-mode'.")

;;;;;;;;;;;;;;
;; prettify ;;
;;;;;;;;;;;;;;

(defvar mcore-prettify-symbols-alist
  '(("lam" . 955)                      ; λ
    ("all" . 8704)                     ; ∀
    ("->" . 8594))                     ; →
  "List of syntax to prettify for `mcore-mode'.")

(if (boundp 'prettify-symbols-alist)
    (add-hook 'mcore-mode-hook
              (lambda ()
                (mapc (lambda (pair) (push pair prettify-symbols-alist))
                      mcore-prettify-symbols-alist))))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(add-hook 'mcore-mode-hook
          (lambda ()
            ;; Set default compile command
            (progn
              (set (make-local-variable 'compile-command)
                   (concat "mi " (buffer-name)))
              ;; Get location of standard library from environment
              (let ((path
                     (replace-regexp-in-string
                      "[[:space:]\n]*$" ""
                      (shell-command-to-string "$SHELL -l -c 'echo $MCORE_STDLIB'"))))
                (if (> (length path) 0)
                  (set (make-local-variable 'compilation-environment)
                       (list (concat "MCORE_STDLIB=" path))))))))

(setq mcore-error-regexp
      '(mcore "\"\\(.+\\)\" \\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist mcore-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'mcore)))

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode mcore-mode prog-mode "mcore"
  "Major mode for editing Miking MCore code."
  (setq-local font-lock-defaults '(mcore-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")

  (when (and (fboundp 'treesit-ready-p)
             (treesit-ready-p 'mlang))
    (setq-local treesit-font-lock-settings mcore--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment punctuation)
                  (keyword type builtin constant)
                  (function-name variable-name)
                  (pattern-name label-name)))
    (treesit-major-mode-setup)))

;; Open “*.mc” in mcore-mode
(add-to-list 'auto-mode-alist '("\\.mc\\'" . mcore-mode))

(provide 'mcore-mode)
;;; mcore-mode.el ends here
