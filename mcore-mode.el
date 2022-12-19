;;; mcore-mode.el -*- lexical-binding: t -*-

(require 'pcase)
(require 'seq)
(require 'treesit nil 'noerror)

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep these lists sorted
(defvar mcore--builtin-types
  '("()"
    "Int"
    "Float"
    "Bool"
    "Char"
    "String"
    "Tensor"
    ))

(defvar mcore--special-types
  '("Unknown"))

(defvar mcore--base-keywords
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

(defvar mcore--special-keywords
  '(
    "include"
    "mexpr"
    ))

(defvar mcore--extra-keywords
  '(
    "hole"
    "independent"
    "accelerate"
    "loop"
    ))

(defvar mcore--constants
  '(
    "false"
    "true"
    "()"
    ))

(defvar mcore--special-constants
  '(
    "never"
    ))

(defvar mcore--builtin-functions
  '(
    ;; NOTE(aathn, 2022-12-15): More builtin functions can be added here
    "error"
    ))

(defvar mcore-font-lock-keywords
  (let* ((mcore-keywords
          (append
           mcore--base-keywords
           mcore--extra-keywords))
         (mcore-warning
          (append
           mcore--special-keywords
           mcore--special-constants
           mcore--special-types))

         (mcore-keywords-regexp (regexp-opt mcore-keywords 'symbols))
         (mcore-builtin-functions-regexp (regexp-opt mcore--builtin-functions 'symbols))
         (mcore-constants-regexp (regexp-opt mcore--constants 'symbols))
         (mcore-builtin-types-regexp (regexp-opt mcore--builtin-types 'symbols))
         (mcore-warning-regexp (regexp-opt mcore-warning 'symbols))

         (mcore-types-regexp "\\_<[[:upper:]][[:word:]]*\\_>"))
    `(
      (,mcore-keywords-regexp   . font-lock-keyword-face)
      (,mcore-constants-regexp  . font-lock-constant-face)
      (,mcore-builtin-types-regexp . font-lock-type-face)
      (,mcore-builtin-functions-regexp  . font-lock-builtin-face)
      (,mcore-types-regexp      . font-lock-type-face)
      (,mcore-warning-regexp     . font-lock-warning-face)
      ))
  "List of font lock keyword specifications to use in `mcore-mode'.")

(defvar mcore-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Inline comment "-- ..."
    ;; Block comment "/- ... -/"
    (modify-syntax-entry ?- ". 123" table)
    (modify-syntax-entry ?/ ". 14cn" table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for `mcore-mode'.")

(defvar mcore--treesit-font-lock-settings
  (when (featurep 'treesit)
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
         ,(concat "^\\(" (string-join mcore--builtin-functions "\\|") "\\)$")
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
     `([,@mcore--base-keywords] @font-lock-keyword-face
       [,@mcore--special-keywords] @font-lock-warning-face
       (
        (var_ident) @font-lock-keyword-face
        (:match
         ,(concat "^\\(" (string-join mcore--extra-keywords "\\|") "\\)$")
         @font-lock-keyword-face)
        ))

     :language 'mlang
     :feature 'punctuation
     '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face
       ["=" ";" ":" "->" "." "," "&" "|" "!" "+" "++"] @font-lock-delimiter-face)

     :language 'mlang
     :feature 'comment
     '([(line_comment) (block_comment)] @font-lock-comment-face)))
  "Tree-sitter font-lock settings for `mcore-ts-mode'.")

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

(defvar mcore-indent-level 2
  "Number of spaces for each indentation step in `mcore-mode'")

(defvar mcore--treesit-indent-rules
  (let ()
    `((mlang
       ((node-is "}") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ",") parent 0)
       ((parent-is "sequencing_expr") parent 0)
       ((parent-is "or_pat") parent -2)
       ((parent-is "funapp_expr") parent mcore-indent-level)
       ((query "(_ [\"(\" \"{\" \"[\"] . _ _ @match)") (nth-sibling 1) 0)
       ((query "(_ \"in\" _ @match)") parent 0)
       ((query "[\"in\" \"then\" \"else\" \"end\"] @match") parent-bol 0)
       ((parent-is "sem_decl") parent-bol 0)
       ((parent-is "syn_decl") parent-bol 0)
       ((and (parent-is "block_comment") comment-end) comment-start -1)
       ((parent-is "block_comment") comment-start-skip 0)
       ((parent-is "source_file") parent-bol 0)
       (catch-all parent-bol mcore-indent-level)
       )))
  "Tree-sitter indentation settings for `mcore-ts-mode'")

;;;;;;;;;;;;;;
;; Prettify ;;
;;;;;;;;;;;;;;

(defvar mcore-prettify-symbols-alist
  '(("lam" . ?λ)
    ("all" . ?∀)
    ("->" . ?→))
  "List of syntax to prettify for `mcore-mode'.")

;;;;;;;;;;;;;;;;;
;; Compilation ;;
;;;;;;;;;;;;;;;;;

(defun mcore--setup-compile ()
  ;; Set default compile command
  (set (make-local-variable 'compile-command)
       (concat "mi " (buffer-name)))
  ;; Get location of standard library from environment
  (let ((path
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $MCORE_STDLIB'"))))
    (if (> (length path) 0)
        (set (make-local-variable 'compilation-environment)
             (list (concat "MCORE_STDLIB=" path))))))

(let ((mcore-error-regexp
       '(mcore "\"\\(.+\\)\" \\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
  (add-to-list 'compilation-error-regexp-alist-alist mcore-error-regexp)
  (add-to-list 'compilation-error-regexp-alist 'mcore))

;;;;;;;;;;;
;; Imenu ;;
;;;;;;;;;;;

(defvar mcore--imenu-generic-expression
  '(("Lets" "^let\\s-+\\([_A-Za-z0-9+]+\\)" 1)
    ("Types" "^\\s-*\\(type\\|con\\)\\s-+\\([_A-Za-z0-9+]+\\)" 2)
    ("Langs" "^\\s-*\\(lang\\|sem\\|syn\\)\\s-+\\([_A-Za-z0-9+]+\\)" 2))
  "A list of regexps for identifying mcore definitions.")

(defun mcore--treesit-imenu-index-function ()
  "Query tree-sitter for mcore definitions."
  (seq-filter
   #'cdr
   (list
    ;; Make a menu Langs, with a submenu for each language fragment in the file
    ;; containing the sems, syns and types of that fragment.
    `("Langs"
      .
      ,(mapcar
        (pcase-lambda (`(_ . ,node))
          (let ((lang-name
                 (cdar (treesit-query-capture node '((lang_ident) @match)))))
            `(,(treesit-node-text lang-name t) .
              ((,(treesit-node-text lang-name t) . ,(treesit-node-start lang-name))
               ,@(mapcar
                  (pcase-lambda (`(_ . ,node))
                    `(,(treesit-node-text node t) . ,(treesit-node-start node)))
                  (treesit-query-capture
                   node
                   '((sem_decl (fun_ident) @match)
                     (sem_type (fun_ident) @match)
                     (type_decl (type_ident) @match)
                     (syn_decl (type_ident) @match))))))))
        (treesit-query-capture
         'mlang '((mlang) @match))))
    ;; Make a menu Types, with a submenu for each top-level type in the file
    ;; containing the constructors of that type.
    `("Types"
      .
      ,(mapcar
        (pcase-lambda (`(_ . ,node))
          `(,(treesit-node-text node t)
            .
            ((,(treesit-node-text node t) . ,(treesit-node-start node))
             ,@(thread-last
                 (treesit-query-capture
                  'mlang
                  `((con_bind (con_ident) @match
                              (con_type
                               "->" (type_ident) @type
                               (:match ,(treesit-node-text node t) @type)))))
                 (seq-filter (pcase-lambda (`(,capture . _)) (eq capture 'match)))
                 (mapcar
                  (pcase-lambda (`(_ . ,node))
                    `(,(treesit-node-text node t) . ,(treesit-node-start node))))))))
        (treesit-query-capture
         'mlang '((type_bind (type_ident) @match)))))
    ;; Make a menu Lets containing all top-level (MLang) lets in the file.
    `("Lets"
      .
      ,(mapcar
        (pcase-lambda (`(_ . ,node))
          `(,(treesit-node-text node t) . ,(treesit-node-start node)))
        (treesit-query-capture
         'mlang '((source_file (let_bind (var_ident) @match)))))))))

;;;;;;;;;;;;;;;;;;;;;
;; Mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode mcore-base-mode prog-mode "mcore"
  "Generic mode for editing MCore files."
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(--\\|/-\\)\\s-*")
  (setq-local comment-end-skip "\\s-*\\(\\s>\\|-/\\)")
  (setq-local prettify-symbols-alist mcore-prettify-symbols-alist)
  (mcore--setup-compile))

;;;###autoload
(define-derived-mode mcore-mode mcore-base-mode "mcore"
  "Major mode for editing MCore files."
  (setq-local font-lock-defaults '(mcore-font-lock-keywords))
  (setq-local imenu-generic-expression mcore--imenu-generic-expression))

;;;###autoload
(define-derived-mode mcore-ts-mode mcore-base-mode "mcore"
  "Major mode for editing MCore files, powered by tree-sitter."
  :syntax-table mcore-mode-syntax-table
  (when (and (featurep 'treesit)
             (treesit-ready-p 'mlang))

    ;; Indentation
    (setq-local treesit-simple-indent-rules mcore--treesit-indent-rules)

    ;; Highlighting
    (setq-local treesit-font-lock-settings mcore--treesit-font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment punctuation)
                  (keyword type builtin constant)
                  (function-name variable-name)
                  (pattern-name label-name)))

    (setq-local imenu-create-index-function #'mcore--treesit-imenu-index-function)
    (setq-local treesit-defun-type-regexp "sem_decl\\|let_bind")
    (treesit-major-mode-setup)))

;; Open “*.mc” in mcore-mode
(add-to-list 'auto-mode-alist '("\\.mc\\'" . mcore-mode))

(provide 'mcore-mode)
;;; mcore-mode.el ends here
