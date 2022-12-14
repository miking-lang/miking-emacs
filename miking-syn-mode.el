;;; miking-syn-mode.el

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep this list sorted
(setq miking-syn-keywords
      '(
        "empty"
        "except"
        "include"
        "infix"
        "language"
        "postfix"
        "precedence"
        "prefix"
        "prod"
        "start"
        "token"
        "type"
        ))

(setq miking-syn-types-regexp "\\_<[[:upper:]][[:word:]]*\\_>")

(setq miking-syn-keywords-regexp (regexp-opt miking-syn-keywords 'symbols))
(setq miking-syn-font-lock-keywords
     `(
       (,miking-syn-keywords-regexp   . font-lock-keyword-face)
       (,miking-syn-types-regexp      . font-lock-type-face)
       )
     )

(defvar miking-syn-mode-syntax-table nil "Syntax table for `miking-syn-mode'.")

(setq miking-syn-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Inline comment "-- ..."
        ;; Block comment "/- ... -/"
        (modify-syntax-entry ?- ". 123" table)
        (modify-syntax-entry ?/ ". 14cn" table)
        (modify-syntax-entry ?\n "> " table)
        (modify-syntax-entry ?' "\"" table)
        table))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(add-hook 'miking-syn-mode-hook
          (lambda ()
            ;; Set default compile command
            (progn
              (set (make-local-variable 'compile-command)
                   (concat "mi syn " (buffer-name) " " (file-name-sans-extension (buffer-name)) "_gen.mc")))))

(setq miking-syn-error-regexp
      '(miking-syn "\"\\(.+\\)\" \\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist miking-syn-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'miking-syn)))

;; Display ansi-colors
; from https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode miking-syn-mode prog-mode "miking-syn"
  "Major mode for editing Miking miking-syn code."
  (setq-local font-lock-defaults '(miking-syn-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")
)

;; Open “*.syn” in miking-syn-mode
(add-to-list 'auto-mode-alist '("\\.syn\\'" . miking-syn-mode))

(provide 'miking-syn-mode)
;;; miking-syn-mode.el ends here
