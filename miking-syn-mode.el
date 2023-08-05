;;; miking-syn-mode.el -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Elias Castegren <elias.castegren@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(require 'compile)
;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep this list sorted
(defvar miking-syn--keywords
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

(defvar miking-syn--operators
      '(
        "+"
        "?"
        "*"
        "|"
        ))

(defvar miking-syn--types-regexp "\\_<[[:upper:]][[:word:]]*\\_>")
(defvar miking-syn--keywords-regexp (regexp-opt miking-syn--keywords 'symbols))
(defvar miking-syn--operators-regexp (regexp-opt miking-syn--operators 'symbols))

(defvar miking-syn-font-lock-keywords
    `(
         (,miking-syn--operators-regexp . font-lock-builtin-face)
         (,miking-syn--keywords-regexp  . font-lock-keyword-face)
         (,miking-syn--types-regexp     . font-lock-type-face)
         )
    "List of font lock keyword specifications to use in `miking-syn-mode'.")

(defvar miking-syn-mode-syntax-table
    (let ((table (make-syntax-table)))
        ;; Inline comment "-- ..."
        ;; Block comment "/- ... -/"
        (modify-syntax-entry ?- ". 123" table)
        (modify-syntax-entry ?/ ". 14cn" table)
        (modify-syntax-entry ?\n "> " table)
        (modify-syntax-entry ?' "\"" table)
        table)
    "Syntax table for `miking-syn-mode'.")

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

;;;###autoload
(define-derived-mode miking-syn-mode prog-mode "miking-syn"
  "Major mode for editing Miking miking-syn code."
  (setq-local font-lock-defaults '(miking-syn-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")
)

;; Open “*.syn” in miking-syn-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.syn\\'" . miking-syn-mode))

(provide 'miking-syn-mode)
;;; miking-syn-mode.el ends here
