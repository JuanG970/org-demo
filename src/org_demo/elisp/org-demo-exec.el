;;; org-demo-exec.el --- Append a source block and execute it via babel  -*- lexical-binding: t; -*-

;; Appends an org-mode source block, then calls org-babel-execute-src-block
;; which handles execution AND result insertion natively.
;; Returns the babel output as a JSON string for the CLI.
;; Parameters: {{file}}, {{lang}}, {{code}}, {{name}}, {{header_args}}

(progn
  (require 'json)
  (require 'org)
  (require 'ob)
  (let* ((file (expand-file-name "{{file}}"))
         (lang "{{lang}}")
         (code "{{code}}")
         (block-name "{{name}}")
         (header-args "{{header_args}}"))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      ;; Insert #+NAME: line if a name was provided
      (when (and block-name (not (string-empty-p block-name)))
        (insert (format "#+NAME: %s\n" block-name)))
      ;; Insert #+BEGIN_SRC with optional header arguments
      (if (and header-args (not (string-empty-p header-args)))
          (insert (format "#+BEGIN_SRC %s %s\n" lang header-args))
        (insert (format "#+BEGIN_SRC %s\n" lang)))
      (insert code)
      (unless (string-suffix-p "\n" code) (insert "\n"))
      (insert "#+END_SRC\n")
      ;; Move point back onto the src block for babel
      (re-search-backward "^#\\+BEGIN_SRC" nil t)
      ;; Execute — babel inserts results into the buffer automatically
      (let ((result (org-babel-execute-src-block)))
        (save-buffer)
        (json-encode (or (and result (format "%s" result)) ""))))))
