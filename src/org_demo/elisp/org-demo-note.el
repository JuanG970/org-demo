;;; org-demo-note.el --- Append a commentary block to a demo document  -*- lexical-binding: t; -*-

;; Appends a paragraph of commentary text to the end of the org file.
;; Parameters: {{file}}, {{text}}

(let* ((file (expand-file-name "{{file}}"))
       (text "{{text}}"))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert text)
    (insert "\n")
    (write-region (point-min) (point-max) file nil 'silent))
  "ok")
