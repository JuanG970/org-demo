;;; org-demo-init.el --- Create a new org-mode demo document  -*- lexical-binding: t; -*-

;; Creates a new .org file with a title heading and a timestamp property.
;; Parameters: {{file}}, {{title}}, {{uuid}}

(let* ((file (expand-file-name "{{file}}"))
       (title "{{title}}")
       (uuid "{{uuid}}")
       (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
  (with-temp-buffer
    (org-mode)
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+PROPERTY: org-demo-id %s\n" uuid))
    (insert (format "#+DATE: %s\n" timestamp))
    (insert "\n")
    (write-region (point-min) (point-max) file nil 'silent))
  (format "Created %s" file))
