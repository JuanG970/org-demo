;;; org-demo-verify.el --- Re-execute all source blocks via babel  -*- lexical-binding: t; -*-

;; Opens the org file and calls org-babel-execute-buffer to re-run
;; all source blocks. Babel updates results in place.
;; Parameters: {{file}}

(progn
  (require 'org)
  (require 'ob)
  (with-current-buffer (find-file-noselect (expand-file-name "{{file}}"))
    (org-babel-execute-buffer)
    (save-buffer)
    "ok"))
