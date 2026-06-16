;;; org-demo-call.el --- Append a #+CALL: line and execute it via babel  -*- lexical-binding: t; -*-

;; Appends a #+CALL: line to invoke a named source block, then
;; executes it via org-babel-lob-execute.
;; Returns the babel output as a JSON string for the CLI.
;; Parameters: {{file}}, {{name}}, {{arguments}}, {{inside_header}}, {{end_header}}

(progn
  (require 'json)
  (require 'org)
  (require 'ob)
  (require 'ob-lob)
  (let* ((file (expand-file-name "{{file}}"))
         (call-name "{{name}}")
         (arguments "{{arguments}}")
         (inside-header "{{inside_header}}")
         (end-header "{{end_header}}"))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      ;; Build #+CALL: line
      ;; Full syntax: #+CALL: name[inside-header](arguments) end-header
      (let ((call-line (concat "#+CALL: " call-name)))
        (when (and inside-header (not (string-empty-p inside-header)))
          (setq call-line (concat call-line "[" inside-header "]")))
        (when (and arguments (not (string-empty-p arguments)))
          (setq call-line (concat call-line "(" arguments ")")))
        (when (and end-header (not (string-empty-p end-header)))
          (setq call-line (concat call-line " " end-header)))
        (insert call-line "\n"))
      ;; Move point back onto the #+CALL: line
      (forward-line -1)
      ;; Execute the call via babel
      (let ((result (org-babel-lob-execute
                     (org-babel-lob-get-info))))
        (save-buffer)
        (json-encode (or (and result (format "%s" result)) ""))))))
