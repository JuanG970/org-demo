;;; org-demo-tangle.el --- Extract tangled source files via org-babel-tangle  -*- lexical-binding: t; -*-

;; Calls org-babel-tangle on the document to extract source code into files.
;; Returns a JSON array of the files that were written.
;; Parameters: {{file}}, {{target_dir}}

(progn
  (require 'json)
  (require 'org)
  (require 'ob-tangle)
  (let* ((file (expand-file-name "{{file}}"))
         (target-dir "{{target_dir}}"))
    (with-current-buffer (find-file-noselect file)
      (when (and target-dir (not (string-empty-p target-dir)))
        (let ((default-directory (file-name-as-directory
                                  (expand-file-name target-dir))))
          (unless (file-directory-p default-directory)
            (make-directory default-directory t))
          (let ((files (org-babel-tangle)))
            (json-encode (or files (vector))))))
      (let ((files (org-babel-tangle)))
        (json-encode (or files (vector)))))))
