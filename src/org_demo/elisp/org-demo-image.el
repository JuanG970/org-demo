;;; org-demo-image.el --- Append an image reference to a demo document  -*- lexical-binding: t; -*-

;; Copies the image file to the document's directory with a generated name,
;; then appends an org-mode image link.
;; Parameters: {{file}}, {{image_path}}, {{alt_text}}, {{dest_filename}}

(let* ((file (expand-file-name "{{file}}"))
       (image-path (expand-file-name "{{image_path}}"))
       (alt-text "{{alt_text}}")
       (dest-filename "{{dest_filename}}")
       (dest-dir (file-name-directory file))
       (dest-path (expand-file-name dest-filename dest-dir)))
  ;; Copy the image
  (copy-file image-path dest-path t)
  ;; Append to org file
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    ;; Record the source command as a comment
    (insert (format "# image: %s\n" dest-filename))
    (if (string= alt-text "")
        (insert (format "[[file:%s]]\n" dest-filename))
      (insert (format "[[file:%s][%s]]\n" dest-filename alt-text)))
    (write-region (point-min) (point-max) file nil 'silent))
  (format "Copied %s -> %s" image-path dest-path))
