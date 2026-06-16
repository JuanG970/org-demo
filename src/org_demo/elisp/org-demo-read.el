;;; org-demo-read.el --- Parse a demo document and return its entries as JSON  -*- lexical-binding: t; -*-

;; Reads an org-mode demo file and returns a JSON array of entries.
;; Each entry is an object with a "type" field ("note", "exec", "image", "call")
;; plus type-specific fields.
;; exec entries include: lang, code, output, name (opt), header_args (opt)
;; call entries include: name, arguments (opt), inside_header (opt), end_header (opt), output
;; Parameters: {{file}}

(progn
  (require 'json)
  (require 'org)
  (let* ((file (expand-file-name "{{file}}"))
         (entries '()))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (let ((title "")
            (uuid "")
            (date ""))
        (when (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
          (setq title (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "^#\\+PROPERTY:\\s-*org-demo-id\\s-+\\(.*\\)$" nil t)
          (setq uuid (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "^#\\+DATE:\\s-*\\(.*\\)$" nil t)
          (setq date (match-string 1)))

        (goto-char (point-min))
        (while (and (not (eobp))
                    (looking-at-p "^#\\+"))
          (forward-line 1))
        (while (and (not (eobp)) (looking-at-p "^$"))
          (forward-line 1))

        (while (not (eobp))
          (cond
           ;; Source block — optionally preceded by #+NAME:
           ((or (looking-at "^#\\+NAME:\\s-*\\(.*\\)$")
                (looking-at "^#\\+BEGIN_SRC\\s-+\\(\\S-+\\)\\(.*\\)$"))
            (let ((block-name "")
                  (lang "")
                  (header-args "")
                  (code "")
                  (output ""))
              ;; Check if current line is #+NAME:
              (when (looking-at "^#\\+NAME:\\s-*\\(.*\\)$")
                (setq block-name (string-trim (match-string 1)))
                (forward-line 1))
              ;; Now we should be on #+BEGIN_SRC
              (if (looking-at "^#\\+BEGIN_SRC\\s-+\\(\\S-+\\)\\(.*\\)$")
                  (progn
                    (setq lang (match-string 1))
                    (setq header-args (string-trim (or (match-string 2) "")))
                    (forward-line 1)
                    (let ((code-start (point)))
                      (if (re-search-forward "^#\\+END_SRC" nil t)
                          (progn
                            (beginning-of-line)
                            (setq code (buffer-substring-no-properties code-start (point)))
                            (forward-line 1))
                        (goto-char (point-max))))
                    ;; Skip blank lines between #+END_SRC and #+RESULTS:
                    (while (and (not (eobp)) (looking-at-p "^$"))
                      (forward-line 1))
                    ;; Parse results
                    (when (looking-at-p "^#\\+RESULTS")
                      (forward-line 1)
                      (cond
                       ;; EXAMPLE block format
                       ((looking-at-p "^#\\+BEGIN_EXAMPLE")
                        (forward-line 1)
                        (let ((out-start (point)))
                          (if (re-search-forward "^#\\+END_EXAMPLE" nil t)
                              (progn
                                (beginning-of-line)
                                (setq output (buffer-substring-no-properties out-start (point)))
                                (forward-line 1))
                            (goto-char (point-max)))))
                       ;; Colon-prefixed scalar output (": line")
                       ((looking-at-p "^: ")
                        (let ((lines '()))
                          (while (and (not (eobp)) (looking-at "^: \\(.*\\)$"))
                            (push (match-string 1) lines)
                            (forward-line 1))
                          (setq output (mapconcat #'identity (nreverse lines) "\n"))))))
                    ;; Clean trailing newlines
                    (when (string-suffix-p "\n" code)
                      (setq code (substring code 0 -1)))
                    (when (string-suffix-p "\n" output)
                      (setq output (substring output 0 -1)))
                    (let ((entry (list (cons "type" "exec")
                                       (cons "lang" lang)
                                       (cons "code" code)
                                       (cons "output" output))))
                      (when (and block-name (not (string-empty-p block-name)))
                        (push (cons "name" block-name) (cdr entry)))
                      (when (and header-args (not (string-empty-p header-args)))
                        (push (cons "header_args" header-args) (cdr entry)))
                      (push entry entries)))
                ;; #+NAME: line not followed by #+BEGIN_SRC — treat as note text
                (push (list (cons "type" "note")
                            (cons "text" (format "#+NAME: %s" block-name)))
                      entries))))

           ;; #+CALL: lines
           ((looking-at "^#\\+CALL:\\s-*\\(\\S-+\\)\\(?:\\[\\([^]]*\\)\\]\\)?\\(?:(\\([^)]*\\))\\)?\\(.*\\)$")
            (let ((call-name (match-string 1))
                  (inside-header (or (match-string 2) ""))
                  (arguments (or (match-string 3) ""))
                  (end-header (string-trim (or (match-string 4) "")))
                  (output ""))
              (forward-line 1)
              ;; Skip blank lines
              (while (and (not (eobp)) (looking-at-p "^$"))
                (forward-line 1))
              ;; Parse results
              (when (looking-at-p "^#\\+RESULTS:")
                (forward-line 1)
                (cond
                 ((looking-at-p "^#\\+BEGIN_EXAMPLE")
                  (forward-line 1)
                  (let ((out-start (point)))
                    (if (re-search-forward "^#\\+END_EXAMPLE" nil t)
                        (progn
                          (beginning-of-line)
                          (setq output (buffer-substring-no-properties out-start (point)))
                          (forward-line 1))
                      (goto-char (point-max)))))
                 ((looking-at-p "^: ")
                  (let ((lines '()))
                    (while (and (not (eobp)) (looking-at "^: \\(.*\\)$"))
                      (push (match-string 1) lines)
                      (forward-line 1))
                    (setq output (mapconcat #'identity (nreverse lines) "\n"))))))
              (when (string-suffix-p "\n" output)
                (setq output (substring output 0 -1)))
              (push (list (cons "type" "call")
                          (cons "name" call-name)
                          (cons "arguments" arguments)
                          (cons "inside_header" inside-header)
                          (cons "end_header" end-header)
                          (cons "output" output))
                    entries)))

           ;; Image link
           ((looking-at "^# image:")
            (forward-line 1)
            (let ((alt "")
                  (path ""))
              (when (looking-at "^\\[\\[file:\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]")
                (setq path (match-string 1))
                (setq alt (or (match-string 2) ""))
                (forward-line 1))
              (push (list (cons "type" "image")
                          (cons "path" path)
                          (cons "alt" alt))
                    entries)))

           ;; Blank lines
           ((looking-at-p "^$")
            (forward-line 1))

           ;; Plain text (notes)
           (t
            (let ((text-start (point)))
              (while (and (not (eobp))
                          (not (looking-at-p "^$"))
                          (not (looking-at-p "^#\\+"))
                          (not (looking-at-p "^# image:"))
                          (not (looking-at-p "^\\[\\[")))
                (forward-line 1))
              (let ((text (buffer-substring-no-properties text-start (point))))
                (when (string-suffix-p "\n" text)
                  (setq text (substring text 0 -1)))
                (push (list (cons "type" "note")
                            (cons "text" text))
                      entries))))))

        (let ((result (list (cons "title" title)
                            (cons "uuid" uuid)
                            (cons "date" date)
                            (cons "entries" (vconcat (nreverse entries))))))
          (json-encode result)))))))
