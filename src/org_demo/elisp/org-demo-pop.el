;;; org-demo-pop.el --- Remove the last entry from a demo document  -*- lexical-binding: t; -*-

;; Removes the most recent entry (note paragraph, src+results block, or image block)
;; from the end of the org file.
;; Parameters: {{file}}

(let* ((file (expand-file-name "{{file}}")))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (let ((end (point-max))
          (start nil))
      (cond
       ;; Case 1: Last thing is a src block with EXAMPLE results
       ((save-excursion
          (goto-char (point-max))
          (re-search-backward "^#\\+END_EXAMPLE" nil t))
        (let ((example-end (match-beginning 0)))
          (goto-char example-end)
          (if (re-search-backward "^#\\+RESULTS:" nil t)
              (let ((results-start (match-beginning 0)))
                (goto-char results-start)
                (if (re-search-backward "^#\\+BEGIN_SRC" nil t)
                    (progn
                      (beginning-of-line)
                      (when (and (> (point) (point-min))
                                 (save-excursion
                                   (forward-line -1)
                                   (looking-at-p "^$")))
                        (forward-line -1))
                      (setq start (point)))
                  (setq start results-start)))
            (goto-char example-end)
            (re-search-backward "^#\\+BEGIN_EXAMPLE" nil t)
            (beginning-of-line)
            (setq start (point)))))

       ;; Case 2: Last thing is a src block with colon-prefixed results
       ((save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (beginning-of-line)
          (looking-at-p "^: "))
        ;; Walk backwards past all ": " lines
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (beginning-of-line)
        (while (and (not (bobp)) (looking-at-p "^: "))
          (forward-line -1))
        ;; Should be on #+RESULTS: line now
        (when (looking-at-p "^#\\+RESULTS:")
          (let ((results-start (point)))
            (if (progn (forward-line -1)
                       (re-search-backward "^#\\+BEGIN_SRC" nil t))
                (progn
                  (beginning-of-line)
                  (when (and (> (point) (point-min))
                             (save-excursion
                               (forward-line -1)
                               (looking-at-p "^$")))
                    (forward-line -1))
                  (setq start (point)))
              (setq start results-start)))))

       ;; Case 3: Last thing is a src block with no results (just #+END_SRC)
       ((save-excursion
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (beginning-of-line)
          (looking-at-p "^#\\+END_SRC"))
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (if (re-search-backward "^#\\+BEGIN_SRC" nil t)
            (progn
              (beginning-of-line)
              (when (and (> (point) (point-min))
                         (save-excursion
                           (forward-line -1)
                           (looking-at-p "^$")))
                (forward-line -1))
              (setq start (point)))))

       ;; Case 4: Last thing is an image link
       ((save-excursion
          (goto-char (point-max))
          (re-search-backward "^\\[\\[file:" nil t))
        (beginning-of-line)
        (when (save-excursion
                (forward-line -1)
                (looking-at-p "^# image:"))
          (forward-line -1))
        (when (and (> (point) (point-min))
                   (save-excursion
                     (forward-line -1)
                     (looking-at-p "^$")))
          (forward-line -1))
        (setq start (point)))

       ;; Case 5: Last thing is a text note
       (t
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (end-of-line)
        (setq end (point))
        (while (and (> (point) (point-min))
                    (progn
                      (forward-line -1)
                      (not (or (looking-at-p "^$")
                               (looking-at-p "^#\\+")
                               (looking-at-p "^\\[\\[")
                               (looking-at-p "^: ")))))
          nil)
        (unless (bobp)
          (forward-line 1))
        (setq start (point))))

      (when start
        (delete-region start end)
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (unless (bobp) (forward-char 1))
        (delete-region (point) (point-max))
        (insert "\n")
        (write-region (point-min) (point-max) file nil 'silent)))
    "ok"))
