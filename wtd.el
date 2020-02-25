;; functions for writing things down in org-mode

(defvar project-dir "/home/rplevy/prj/wtd-personal/")

(defun wtd-timestamp ()
  (format-time-string "%Y%m%d%H%M%S"))

(defun wtd-insert-timestamp ()
  (interactive)
  (insert (concat (wtd-timestamp) ", ")))

(defun wtd-identifier ()
  (concat "<<" (eshell-user-name) "-" (wtd-timestamp) ">>"))

(defun wtd-insert-identifier ()
  (interactive)
  (insert (wtd-identifier)))

(defun wtd-journal-transaction ()
  (interactive)
  (beginning-of-line)
  (insert (concat "** " (wtd-identifier))))

(defun wtd-identifer-to-link (&optional identifier title)
  (interactive)
  (concat "["
          "[file:" (replace-regexp-in-string project-dir "" (buffer-file-name))
          "::" (or identifier (buffer-substring (region-beginning) (region-end)))
          "]"
          (if title (concat "[" title "]") "")
          "]"))

(defun wtd-copy-id-as-link ()
  (interactive)
  (kill-new (wtd-identifer-to-link)))

(defun wtd-transaction-to-link ()
  (interactive)
  (outline-mark-subtree)
  (let* ((m          (buffer-substring (region-beginning) (region-end)))
         (m1         (car (split-string m "\n")))
         (identifier (replace-regexp-in-string "^\*+ <<\\\(.*\\\)>> .*" "\\1" m1))
         (title      (replace-regexp-in-string "^\*+ <<.*>> \\\(.*\\\)" "\\1" m1)))
    (wtd-identifer-to-link (org-trim identifier) (org-trim title))))

(defun wtd-copy-transaction-link ()
  (interactive)
  (kill-new (wtd-transaction-to-link)))
