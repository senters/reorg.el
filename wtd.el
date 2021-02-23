;; functions for writing things down in org-mode

(defvar project-dir "/home/rplevy/prj/wtd-personal/")

(defvar time-format "%Y%m%d%H%M%S")

(defun wtd-timestamp ()
  (format-time-string time-format))

(defun wtd-timestamp-yesterday ()
  (pcase-let ((`(,s_ ,m_ ,h_ ,d ,m ,y ,dow_ ,dst_ ,utc_)
               (decode-time (time-subtract (current-time) (* 24 3600)))))
    (format-time-string time-format
                        (apply #'encode-time
                               (list 0 0 0 d m y)))))

(defun wtd-insert-timestamp ()
  (interactive)
  (insert (concat (wtd-timestamp))))

(defun wtd-insert-timestamp-yesterday ()
  (interactive)
  (insert (concat (wtd-timestamp-yesterday))))

(defun wtd-identifier ()
  (concat "<<" (eshell-user-name) "-" (wtd-timestamp) ">>"))

(defun wtd-insert-identifier ()
  (interactive)
  (insert (wtd-identifier)))

(defun wtd-journal-transaction ()
  (interactive)
  (other-window 1)
  (switch-to-buffer "journal.org")
  (beginning-of-buffer)
  (next-line)
  (insert (concat "** " (wtd-identifier) " \n\n"))
  (previous-line)
  (previous-line)
  (end-of-line))

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

(defmacro wtd-def-step (instrument-step-fn instrument-step &optional comma timestamp)
  `(defun ,instrument-step-fn ()
     (interactive)
     (insert (concat "+ step " ,instrument-step " "
                     (or ,timestamp (wtd-timestamp))
                     (if ,comma ", " "")))))
