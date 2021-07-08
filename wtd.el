(defvar project-dir "prj/wtd/")

(defvar identity-dir "prj/wtd-personal/")

(defvar server-dir "prj/senters/")

(defun load-project-file (f)
  (load-file (concat "$HOME/" project-dir f)))

(load-project-file "dependencies.el")
(load-project-file "utils.el")

(defvar time-format "%Y%m%d%H%M%S")

(defun wtd-timestamp ()
  (format-time-string time-format))

(defun wtd-timestamp-for-day (&optional f)
  (pcase-let ((`(,s_ ,m_ ,h_ ,d ,m ,y ,dow_ ,dst_ ,utc_)
               (decode-time (funcall f (current-time)))))
    (format-time-string time-format
                        (apply #'encode-time
                               (list 0 0 0 d m y)))))

(defun wtd-timestamp-yesterday ()
  (wtd-timestamp-for-day
   (lambda (time)
     (time-subtract time (* 24 3600)))))

(defun wtd-timestamp-today ()
  (wtd-timestamp-for-day
   (lambda (time) time)))

(defun wtd-insert-timestamp ()
  (interactive)
  (insert (wtd-timestamp)))

(defun wtd-insert-timestamp-yesterday ()
  (interactive)
  (insert (wtd-timestamp-yesterday)))

(defun wtd-identifier (&optional timestamp)
  (concat "<<" (eshell-user-name) "-" (or timestamp (wtd-timestamp)) ">>"))

(defun wtd-insert-identifier ()
  (interactive)
  (insert (wtd-identifier)))

(defun remove-anchor-brackets (identifier)
  (replace-regexp-in-string "\[<>\]" "" identifier))

(defun title->label (title)
  (replace-regexp-in-string " " "-" (downcase title)))

(defun in-journal? ()
  (equal "journal.org" (buffer-name)))

(defun wtd-identifier-to-link (&optional identifier title)
  (interactive)
  (concat "["
          "[file:" (replace-regexp-in-string (concat "$HOME/" identity-dir)
                                             "" (buffer-file-name))
          "::" (or identifier (buffer-substring (region-beginning) (region-end)))
          "]"
          (if title (concat "[" title "]") "")
          "]"))

(defun wtd-subtree-heading-to-title (subtree-text)
  (replace-regexp-in-string "^\*+ <<.*>> \\\(.*\\\)" "\\1" subtree-text))

(defun wtd-subtree-heading-to-identifier (subtree-text)
  (replace-regexp-in-string "^\*+ <<\\\(.*\\\)>> .*" "\\1" subtree-text))

(defun wtd-subtree ()
  (outline-mark-subtree)
  (let ((bss (buffer-substring (region-beginning) (region-end))))
    (deactivate-mark)
    bss))

(defun wtd-subtree-heading ()
  (car (split-string (wtd-subtree) "\n")))

(defun wtd-transaction-title ()
  (wtd-subtree-heading-to-title (wtd-subtree-heading)))

(defun wtd-transaction-link ()
  (let* ((heading    (wtd-subtree-heading))
         (identifier (wtd-subtree-heading-to-identifier heading))
         (title      (wtd-subtree-heading-to-title heading)))
    (wtd-identifier-to-link (org-trim identifier) (org-trim title))))

(defun wtd-copy-transaction-link ()
  (interactive)
  (kill-new (wtd-transaction-link)))

(defun wtd-document-at-point ()
  (interactive)
  (message "This function will go to or create the document linked to"))

(defun wtd-document-buffer-name (identifier title &optional label)
  (concat (remove-anchor-brackets identifier) "-"
          (or label (title->label title)) ".org"))

(defun wtd-document (&optional identifier label title content)
  (interactive)
  (cd (concat "~/" identity-dir))
  (let* ((identifier (or identifier (wtd-identifier)))
         (title (or title (read-string "new document title: ")))
         (document-buffer-name (wtd-document-buffer-name
                                identifier title label))
         (path (concat "index/" document-buffer-name))
         (link (concat "file:" path "::" identifier)))
    (find-file path)
    (insert (concat "** " identifier " " title "\n"))
    (when content
      (indent-relative)
      (insert (concat content "\n")))
    (end-of-buffer)
    document-buffer-name))

(defun wtd-clone (&optional doc doc-title)
  (interactive)
  (cd (concat "~/" identity-dir))
  (let ((document (or doc
                      (ido-completing-read "clone document: "
                                           (directory-files
                                            "index/" nil
                                            directory-files-no-dot-files-regexp)))))
    (find-file (concat "index/" document))
    (wtd-document nil nil doc-title)
    (insert-buffer document)
    (kill-line)
    (kill-line)))

(defun wtd-revise ()
  (interactive)
  (when (in-journal?)
    (let* ((subtree-string (wtd-subtree))
           (ttitle (wtd-transaction-title))
           (tlink (wtd-transaction-link)))
      (wtd-document nil nil
                    (concat "Revision of " ttitle)
                    (concat "- Revision of " tlink "\n" subtree-string))
      (goto-line 3)
      (kill-line)
      (kill-line))))

(defun wtd-discard (&optional doc-buffer-name)
  (interactive)
  (cd (concat "~/" identity-dir))
  (let ((document (or doc-buffer-name
                      (ido-completing-read
                       "discard document: "
                       (cons (buffer-name)
                             (directory-files
                              "index/" nil
                              directory-files-no-dot-files-regexp))))))
    (find-file (concat "index/" document))
    (delete-file (buffer-file-name))
    (kill-buffer)))

(defun wtd-transact ()
  (interactive)
  (let ((buffer (buffer-name)))
    (other-window 1)
    (switch-to-buffer "journal.org")
    (beginning-of-buffer)
    (next-line)
    (insert-buffer buffer)
    (other-window 1)
    (switch-to-buffer buffer)
    (delete-file (buffer-file-name))
    (kill-buffer buffer)
    (when (not (eq (buffer-name) "journal.org"))
      (switch-to-buffer "journal.org"))
    (save-buffer)))

(global-set-key (kbd "C-c C-t") 'wtd-transact)

(defvar wtd-instruments nil)

(defun wtd-update-instruments ()
  (interactive)
  (let ((r (request-response-data
            (request "http://localhost:5514/0.1/instruments"
              :sync t
              :parser 'json-read))))
    (setq wtd-instruments
      (assoc-default 'instruments r))))

(defun wtd-find-instrument (instr-name)
  (first-match (lambda (instr-spec)
                 (string-equal instr-name
                               (assoc-default 'local-name instr-spec)))
               wtd-instruments))

(defun wtd-find-step (instr-spec step-name)
  (first-match (lambda (step-spec)
                 (string-equal step-name
                               (assoc-default 'name step-spec)))
               (assoc-default 'steps instr-spec)))

(defun wtd-insert-step (step &optional arg-key-vals)
  (let* ((instrument-and-step-names (split-string step "\\."))
         (instr-name (car instrument-and-step-names))
         (step-name (cadr instrument-and-step-names))
         (instrument-spec (wtd-find-instrument instr-name))
         (step-spec (wtd-find-step instrument-spec (cadr instrument-and-step-names)))
         (spacer (current-col-space)))
    (insert "+ step " instr-name "." step-name " " (wtd-timestamp))
    (let* ((arg-keys (assoc-default 'args step-spec))
           (args (or arg-key-vals
                     (cl-mapcar 'cons arg-keys (make-list (length arg-keys) "")))))
      (dovector (arg args)
                (insert (concat "\n" spacer "  + " (car arg) " " (cdr arg)))))))

;; (wtd-insert-step "water.log-hydration") ;; leave values to be filled in
;; (wtd-insert-step "water.log-hydration" '(("amount" . "1")))

(defun wtd-step ()
  (interactive)
  (let ((choices (apply
                  #'append
                  (mapcar
                   (lambda (instrument)
                     (let* ((instr (assoc-default 'local-name instrument))
                            (steps (assoc-default 'steps instrument)))
                       (mapcar
                        (lambda (step)
                          (concat instr "." (assoc-default 'name step)))
                        steps)))
                   wtd-instruments))))
    (let ((choice (ido-completing-read "instrument.step:" choices)))
      (wtd-insert-step choice))))

(defun wtd-senters-call? (token)
  ;; todo: support other calls
  (string-equal token "step"))

(defvar wtd-voice-doc-title "Voice Input Document")

(defvar wtd-voice-doc-buffer-name nil)

(defun wtd-voice-form-field-has-value? (field)
  (and (not (string-equal "" (assoc-default field doc)))
       (not (string-equal (symbol-name field)
                          (assoc-default field doc)))))

(defun wtd-voice-queue ()
  (interactive)
  (let ((documents
         (assoc-default 'documents
                        (request-response-data
                         (request "http://localhost:5514/0.1/voice-queue"
                           :sync t
                           :parser 'json-read)))))
    (when documents
      (dovector (doc documents)
                ;; experimenting with selecting available steps in a
                ;; web form. will need to replace with being driven
                ;; by instrument specs from journal though
                (if (wtd-voice-form-field-has-value? 'water)
                    (progn
                      (if (and wtd-voice-doc-buffer-name
                               (get-buffer wtd-voice-doc-buffer-name))
                          (progn
                            (switch-to-buffer wtd-voice-doc-buffer-name)
                            (end-of-buffer))
                        (setq wtd-voice-doc-buffer-name
                              (wtd-document nil nil wtd-voice-doc-title)))
                      (insert "   ")
                      (wtd-insert-step
                       "water.log-hydration"
                       (list (cons "amount"
                                   (assoc-default 'water doc))))
                      (newline)
                      (save-buffer))
                  (progn
                    (wtd-document nil nil
                                  (assoc-default 'title doc)
                                  (assoc-default 'document doc))
                    (save-buffer)))))))

(defun reorg-sweep-voice-buffer ()
  (interactive)
  (let ((target-buffer (buffer-name)))
    (switch-to-buffer wtd-voice-doc-buffer-name)
    (goto-line 2)
    (set-mark-command nil)
    (end-of-buffer)
    (kill-ring-save nil nil t)
    (wtd-discard wtd-voice-doc-buffer-name)
    (setq wtd-voice-doc-buffer-name nil)
    (switch-to-buffer target-buffer)
    (yank)))

(defun reorg-every-15-minutes ()
  (wtd-voice-queue)
  (wtd-update-instruments))
(defvar reorg-timer nil)
(defun reorg-start-polling ()
  (setq reorg-timer (run-with-timer 0 15 'reorg-every-15-minutes)))
(defun reorg-stop-polling ()
  (cancel-timer reorg-timer))

(defun wtd-ctrl-c-ctrl-c ()
  (interactive)
  (let* ((current-line (org-current-line-string))
         (parsed (cdddr (split-string current-line " " t))))
    (if (wtd-senters-call? (car parsed))
        (progn
          (back-to-indentation)
          (kill-line)
          (wtd-insert-step (cadr parsed)))
      nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'wtd-ctrl-c-ctrl-c)

(defun reorg-start-server ()
  (interactive)
  (cd (concat "~/" server-dir))
  (async-shell-command "lein ring server-headless 5514"
                       "*reorg-server*" "*reorg-server errors*"))

(defun reorg-stop-server ()
  (interactive)
  (kill-buffer "*reorg-server*"))

(defun reorg-start ()
  (interactive)
  (reorg-start-server)
  (cd (concat "~/" identity-dir))
  (reorg-start-polling))

(defun reorg-stop ()
  (reorg-stop-polling)
  (reorg-stop-server))
