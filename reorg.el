(defvar project-dir (getenv "REORG_HOME"))

(defvar identity-dir (getenv "REORG_IDENTITY"))

(defun load-project-file (f)
  (load-file (concat "$HOME/" project-dir f)))

(load-project-file "dependencies.el")
(load-project-file "utils.el")

(defvar time-format "%Y%m%d%H%M%S")

(defun reorg-timestamp ()
  (format-time-string time-format))

(defun reorg-timestamp-for-day (&optional f)
  (pcase-let ((`(,s_ ,m_ ,h_ ,d ,m ,y ,dow_ ,dst_ ,utc_)
               (decode-time (funcall f (current-time)))))
    (format-time-string time-format
                        (apply #'encode-time
                               (list 0 0 0 d m y)))))

(defun reorg-timestamp-yesterday ()
  (reorg-timestamp-for-day
   (lambda (time)
     (time-subtract time (* 24 3600)))))

(defun reorg-timestamp-today ()
  (reorg-timestamp-for-day
   (lambda (time) time)))

(defun reorg-insert-timestamp ()
  (interactive)
  (insert (reorg-timestamp)))

(defun reorg-insert-timestamp-yesterday ()
  (interactive)
  (insert (reorg-timestamp-yesterday)))

(defun reorg-identifier (&optional timestamp)
  (concat "<<" (eshell-user-name) "-" (or timestamp (reorg-timestamp)) ">>"))

(defun reorg-insert-identifier ()
  (interactive)
  (insert (reorg-identifier)))

(defun remove-anchor-brackets (identifier)
  (replace-regexp-in-string "\[<>\]" "" identifier))

(defun title->label (title)
  (replace-regexp-in-string " " "-" (downcase title)))

(defun in-journal? ()
  (equal "journal.org" (buffer-name)))

(defun reorg-identifier-to-link (&optional identifier title)
  (interactive)
  (concat "["
          "[file:" (replace-regexp-in-string (concat "$HOME/" identity-dir)
                                             "" (buffer-file-name))
          "::" (or identifier (buffer-substring (region-beginning) (region-end)))
          "]"
          (if title (concat "[" title "]") "")
          "]"))

(defun reorg-subtree-heading-to-title (subtree-text)
  (replace-regexp-in-string "^\*+ <<.*>> \\\(.*\\\)" "\\1" subtree-text))

(defun reorg-subtree-heading-to-identifier (subtree-text)
  (replace-regexp-in-string "^\*+ <<\\\(.*\\\)>> .*" "\\1" subtree-text))

(defun reorg-subtree ()
  (outline-mark-subtree)
  (let ((bss (buffer-substring (region-beginning) (region-end))))
    (deactivate-mark)
    bss))

(defun reorg-subtree-heading ()
  (car (split-string (reorg-subtree) "\n")))

(defun reorg-transaction-title ()
  (reorg-subtree-heading-to-title (reorg-subtree-heading)))

(defun reorg-transaction-link ()
  (let* ((heading    (reorg-subtree-heading))
         (identifier (reorg-subtree-heading-to-identifier heading))
         (title      (reorg-subtree-heading-to-title heading)))
    (reorg-identifier-to-link (org-trim identifier) (org-trim title))))

(defun reorg-copy-transaction-link ()
  (interactive)
  (kill-new (reorg-transaction-link)))

(defun reorg-document-at-point ()
  (interactive)
  (message "This function will go to or create the document linked to"))

(defun reorg-document-buffer-name (identifier title &optional label)
  (concat (remove-anchor-brackets identifier) "-"
          (or label (title->label title)) ".org"))

(defun reorg-document (&optional identifier label title content)
  (interactive)
  (cd (concat "~/" identity-dir))
  (let* ((identifier (or identifier (reorg-identifier)))
         (title (or title (read-string "new document title: ")))
         (document-buffer-name (reorg-document-buffer-name
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

(defun reorg-clone (&optional doc doc-title)
  (interactive)
  (cd (concat "~/" identity-dir))
  (let ((document (or doc
                      (ido-completing-read "clone document: "
                                           (directory-files
                                            "index/" nil
                                            directory-files-no-dot-files-regexp)))))
    (find-file (concat "index/" document))
    (reorg-document nil nil doc-title)
    (insert-buffer document)
    (kill-line)
    (kill-line)))

(defun reorg-revise ()
  (interactive)
  (when (in-journal?)
    (let* ((subtree-string (reorg-subtree))
           (ttitle (reorg-transaction-title))
           (tlink (reorg-transaction-link)))
      (reorg-document nil nil
                    (concat "Revision of " ttitle)
                    (concat "- Revision of " tlink "\n" subtree-string))
      (goto-line 3)
      (kill-line)
      (kill-line))))

(defun reorg-discard (&optional doc-buffer-name)
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

(defun transactable? ()
  (interactive)
  (let ((buffer (buffer-name)))
    (cond ((not (string= "org" (file-name-extension buffer)))
           (progn (message "only org documents can be transacted")
                  nil))
          ((string= buffer "journal.org")
           (progn (message "journal cannot be transacted to journal")
                  nil))
          (t t))))

(defun reorg-transact ()
  (interactive)
  (when (transactable?)
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
      (other-window 1)
      (save-buffer))))

(global-set-key (kbd "C-c C-t") 'reorg-transact)

(defvar reorg-instruments nil)

(defun reorg-update-instruments ()
  (interactive)
  (let ((r (request-response-data
            (request "http://localhost:5514/0.1/instruments"
              :sync t
              :parser 'json-read))))
    (setq reorg-instruments
      (assoc-default 'instruments r))))

(defun reorg-find-instrument (instr-name)
  (first-match (lambda (instr-spec)
                 (string-equal instr-name
                               (assoc-default 'local-name instr-spec)))
               reorg-instruments))

(defun reorg-find-step (instr-spec step-name)
  (first-match (lambda (step-spec)
                 (string-equal step-name
                               (assoc-default 'name step-spec)))
               (assoc-default 'steps instr-spec)))

(defun reorg-insert-step (step &optional arg-key-vals)
  (let* ((instrument-and-step-names (split-string step "\\."))
         (instr-name (car instrument-and-step-names))
         (step-name (cadr instrument-and-step-names))
         (instrument-spec (reorg-find-instrument instr-name))
         (step-spec (reorg-find-step instrument-spec (cadr instrument-and-step-names)))
         (spacer (current-col-space)))
    (insert "+ step " instr-name "." step-name " " (reorg-timestamp))
    (let* ((arg-keys (assoc-default 'args step-spec))
           (args (or arg-key-vals
                     (cl-mapcar 'cons arg-keys (make-list (length arg-keys) "")))))
      (dovector (arg args)
                (insert (concat "\n" spacer "  + " (car arg) " " (cdr arg)))))))

;; (reorg-insert-step "water.log-hydration") ;; leave values to be filled in
;; (reorg-insert-step "water.log-hydration" '(("amount" . "1")))
;; (reorg-insert-step "water.log-hydration" '((amount . "1")))

(defun reorg-step ()
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
                   reorg-instruments))))
    (let ((choice (ido-completing-read "instrument.step:" choices)))
      (reorg-insert-step choice))))

(defun reorg-senters-call? (token)
  ;; todo: support other calls
  (string-equal token "step"))

(defvar reorg-voice-doc-title "Voice Input Document")

(defvar reorg-voice-doc-buffer-name nil)

(defun reorg-voice-form-field-has-value? (field)
  (and (not (string-equal "" (assoc-default field doc)))
       (not (string-equal (symbol-name field)
                          (assoc-default field doc)))))

(defun reorg-voice-queue ()
  (interactive)
  (let ((documents
         (assoc-default 'documents
                        (request-response-data
                         (request "http://localhost:5514/0.1/voice-queue"
                           :sync t
                           :parser 'json-read)))))
    ;; (when documents (prn-scratch documents))
    (when documents
      (dovector (doc documents)
                (let* ((instr-step (symbol-name (caar doc)))
                       (step-args (cdr doc)))
                  (if (string-equal instr-step "create-new-doc")
                      (progn
                        (reorg-document nil nil
                                        (assoc-default 'doc-title doc)
                                        (assoc-default 'doc-body doc))
                        (save-buffer))
                    (progn
                      (if (and reorg-voice-doc-buffer-name
                               (get-buffer reorg-voice-doc-buffer-name))
                          (progn
                            (switch-to-buffer reorg-voice-doc-buffer-name)
                            (end-of-buffer))
                        (setq reorg-voice-doc-buffer-name
                              (reorg-document nil nil reorg-voice-doc-title)))
                      (insert "   ")
                      (reorg-insert-step instr-step
                                       (mapcaar-dotpair-list 'symbol-name
                                                             step-args))
                      (newline)
                      (save-buffer))))))))

(defun reorg-sweep-voice-buffer ()
  (interactive)
  (let ((target-buffer (buffer-name)))
    (switch-to-buffer reorg-voice-doc-buffer-name)
    (goto-line 2)
    (set-mark-command nil)
    (end-of-buffer)
    (kill-ring-save nil nil t)
    (reorg-discard reorg-voice-doc-buffer-name)
    (setq reorg-voice-doc-buffer-name nil)
    (switch-to-buffer target-buffer)
    (yank)))

(defun reorg-every-15-seconds ()
  (reorg-voice-queue)
  (reorg-update-instruments))
(defvar reorg-timer nil)
(defun reorg-start-polling ()
  (setq reorg-timer (run-with-timer 0 15 'reorg-every-15-seconds)))
(defun reorg-stop-polling ()
  (when reorg-timer
    (cancel-timer reorg-timer)))

(defun reorg-ctrl-c-ctrl-c ()
  (interactive)
  (let* ((current-line (org-current-line-string))
         (parsed (cdddr (split-string current-line " " t))))
    (if (reorg-senters-call? (car parsed))
        (progn
          (back-to-indentation)
          (kill-line)
          (reorg-insert-step (cadr parsed)))
      nil)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'reorg-ctrl-c-ctrl-c)

(defun reorg-start-server ()
  (interactive)
  (cd (concat "~/" project-dir))
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
  (interactive)
  (reorg-stop-polling)
  (reorg-stop-server))
