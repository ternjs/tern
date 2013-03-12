;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defun tern-req (port doc c)
  (declare (special url-mime-charset-string url-request-method url-request-data url-show-status))
  (let ((url-mime-charset-string nil)
        (url-request-method "POST")
        (url-request-data (json-encode doc))
        (url-show-status nil)
        (url (url-parse-make-urlobj "http" nil nil "localhost" port "/" nil nil nil)))
    (url-http url #'tern-req-finished (list c))))

(defun tern-req-finished (c)
  (let ((found-body (search-forward "\n\n" nil t)))
    (if (and (consp c) (eq (car c) :error))
        (let ((message (and found-body
                            (buffer-substring (point) (point-max)))))
          (kill-buffer (current-buffer))
          (funcall (cddr c) (cons (cadr c) message) nil))
      (let ((json (json-read)))
        (kill-buffer (current-buffer))
        (funcall c nil json)))))

(defun tern-find-server (c &optional ignore-port-file)
  (block nil
    (when tern-known-port
      (return (funcall c tern-known-port)))
    (unless (buffer-file-name)
      (return (funcall c nil)))
    (let ((dirs (split-string (buffer-file-name) "/" t))
          my-dir
          found)
      (loop for i from (1- (length dirs)) downto 0 do
            (let ((dir "/"))
              (loop for j from 0 below i do
                    (setf dir (concat dir (nth j dirs) "/")))
              (unless my-dir (setf my-dir dir))
              (if (or (file-exists-p (concat dir ".tern-project"))
                      (file-exists-p (concat dir ".tern-port")))
                  (progn (setf found dir) (return)))))
      (if (and found (not ignore-port-file) (file-exists-p (concat found ".tern-port")))
          (funcall c (string-to-number (with-temp-buffer
                                         (insert-file-contents (concat found ".tern-port"))
                                         (buffer-string))))
        (tern-start-server (or found my-dir) c)))))

;; FIXME this probably won't work on other systems...
(defvar tern-command '("/usr/bin/node" "/home/marijn/src/js/tern/desktop.js"))

(defun tern-start-server (dir c)
  (let* ((default-directory dir)
         (proc (apply #'start-process "Tern" nil tern-command)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event) (funcall c nil)))
    (set-process-filter proc (lambda (proc output)
                               (when (string-match "Listening on port \\([0-9][0-9]*\\)" output)
                                 (set-process-sentinel proc nil)
                                 (set-process-filter proc nil)
                                 (funcall c (string-to-number (match-string 1 output))))))))

(defvar tern-command-generation 0)
(defvar tern-activity-since-command -1)

(defun tern-complete ()
  (interactive)
  (tern-run-command #'tern-do-complete
                    `((query . ((type . "completions") (file . "infer.js") (end . ,(1- (point))))))))

(defun tern-run-command (f doc)
  (let ((generation tern-command-generation)
        (buffer (current-buffer))
        (retrying nil)
        runner
        callback)
    (setf callback
          (lambda (port)
            (if port
                (tern-req port doc runner)
              (message "Could not find a Tern server"))))
    (setf runner
          (lambda (err data)
            (when (< tern-activity-since-command generation)
              (cond ((not err)
                     (with-current-buffer buffer (funcall f data)))
                    ((and (eq (cadar err) 'connection-failed) (not retrying))
                     (setf retrying t)
                     (tern-find-server callback t))
                    (t (message "Failed to start a Tern server"))))))
    (tern-find-server callback)))
  
(defun tern-do-complete (data)
  (let ((cs (cdr (assoc 'completions data))))
    (when (> (length cs) 0)
      (tern-complete-word (cdr (assoc 'name (elt cs 0)))))))

(defun tern-complete-word (word)
  (let (start end)
    (save-excursion
      (if (re-search-backward "[^a-zA-Z_$]" nil t)
          (forward-char)
        (goto-char 0))
      (setf start (point) end start)
      (when (re-search-forward "[^a-zA-Z_$]" nil t)
        (setf end (1- (point))))
      (delete-region start end)
      (goto-char start)
      (insert word))
    (goto-char (+ start (length word)))))

;; Mode

(defvar tern-mode-keymap (make-sparse-keymap))

(define-minor-mode tern-mode
  "Minor mode binding to the Tern JavaScript analyzer"
  nil
  " Tern"
  tern-mode-keymap
  (if tern-mode (tern-mode-enable) (tern-mode-disable)))

(defvar tern-known-port nil)

(defun tern-completion-at-point () #'tern-complete)

(defun tern-mode-enable ()
  (set (make-local-variable 'tern-known-port) nil)
  (make-local-variable 'completion-at-point-functions)
  (push 'tern-completion-at-point completion-at-point-functions))

(defun tern-mode-disable ()
  (setf completion-at-point-functions
        (remove 'tern-completion-at-point completion-at-point-functions)))
