;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defun tern-req (port doc c)
  (declare (special url-mime-charset-string url-request-method url-request-data url-show-status))
  (let ((url-mime-charset-string nil) ; Suppress huge, useless header
        (url-request-method "POST")
        (url-request-data (json-encode doc))
        (url-show-status nil)
        (url (url-parse-make-urlobj "http" nil nil "localhost" port "/" nil nil nil)))
    (url-http url #'tern-req-finished (list c))))

(defun tern-req-finished (c)
  (declare (special url-http-process))
  (let ((found-body (search-forward "\n\n" nil t)))
    (if (and (consp c) (eq (car c) :error))
        (let ((message (and found-body
                            (buffer-substring-no-properties (point) (point-max)))))
          (delete-process url-http-process)
          (kill-buffer (current-buffer))
          (funcall (cddr c) (cons (cadr c) message) nil))
      (let ((json (json-read)))
        (delete-process url-http-process)
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
      ;; FIXME use built-in filename manipulation utils
      (loop for i from (1- (length dirs)) downto 0 do
            (let ((dir "/"))
              (loop for j from 0 below i do
                    (setf dir (concat dir (nth j dirs) "/")))
              (unless my-dir (setf my-dir dir))
              (if (or (file-exists-p (concat dir ".tern-project"))
                      (file-exists-p (concat dir ".tern-port")))
                  (progn (setf found dir) (return)))))
      (setf tern-project-dir (or found my-dir))
      (if (and found (not ignore-port-file) (file-exists-p (concat found ".tern-port")))
          (let ((port (string-to-number (with-temp-buffer
                                          (insert-file-contents (concat found ".tern-port"))
                                          (buffer-string)))))
            (setf tern-known-port port)
            (funcall c port))
        (tern-start-server tern-project-dir c)))))

(defvar tern-command '("tern"))

(defun tern-start-server (dir c)
  (let* ((default-directory dir)
         (proc (apply #'start-process "Tern" nil tern-command)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event)
                                 (delete-process proc)
                                 (funcall c nil)))
    (set-process-filter proc (lambda (proc output)
                               (when (string-match "Listening on port \\([0-9][0-9]*\\)" output)
                                 (setf tern-known-port (string-to-number (match-string 1 output)))
                                 (set-process-sentinel proc (lambda (proc _event)
                                                              (delete-process proc)
                                                              (setf tern-known-port nil)))
                                 (set-process-filter proc nil)
                                 (funcall c tern-known-port))))))

(defvar tern-command-generation 0)
(defvar tern-activity-since-command -1)

(defvar tern-known-port nil)
(defvar tern-project-dir nil)

(defvar tern-last-completions nil)
(defvar tern-buffer-is-dirty nil)

(defun tern-local-filepath ()
  (substring (buffer-file-name) (length tern-project-dir)))

(defun tern-run-command (f query)
  (let ((generation (incf tern-command-generation))
        (buffer (current-buffer))
        (retrying nil)
        runner
        callback
        (doc `((query . ,query)))
        (sending-file nil))
    (when tern-buffer-is-dirty
      (setf sending-file t)
      (push `(files . [((type . "full") (text . ,(buffer-string)) (name . ,(tern-local-filepath)))])
            doc))
    (setf callback
          (lambda (port)
            (if port
                (tern-req port doc runner)
              (message "Could not find a Tern server"))))
    (setf runner
          (lambda (err data)
            (when (< tern-activity-since-command generation)
              (cond ((not err)
                     (when sending-file (setf tern-buffer-is-dirty nil))
                     (with-current-buffer buffer (funcall f data)))
                    ((and (eq (cadar err) 'connection-failed) (not retrying))
                     (setf retrying t)
                     (setf tern-known-port nil)
                     (tern-find-server callback t))
                    (t (message "Request failed: %s" (cdr err)))))))
    (tern-find-server callback)))

(defun tern-do-complete (data)
  (let ((cs (loop for elt across (cdr (assoc 'completions data)) collect (cdr (assoc 'name elt))))
        (start (1+ (cdr (assoc 'from data))))
        (end (1+ (cdr (assoc 'to data)))))
    (setf tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
    (completion-in-region start end cs)))

(defun tern-completion-matches-last ()
  (when tern-last-completions
    (destructuring-bind (word start end list) tern-last-completions
      (and (<= end (point-max))
           (equal word (buffer-substring-no-properties start end))
           (if (= (point) end)
               (cdr tern-last-completions)
             (and (>= (point) end)
                  (<= (point) (+ end 50))
                  (string-match-p "^[a-zA-Z_$]*$" (buffer-substring-no-properties end (point)))
                  (let ((new-word (buffer-substring-no-properties start (point))))
                    (list start (point)
                          (loop for elt in list
                                when (eq (compare-strings word 0 (length word) new-word 0 (length word)) t)
                                collect elt)))))))))

(defun tern-completion-at-point ()
  (or (tern-completion-matches-last)
      (lambda ()
        (tern-run-command #'tern-do-complete
                          `((type . "completions")
                            (file . ,(tern-local-filepath))
                            (end . ,(1- (point))))))))

;; Mode

(defun tern-after-change-function (_start _end _len)
  (setf tern-buffer-is-dirty t))

(defvar tern-mode-keymap (make-sparse-keymap))

(define-minor-mode tern-mode
  "Minor mode binding to the Tern JavaScript analyzer"
  nil
  " Tern"
  tern-mode-keymap
  (if tern-mode (tern-mode-enable) (tern-mode-disable)))

(defun tern-mode-enable ()
  (set (make-local-variable 'tern-known-port) nil)
  (set (make-local-variable 'tern-project-dir) nil)
  (set (make-local-variable 'tern-last-completions) nil)
  (set (make-local-variable 'tern-buffer-is-dirty) (buffer-modified-p))
  (make-local-variable 'completion-at-point-functions)
  (push 'tern-completion-at-point completion-at-point-functions)
  (add-hook 'after-change-functions 'tern-after-change-function t t))

(defun tern-mode-disable ()
  (setf completion-at-point-functions
        (remove 'tern-completion-at-point completion-at-point-functions))
  (remove-hook 'after-change-functions 'tern-after-change-function t))
