;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))
(require 'json)

(defun tern-req (port doc c)
  (declare (special url-mime-charset-string url-request-method url-request-data url-show-status))
  (let ((url-mime-charset-string nil)
        (url-request-method "POST")
        (url-request-data (json-encode doc))
        (url-show-status nil)
        (url (url-parse-make-urlobj "http" nil nil "localhost" port "/" nil nil nil)))
    (url-http url #'tern-req-finished (list c))))

(defun tern-req-finished (c)
  (let ((found-body (search-forward "\n\n")))
    (if (and (consp c) (eq (car c) :error))
        (let ((message (and found-body
                            (buffer-substring (point) (point-max)))))
          (funcall c (or message (prin1-to-string (cadr c))) nil))
      (funcall c nil (json-read)))))

(defun tern-find-server (c)
  (block nil
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
              (if (file-exists-p (concat dir ".tern-port"))
                  (progn (setf found dir) (return)))))
      (if found
          (let ((port (string-to-number (with-temp-buffer
                                          (insert-file-contents (concat found ".tern-port"))
                                          (buffer-string)))))
            (tern-verify-server port (lambda (port)
                                       (if port
                                           (funcall c port)
                                         (tern-start-server (or found my-dir) c)))))
        (tern-start-server (or found my-dir) c)))))

(defun tern-verify-server (port c)
  (declare (special url-show-status))
  (let ((args (list nil c))
        (url-show-status nil))
    (setf (car args)
          (url-http (url-parse-make-urlobj "http" nil nil "localhost" port "/ping" nil nil nil)
                    (lambda (info c)
                      (funcall c (unless (and (consp info) (eq (car info) :error)) port)))
                    args))))

(defvar tern-command '("/usr/bin/node" "/home/marijn/js/tern/desktop.js"))

(defun tern-start-server (dir c)
  (let* ((default-directory dir)
         (proc (apply #'start-process "Tern" nil tern-command)))
    (set-process-sentinel proc (lambda (c _proc _event)
                                 (funcall c nil)))
    (set-process-filter proc (lambda (proc output)
                               (when (string-match "Listening on port \\([0-9][0-9]*\\)" output)
                                 (set-process-sentinel proc nil)
                                 (funcall c (string-to-number (match-string 1 output))))))))

(defvar tern-command-generation 0)
(defvar tern-activity-since-command -1)

(defun tern-complete ()
  (interactive)
  (let ((cmd (tern-run-command #'tern-do-complete))
        (end (1- (point))))
    (tern-find-server (lambda (port)
                        (when port
                          (tern-req port `((query . ((type . "completions") (file . "infer.js") (end . ,end)))) cmd))))))

(defun tern-run-command (f)
  (let ((generation tern-command-generation)
        (buffer (current-buffer)))
    (lambda (err data)
      (when (< tern-activity-since-command generation)
        (if err
            (message err)
          (with-current-buffer buffer
            (funcall f data)))))))

(defun tern-do-complete (data)
  (let ((cs (cdr (assoc 'completions data))))
    (when (> (length cs) 0)
      (tern-complete-word (cdr (assoc 'name (elt cs 0)))))))

(defun tern-complete-word (word)
  (let (start end)
    (save-excursion
      (if (re-search-backward "[^\\w_$]" nil t)
          (forward-char)
        (goto-char 0))
      (setf start (point) end start)
      (when (re-search-forward "[^\\w_$]" nil t)
        (setf end (1- (point))))
      (delete-region start end)
      (goto-char start)
      (insert word))
    (goto-char (+ start (length word)))))

;; Mode

(defvar tern-mode-keymap (make-sparse-keymap))
(define-key tern-mode-keymap [M-/] #'tern-complete)

(define-minor-mode tern-mode
  "Minor mode that enables bindings for the Tern JavaScript analyser"
  nil
  " Tern"
  tern-mode-keymap)
