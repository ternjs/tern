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
  (let ((is-error (and (consp c) (eq (car c) :error)))
        (found-body (search-forward "\n\n" nil t)))
    (if (or is-error (not found-body))
        (let ((message (and found-body
                            (buffer-substring-no-properties (point) (point-max)))))
          (delete-process url-http-process)
          (kill-buffer (current-buffer))
          (funcall (if is-error (cddr c) c)
                   (cons (and is-error (cadr c)) message) nil))
      (let ((json (json-read)))
        (delete-process url-http-process)
        (kill-buffer (current-buffer))
        (funcall c nil json)))))

(defun tern-project-dir ()
  (or tern-project-dir
      (let ((project-dir (file-name-directory (buffer-file-name))))
        (loop for cur = project-dir then (file-name-directory (substring cur 0 (1- (length cur))))
              while cur do
              (when (file-exists-p (concat cur ".tern-project"))
                (return (setf project-dir cur))))
        (setf tern-project-dir project-dir))))

(defun tern-find-server (c &optional ignore-port-file)
  (block nil
    (when tern-known-port
      (return (funcall c tern-known-port)))
    (unless (buffer-file-name)
      (return (funcall c nil)))
    (let ((port-file (and (not ignore-port-file) (concat (tern-project-dir) ".tern-port"))))
      (if (and port-file (file-exists-p port-file))
          (let ((port (string-to-number (with-temp-buffer
                                          (insert-file-contents port-file)
                                          (buffer-string)))))
            (setf tern-known-port port)
            (funcall c port))
        (tern-start-server c)))))

(defvar tern-command '("tern"))

(defun tern-start-server (c)
  (let* ((default-directory tern-project-dir)
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
(defvar tern-last-point-pos nil)

(defvar tern-known-port nil)
(defvar tern-project-dir nil)

(defvar tern-last-completions nil)
(defvar tern-last-argument-hints nil)
(defvar tern-buffer-is-dirty nil)

(defun tern-project-relative-file ()
  (substring (buffer-file-name) (length (tern-project-dir))))

(defun tern-get-partial-file (at)
  (let* (min-indent start-pos end-pos
         (min-pos (max 0 (- (point) 2000))))
    (save-excursion
      (goto-char at)
      (loop
       (unless (re-search-backward "\\bfunction\\b" min-pos t) (return))
       (let ((indent (current-indentation))
             (pos (line-beginning-position)))
         (when (or (not min-indent) (< indent min-indent))
           (setf min-indent indent min-indent-pos pos))
         (goto-char pos)))
      (unless start-pos (goto-char min-pos) (setf start-pos (line-beginning-position))))
    (save-excursion
      (goto-char at)
      (forward-char 1000)
      (let ((line-beg (line-beginning-position)))
        (setf end-pos (if (<= line-beg at) (line-end-position) line-beg))))
    `((type . "part")
      (name . ,(tern-project-relative-file))
      (offset . ,(1- start-pos))
      (text . ,(buffer-substring-no-properties start-pos end-pos)))))

(defun tern-run-request (f query &optional silent)
  (let ((generation (incf tern-command-generation))
        (buffer (current-buffer))
        (retrying nil)
        runner
        callback
        (doc `((query . ,query)))
        (sending-file nil)
        (offset 0))
      (let (file file-name)
        (cond
         ((not tern-buffer-is-dirty) (setf file-name (tern-project-relative-file)))
         ((> (buffer-size) 8000)
          (setf file (tern-get-partial-file (1+ (cdr (assoc 'end query))))
                offset (cdr (assoc 'offset file))
                file-name "#0")
          (decf (cdr (assoc 'end query)) offset))
         (t
          (setf file `((type . "full") (text . ,(buffer-string)) (name . ,(tern-project-relative-file)))
                sending-file t
                file-name (tern-project-relative-file))))
        (when file (push `(files . [, file]) doc))
        (push `(file . ,file-name) (cdr (assoc 'query doc))))
    (setf callback
          (lambda (port)
            (if port
                (tern-req port doc runner)
              (message "Could not find a Tern server"))))
    (setf runner
          (lambda (err data)
            (when (< tern-activity-since-command generation)
              (with-current-buffer buffer
                (cond ((not err)
                       (when sending-file (setf tern-buffer-is-dirty nil))
                       (funcall f data offset))
                      ((and (eq (cadar err) 'connection-failed) (not retrying))
                       (setf retrying t)
                       (setf tern-known-port nil)
                       (tern-find-server callback t))
                      ((not silent) (message "Request failed: %s" (cdr err))))))))
    (tern-find-server callback)))

;; Completion

(defun tern-completion-at-point ()
  (or (tern-completion-matches-last)
      (lambda ()
        (tern-run-request #'tern-do-complete
                          `((type . "completions")
                            (end . ,(1- (point))))))))

(defun tern-do-complete (data offset)
  (let ((cs (loop for elt across (cdr (assoc 'completions data)) collect (cdr (assoc 'name elt))))
        (start (+ 1 offset (cdr (assoc 'from data))))
        (end (+ 1 offset (cdr (assoc 'to data)))))
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

;; Argument hints

(defun tern-update-argument-hints ()
  (let ((opening-paren (cadr (syntax-ppss))))
    (when (and opening-paren (equal (char-after opening-paren) ?\())
      (if (and tern-last-argument-hints (eq (car tern-last-argument-hints) opening-paren))
          (tern-show-argument-hints)
        (tern-run-request (lambda (data _offset)
                            (let ((type (tern-parse-function-type data)))
                              (when type
                                (setf tern-last-argument-hints (cons opening-paren type))
                                (tern-show-argument-hints))))
                          `((type . "type")
                            (end . ,(1- opening-paren))
                            (preferFunction . t))
                          t)))))

(defun tern-skip-matching-brackets (end-chars)
  (let ((depth 0) (end (+ (point) 500)))
    (loop while (< (point) (point-max)) do
          (let ((next (char-after (point))))
            (cond
             ((and (<= depth 0) (find next end-chars)) (return t))
             ((or (eq next ?\)) (eq next ?\]) (eq next ?\})) (decf depth))
             ((or (eq next ?\() (eq next ?\[) (eq next ?\{)) (incf depth))
             ((> (point) end) (return nil)))
            (forward-char)))))

(defun tern-parse-function-type (data)
  (let ((type (cdr (assoc 'type data)))
        (name (or (cdr (assoc 'exprName data)) (cdr (assoc 'name data)) "fn")))
    (when (string-match-p "^fn(" type)
      (with-temp-buffer
        (insert type)
        (goto-char 4)
        (let (args retval)
          (loop until (eq (char-after (point)) ?\)) do
                (let ((name (when (looking-at "\\([a-zA-Z_$?]*\\):\\s-*")
                              (goto-char (match-end 0))
                              (match-string 1)))
                      (typestart (point)))
                  (tern-skip-matching-brackets '(?\) ?\,))
                  (push (cons name (buffer-substring typestart (point))) args))
                (when (eq (char-after (point)) ?\,) (forward-char 2)))
          (when (looking-at ") -> ")
            (setf retval (buffer-substring (+ (point) 5) (point-max))))
          (list name (nreverse args) retval))))))

(defun tern-find-current-arg (start)
  (when (< (point) (+ start 500))
    (save-excursion
      (let ((cur-point (point)))
        (goto-char (1+ start))
        (loop for i from 0 do
              (let ((found-end (tern-skip-matching-brackets '(?\) ?\,))))
                (when (>= (point) cur-point) (return i))
                (when (or (not found-end) (looking-at ")")) (return nil))
                (forward-char 1)))))))

(defun tern-show-argument-hints ()
  (declare (special message-log-max))
  (destructuring-bind (paren . type) tern-last-argument-hints
    (let ((parts ())
          (current-arg (tern-find-current-arg paren)))
      (destructuring-bind (name args ret) type
        (push (propertize name 'face 'font-lock-function-name-face) parts)
        (push "(" parts)
        (loop for arg in args for i from 0 do
              (unless (zerop i) (push ", " parts))
              (when (car arg)
                (push (if (eq i current-arg) (propertize (car arg) 'face 'highlight) (car arg)) parts)
                (push ": " parts))
              (push (propertize (cdr arg) 'face 'font-lock-type-face) parts))
        (push ")" parts)
        (when ret
          (push " -> " parts)
          (push (propertize ret 'face 'font-lock-type-face) parts)))
      (let (message-log-max)
        (message (apply #'concat (nreverse parts)))))))

;; Jump-to-definition

(defvar tern-find-definition-stack ())

(defun tern-find-definition ()
  (interactive)
  (tern-run-request (lambda (data _offset)
                      (push (cons (buffer-file-name) (point)) tern-find-definition-stack)
                      (let ((too-long (nthcdr 20 tern-find-definition-stack)))
                        (when too-long (setf (cdr too-long) nil)))
                      (tern-go-to-position (concat (tern-project-dir) (cdr (assoc 'file data)))
                                           (1+ (cdr (assoc 'start data)))))
                    `((type . "definition")
                      (end . ,(1- (point))))))

(defun tern-pop-find-definition ()
  (interactive)
  (when tern-find-definition-stack
    (destructuring-bind (file . pos) (pop tern-find-definition-stack)
      (tern-go-to-position file pos))))

(defun tern-go-to-position (file pos)
  (find-file file)
  (goto-char (min pos (point-max))))

;; Mode plumbing

(defun tern-after-change (_start _end _len)
  (setf tern-buffer-is-dirty t)
  (setf tern-last-point-pos nil)
  (when (and tern-last-argument-hints (<= (point) (car tern-last-argument-hints)))
    (setf tern-last-argument-hints nil)))

(defun tern-post-command ()
  (unless (eq (point) tern-last-point-pos)
    (setf tern-last-point-pos (point))
    (setf tern-activity-since-command tern-command-generation)
    (tern-update-argument-hints)))

(defvar tern-mode-keymap (make-sparse-keymap))
(define-key tern-mode-keymap [(meta ?.)] 'tern-find-definition)
(define-key tern-mode-keymap [(meta ?,)] 'tern-pop-find-definition)

(define-minor-mode tern-mode
  "Minor mode binding to the Tern JavaScript analyzer"
  nil
  " Tern"
  tern-mode-keymap
  (if tern-mode (tern-mode-enable) (tern-mode-disable)))

(defun tern-mode-enable ()
  (set (make-local-variable 'tern-known-port) nil)
  (set (make-local-variable 'tern-project-dir) nil)
  (set (make-local-variable 'tern-last-point-pos) nil)
  (set (make-local-variable 'tern-last-completions) nil)
  (set (make-local-variable 'tern-last-argument-hints) nil)
  (set (make-local-variable 'tern-buffer-is-dirty) (buffer-modified-p))
  (make-local-variable 'completion-at-point-functions)
  (push 'tern-completion-at-point completion-at-point-functions)
  (add-hook 'after-change-functions 'tern-after-change t t)
  (add-hook 'post-command-hook 'tern-post-command t t))

(defun tern-mode-disable ()
  (setf completion-at-point-functions
        (remove 'tern-completion-at-point completion-at-point-functions))
  (remove-hook 'after-change-functions 'tern-after-change t)
  (remove-hook 'post-command-hook 'tern-post-command t))
