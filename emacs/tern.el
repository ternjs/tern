;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)
(require 'url-http)

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
      (and (not (buffer-file-name)) (setf tern-project-dir ""))
      (let ((project-dir (file-name-directory (buffer-file-name))))
        (loop for cur = project-dir then (file-name-directory (substring cur 0 (1- (length cur))))
              while cur do
              (when (file-exists-p (concat cur ".tern-project"))
                (return (setf project-dir cur))))
        (setf tern-project-dir project-dir))))

(defun tern-find-server (c &optional ignore-port)
  (block nil
    (when tern-known-port
      (return (funcall c tern-known-port)))
    (unless (buffer-file-name)
      (return (funcall c nil)))
    (let ((port-file (concat (tern-project-dir) ".tern-port")))
      (when (file-exists-p port-file)
        (let ((port (string-to-number (with-temp-buffer
                                        (insert-file-contents port-file)
                                        (buffer-string)))))
          (unless (eq port ignore-port)
            (setf tern-known-port port)
            (return (funcall c port))))))
    (tern-start-server c)))

(defvar tern-command
  (let* ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name))
         (bin-file (expand-file-name "../bin/tern" (file-name-directory script-file))))
    (list (if (file-exists-p bin-file) bin-file "tern")))
  "The command to be run to start the Tern server. Should be a
list of strings, giving the binary name and arguments.")

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
      (goto-char (min (+ at 1000) (point-max)))
      (let ((line-beg (line-beginning-position)))
        (setf end-pos (if (<= line-beg at) (line-end-position) line-beg))))
    `((type . "part")
      (name . ,(tern-project-relative-file))
      (offset . ,(1- start-pos))
      (text . ,(buffer-substring-no-properties start-pos end-pos)))))

(defun tern-modified-sibling-buffers ()
  (let (found)
    (dolist (buf (buffer-list))
      (when (and (not (eq buf (current-buffer)))
                 (buffer-local-value 'tern-mode buf)
                 (buffer-local-value 'tern-buffer-is-dirty buf)
                 (equal tern-project-dir (buffer-local-value 'tern-project-dir buf)))
        (with-current-buffer buf
          (push `((type . "full")
                  (name . ,(tern-project-relative-file))
                  (text . ,(buffer-string))) found))))
    (nreverse found)))

(defun tern-run-request (f query pos &optional mode)
  (when (stringp query) (setf query `((type . ,query))))
  (let ((generation (incf tern-command-generation))
        (buffer (current-buffer))
        (retrying nil)
        runner
        callback
        (doc `((query . ,query)))
        (files (and (eq mode :full-file) (tern-modified-sibling-buffers)))
        file-name
        (offset 0)
        (pos pos))
    (cond
     ((not tern-buffer-is-dirty) (setf file-name (tern-project-relative-file)))
     ((and (not (eq mode :full-file)) (> (buffer-size) 8000))
      (push (tern-get-partial-file pos) files)
      (setf offset (cdr (assq 'offset (car files)))
            file-name "#0")
      (decf pos offset))
     (t
      (push `((type . "full") (text . ,(buffer-string)) (name . ,(tern-project-relative-file))) files)
      (setf file-name (tern-project-relative-file))))
    (when files (push `(files . ,(apply #'vector files)) doc))
    (push `(file . ,file-name) (cdr (assq 'query doc)))
    (push `(end . ,(1- pos)) (cdr (assq 'query doc)))
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
                       (dolist (file files)
                         (when (equal (cdr (assq 'type file)) "full")
                           (with-current-buffer (find-file-noselect (concat tern-project-dir (cdr (assq 'name file))))
                             (setf tern-buffer-is-dirty nil))))
                       (funcall f data offset))
                      ((and (eq (cadar err) 'connection-failed) (not retrying))
                       (setf retrying t)
                       (let ((old-port tern-known-port))
                         (setf tern-known-port nil)
                         (tern-find-server callback old-port)))
                      ((not (eq mode :silent)) (message "Request failed: %s" (cdr err))))))))
    (tern-find-server callback)))

;; Completion

(defun tern-completion-at-point ()
  (or (tern-completion-matches-last)
      (lambda ()
        (tern-run-request #'tern-do-complete "completions" (point)))))

(defun tern-do-complete (data offset)
  (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 offset (cdr (assq 'start data))))
        (end (+ 1 offset (cdr (assq 'end data)))))
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
                  (string-match-p "^[a-zA-Z0-9_$]*$" (buffer-substring-no-properties end (point)))
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
                            (preferFunction . t))
                          opening-paren
                          :silent)))))

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
  (let ((type (cdr (assq 'type data)))
        (name (or (cdr (assq 'exprName data)) (cdr (assq 'name data)) "fn")))
    (when (string-match-p "^fn(" type)
      (with-temp-buffer
        (insert type)
        (goto-char 4)
        (let (args retval)
          (loop until (eq (char-after (point)) ?\)) do
                (let ((name (when (looking-at "\\([a-zA-Z0-9_$?]*\\):\\s-*")
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

;; Refactoring ops

(defun tern-do-refactor (data _offset)
  (let ((per-file ())
        (orig-buffer (current-buffer)))
    (loop for change across (cdr (assq 'changes data)) do
          (let ((found (assq-string (cdr (assq 'file change)) per-file)))
            (unless found (setf found (list (cdr (assq 'file change)))) (push found per-file))
            (push change (cdr found))))
    (loop for (file . changes) in per-file do
          (setf changes (sort changes (lambda (a b) (> (cdr (assq 'start a)) (cdr (assq 'start b))))))
          (find-file (concat (tern-project-dir) file))
          (loop for change in changes do
                (let ((start (1+ (cdr (assq 'start change))))
                      (end (1+ (cdr (assq 'end change)))))
                (delete-region start end)
                (save-excursion
                  (goto-char start)
                  (insert (cdr (assq 'text change)))))))
    (switch-to-buffer orig-buffer)))

(defun tern-rename-variable (new-name)
  (interactive "MNew variable name: ")
  (tern-run-request #'tern-do-refactor `((type . "rename") (newName . ,new-name)) (point) :full-file))

;; Jump-to-definition

(defvar tern-find-definition-stack ())

(defun tern-find-definition ()
  (interactive)
  (tern-run-request (lambda (data _offset)
                      (push (cons (buffer-file-name) (point)) tern-find-definition-stack)
                      (let ((too-long (nthcdr 20 tern-find-definition-stack)))
                        (when too-long (setf (cdr too-long) nil)))
                      (tern-go-to-position (concat (tern-project-dir) (cdr (assq 'file data)))
                                           (1+ (cdr (assq 'start data)))))
                    "definition"
                    (point)
                    :full-file))

(defun tern-pop-find-definition ()
  (interactive)
  (when tern-find-definition-stack
    (destructuring-bind (file . pos) (pop tern-find-definition-stack)
      (tern-go-to-position file pos))))

(defun tern-go-to-position (file pos)
  (find-file file)
  (goto-char (min pos (point-max))))

;; Query type

(defun tern-get-type ()
  (interactive)
  (tern-run-request (lambda (data _offset) (message (or (cdr (assq 'type data)) "Not found")))
                    "type"
                    (point)))

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
(define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)
(define-key tern-mode-keymap [(control ?c) (control ?c)] 'tern-get-type)

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

(provide 'tern)
