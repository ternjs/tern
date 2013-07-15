;;; -*- lexical-binding: t -*-
;;; tern-ido-complete.el --- Tern Completion by ido.el

;; Author:  <https://github.com/katspaugh>
;; Version: 0.0.1

;;; Commentary:

;; Display and refine completions in minibuffer using
;; `ido-completing-read'.

;;; Installation:

;; Add the following lines below the Tern setup code.

;; (eval-after-load 'tern
;;   '(progn
;;      (setq ido-record-commands nil)
;;      (setq ido-max-window-height 1)
;;      (local-set-key (kbd "M-<tab>") 'tern-ido-complete)))

;;; Code:

(require 'tern)
(require 'ido)


(defun tern-ido-display (data)
  (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 (cdr (assq 'start data))))
        (end (+ 1 (cdr (assq 'end data)))))
    (let ((refined (if (eq 1 (length cs)) cs
                     (list (ido-completing-read "" cs)))))
      (completion-in-region start end refined))))

(defun tern-ido-complete ()
  (interactive)
  (tern-run-query #'tern-ido-display "completions" (point)))


(provide 'tern-ido-complete)
;;; tern-ido-complete.el ends here
