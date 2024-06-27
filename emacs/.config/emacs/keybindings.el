(global-set-key (kbd "C-x C-b") 'ibuffer)

;; From https://www.masteringemacs.org/article/evaluating-elisp-emacs
(defun mp-elisp-mode-eval-buffer ()
  "Evaluate buffer and provide feedback that buffer has been evaluated"
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

;; Multiple cursors on marked region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; First mark word, then add more cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
