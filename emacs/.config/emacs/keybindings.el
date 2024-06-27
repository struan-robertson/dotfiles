(global-set-key (kbd "C-x C-b") 'ibuffer)

;; From https://www.masteringemacs.org/article/evaluating-elisp-emacs
(defun mp-elisp-mode-eval-buffer ()
  "Evaluate buffer and provide feedback that buffer has been evaluated"
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
