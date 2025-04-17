(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; From https://www.masteringemacs.org/article/evaluating-elisp-emacs
(defun my/elisp-mode-eval-buffer ()
  "Evaluate buffer and provide feedback that buffer has been evaluated"
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'my/elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'my/elisp-mode-eval-buffer)

(defun kill-buffer-and-frame ()
  "Kill current buffer and delete its frame."
  (interactive)
  (kill-buffer)
  (delete-frame))

;; Bind it to a key if desired
(global-set-key (kbd "C-x 5 k") 'kill-buffer-and-frame)
