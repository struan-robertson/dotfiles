;; -*- lexical-binding: t; -*-

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(defun kill-buffer-and-frame ()
  "Kill current buffer and delete its frame."
  (interactive)
  (kill-buffer)
  (delete-frame))

;; Bind it to a key if desired
(global-set-key (kbd "C-x 5 k") 'kill-buffer-and-frame)
