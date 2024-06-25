(add-to-list 'load-path "~/.config/emacs/")

;; === Packages ===
(load "packages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((nano-theme :url "https://github.com/rougier/nano-theme" :branch
		 "master"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set modeline to top of screen
;;(setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)	
(setq-default mode-line-format '("%e" mode-line-front-space
 (:propertize
  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated)
  display (min-width (6.0)))
 mode-line-frame-identification mode-line-buffer-identification mode-line-position
 mode-line-format-right-align mode-line-modes (project-mode-line project-mode-line-format) (vc-mode vc-mode) mode-line-misc-info "  " mode-line-end-spaces))

;; Set SSH_AUTH_SOCK
(setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR")
				"/ssh-agent.socket"))

(message "init.el loaded successfully")




















