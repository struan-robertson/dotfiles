(add-to-list 'load-path "~/.config/emacs/lisp")

;; === Packages ===
(load "packages")

;; === Keybindings ===
(load "keybindings")

;; === Faces ===
(set-face-attribute 'variable-pitch nil :family "ShureTechMono Nerd Font Propo")
(set-face-attribute 'default nil :font "ShureTechMono Nerd Font Propo")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#a3be8c" :weight bold))))
 '(font-latex-string-face ((t (:foreground "#d8dee9"))))
 '(font-latex-warning-face ((t (:inherit bold :foreground "#bf616a")))))

;; ;; Set modeline to top of screen
;; ;;(setq-default header-line-format mode-line-format)
;; ;; (setq-default mode-line-format nil)	
;; (setq-default mode-line-format '("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated)
;;   display (min-width (6.0)))
;;  mode-line-frame-identification mode-line-buffer-identification mode-line-position
;;  mode-line-format-right-align mode-line-modes (project-mode-line project-mode-line-format) (vc-mode vc-mode) mode-line-misc-info "  " mode-line-end-spaces))

;; Set SSH_AUTH_SOCK
(setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR")
				"/ssh-agent.socket"))

;; Pair specific chars
(electric-pair-mode 1)

(message "init.el loaded successfully")




















