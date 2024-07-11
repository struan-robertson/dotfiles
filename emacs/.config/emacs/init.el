(add-to-list 'load-path "~/.config/emacs/lisp")

;;; Packages

(load "packages")
;;; Keybindings
(load "keybindings")

;;; Faces
(set-face-attribute 'variable-pitch nil :family "ShureTechMono Nerd Font Propo" :height 130)
(set-face-attribute 'default nil :font "ShureTechMono Nerd Font" :height 130)

;;; Custom Variables 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(helpful powerthesaurus use-package-ensure-system-package vertico orderless corfu multiple-cursors auctex eat which-key magit nano-theme expand-region no-littering vc-use-package))
 '(package-vc-selected-packages
   '((nano-theme :url "https://github.com/rougier/nano-theme" :branch "master")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eat-term-color-10 ((t (:inherit ansi-color-bright-black :foreground "#a3be8c"))))
 '(eat-term-color-11 ((t (:inherit ansi-color-bright-black :foreground "#ebcb8b"))))
 '(eat-term-color-12 ((t (:inherit ansi-color-bright-black :foreground "#5e81ac"))))
 '(eat-term-color-2 ((t (:inherit ansi-color-green :foreground "#a3be8c"))))
 '(eat-term-color-3 ((t (:inherit ansi-color-yellow :foreground "#ebcb8b"))))
 '(eat-term-color-4 ((t (:inherit ansi-color-blue :foreground "#5e81ac"))))
 '(eat-term-color-5 ((t (:inherit ansi-color-magenta :foreground "#b48ead"))))
 '(eat-term-color-6 ((t (:inherit ansi-color-cyan :foreground "#88c0d0"))))
 '(eat-term-color-8 ((t (:inherit nano-faded))))
 '(flymake-error ((t (:underline (:color "#bf616a" :style wave :position nil)))))
 '(flymake-warning ((t (:underline (:color "#b48ead" :style wave :position nil)))))
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#a3be8c" :weight bold))))
 '(font-latex-string-face ((t (:foreground "#d8dee9"))))
 '(font-latex-warning-face ((t (:inherit bold :foreground "#bf616a"))))
 '(font-lock-string-face ((t (:inherit nano-default :foreground "#d8dee9"))))
 '(writegood-duplicates-face ((t (:underline (:color "#b48ead" :style wave :position nil)))))
 '(writegood-passive-voice-face ((t (:underline (:color "#88c0d0" :style wave :position nil)))))
 '(writegood-weasels-face ((t (:underline (:color "#d08770" :style wave :position nil))))))

(message "init.el loaded successfully")




















