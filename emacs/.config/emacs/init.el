(add-to-list 'load-path "~/.config/emacs/")

;; === Packages ===
(load "packages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
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

(message "init.el loaded successfully")
