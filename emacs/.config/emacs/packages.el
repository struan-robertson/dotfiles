;; === Package Setup ===
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-vc-prefer-newest t)

;; Dont litter folders with autosave filesp
(use-package no-littering
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; C-= to expand selection intelligently
(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

(use-package nano-theme
  :vc (:url "https://github.com/rougier/nano-theme"
	    :branch "master")
  :config
  (load-theme 'nano-dark t))
;; Fix theme not being set on terminal clients
(defvar my:theme 'nano-dark)
(defvar my:theme-window-loaded nil)
(defvar my:theme-terminal-loaded nil)
(if (daemonp)
    (add-hook 'after-make-frame-functions(lambda (frame)
                                          (select-frame frame)
                                          (if (window-system frame)
                                              (unless my:theme-window-loaded
                                                (if my:theme-terminal-loaded
                                                    (enable-theme my:theme)
                                                  (load-theme my:theme t))
                                                (setq my:theme-window-loaded t)
                                                )
                                            (unless my:theme-terminal-loaded
                                              (if my:theme-window-loaded
                                                  (enable-theme my:theme)
                                                (load-theme my:theme t))
                                              (setq my:theme-terminal-loaded t)
                                              )
                                            )))

  (progn
    (load-theme my:theme t)
    (if (display-graphic-p)
        (setq my:theme-window-loaded t)
      (setq my:theme-terminal-loaded t)))
)

(use-package magit)

(use-package which-key
  :config
  (which-key-mode))

;; Is it worth switching to zsh for eat integration? Probably not but idk
(use-package eat
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (setq eshell-visual-commands nil))

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection '((output-pdf "Sioyek")
				    (output-html "xdg-open"))))

