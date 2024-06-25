;; === Package Setup ===
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Dont litter folders with autosave filesp
(use-package no-littering
  :ensure t
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; C-= to expand selection intelligently
(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

(use-package nano-theme
  :ensure t
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

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
