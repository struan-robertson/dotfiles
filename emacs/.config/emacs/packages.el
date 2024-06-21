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

(use-package magit
  :ensure t)
