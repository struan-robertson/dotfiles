;; -*- lexical-binding: t; -*-
;; Replace with built in at emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(setopt use-package-always-ensure t)
(require 'vc-use-package)

(use-package use-package-ensure-system-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Load PATH
(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Dont litter folders with autosave filesp
(use-package no-littering
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


;; C-= to expand selection intelligently
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package nano-theme
  :vc
  (nano-theme :url "https://github.com/rougier/nano-theme"
	      :branch "master") ;; Remember that when switching to emacs 30, need to specify ":rev newest"
  :config
  (load-theme 'nano-dark t)
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
  )

(use-package magit)

(use-package which-key
  :config
  (which-key-mode))

;; Is it worth switching to zsh for eat integration? Probably not but idk
(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  :config
  (setq eshell-visual-commands nil))

(use-package tex
  :ensure
  auctex
  :hook
  (LaTeX-mode . reftex-mode)
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")
				     (output-html "xdg-open"))
	TeX-auto-save t
	TeX-parse-self t))
 
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

;; ========= Academic =========

;; TODO add citar embark
(use-package citar
  :custom
  (citar-bibliography '("~/Sync/Roam/biblio.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package nano-theme
  :vc
  (nano-theme :url "https://github.com/rougier/nano-theme"
	      :branch "master") )

(use-package flymake-vale
  :vc
  (flymake-vale :url "https://github.com/tpeacock19/flymake-vale"
		:branch "main")
  :ensure-system-package
  vale
  :hook
  (LaTeX-mode . flymake-vale-load)
  (text-mode . flymake-vale-load)
  (org-mode . flymake-vale-load)
  (markdown-mode . flymake-vale-load)
  (message-mode . flymake-vale-load))

(use-package flymake
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error)
  :hook
  (LaTeX-mode . flymake-mode)
  (text-mode . flymake-mode)
  (org-mode . flymake-mode)
  (markdown-mode . flymake-mode)
  (message-mode . flymake-mode))

(use-package jinx
  :hook
  (text-mode . jinx-mode)
  (LaTeX-mode . jinx-mode)
  (org-mode . jinx-mode)
  (prog-mode-hook . jinx-mode)
  (conf-mode . jinx-mode)
  :bind
  ("M-$" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-faces
	       '(LaTeX-mode font-lock-constant-face)))

(use-package powerthesaurus
  :bind
  ("C-$" . powerthesaurus-transient))

(defun my/toggle-writing-zen ()
  "Disable language improvement tools to allow for dumping text on the page."
  (interactive)
  (if (bound-and-true-p jinx-mode)
      (progn
	(jinx-mode -1)
	(flymake-mode -1))
    (progn
      (jinx-mode 1)
      (flymake-mode 1)))
  )

;; ==== Monad Stack ====
;; Use M-SPC to add corfu seperator for orderless searching
(use-package corfu
  ;; Recommended: Enable Corfu globally. This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; Orderless completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
	;; Initialism alows `eif' to find elp-instrument-function
	orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

(use-package vertico
  :config
  (vertico-mode)
  ;; Use different vertico displays for different modes
  (setq vertico-multiform-categories
	'((file reverse)
	  (consult-location buffer)
	  (consult-grep buffer)
	  (minor-mode reverse)
	  (imenu buffer)
	  (jinx grid (vertico-grid-annotate . 20))
	  ;; (t unobtrusive)
	  ))
  (vertico-multiform-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :bind
  ("C-x C-r" . recentf-open)
  :init
  (setq recentf-max-menu-items 15
	recentf-max-saved-items 100)
  :hook
  (after-init . recentf-mode))

(use-package emacs
  :init

  ;; ========= Vertico =========

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; ========= Corfu =========
  
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

