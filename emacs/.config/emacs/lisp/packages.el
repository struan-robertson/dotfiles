;; -*- lexical-binding: t; -*-

;;; Package Manmagement Configuration

;; Replace with built in at Emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(setopt use-package-always-ensure t)
(require 'vc-use-package)

(use-package use-package-ensure-system-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Emacs

;; Emacs configuration
(use-package emacs
  :init

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Pair specific chars
  (electric-pair-mode 1)

  ;; Dired DWIM path selection
  (setq dired-dwim-target t)

  ;; Minimum warning level
  (setq warning-minimum-level :error))

;; Load PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Dont litter folders with autosave or backup files
(use-package no-littering
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/")))))

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

;;;; Theming

;; Nano theme 
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
	(setq my:theme-terminal-loaded t)))))

	      
;;; Help

;; Which-key shows the available keybindings after a key press	      
(use-package which-key
  :config
  (which-key-mode))

;; More helpful help buffers
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h F" . helpful-function)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point)))

;;; Editing

;; C-= to expand selection intelligently
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

;; ? for speed command help
(use-package outli
  :vc
  (outli :url "https://github.com/jdtsmith/outli"
	 :branch "main")
  :bind
  (:map outli-mode-map
	("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook
  ((prog-mode text-mode) . outli-mode)
  :config
  (setq outli-blend nil))

(use-package flymake
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error)
  :hook
  ((LaTeX-mode text-mode org-mode markdown-mode message-mode) . flymake-mode))

;; TODO learn better
(use-package vundo)

;;;; Monad Stack

;; Use M-SPC to add Corfu separator for orderless searching
(use-package corfu
  ;; Recommended: Enable Corfu globally. This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customisation variable
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
  (vertico-multiform-mode)
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Rich annotations in minibuffer
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;; Languages

;;;; Meta

;; Automatically install treesitter languages if available
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Expand to other modes when required
(defun my/conditional-toggle ()
  (interactive)
  "Change the function called from M-<tab> depending on the active minor modes."
  (cond
   ;; Code folding
   ((bound-and-true-p outline-minor-mode) (outline-cycle))
   (t (backward-button 1))))

(global-set-key (kbd "M-<tab>") 'my/conditional-toggle)

;; Code-folding using treesitter
(use-package treesit-fold
  :vc
  (treesit-fold :url "https://github.com/emacs-tree-sitter/treesit-fold"
		:branch "master")
  :config
  (global-treesit-fold-mode)
  :bind
  ("C-<tab>" . treesit-fold-toggle))

(use-package eglot
  :bind
  (:map eglot-mode-map
	("C-c C-d" . eldoc)
	("C-c C-e" . eglot-rename))
  :hook
  ((python-base-mode . eglot-ensure))
  :config
    (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :pydocstyle (:enabled t
                                                    :convention "numpy")
                                       :yapf (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)
				       :ruff (:enabled t
					      :line_length 88))))))
  )

;;;; CSV

;; CSV mode
(use-package csv-mode)

;;;; Python

(use-package eshell-pet
  :ensure
  nil
  :hook
  (eshell-mode . eshell-pet-mode))
  
(use-package pet
  :ensure-system-package
  dasel
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  )

;; (use-package eshell
;;   :after
;;   (pet vc-git)
;;   :hook
;;   (eshell-mode . eshell-pet-mode)
;; )

;;; External Tools

;;;; Git
(use-package magit)

;;;; Terminal

;; Eat is a full terminal emulator written in elisp
(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  :custom
  (eshell-visual-commands nil))

;; Make specific buffers pop up
(use-package popper
  :bind
  (("M-`"	.	popper-toggle)
   ("C-`"	.	popper-cycle)
   ("C-M-`"	.	popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
	  "^\\*eshell.*\\*$" eshell-mode
          "^\\*eat\\*" eat-mode
	  "^\\*ielm\\*" ielm-mode)
	;; popper-group-function #'popper-group-by-directory
	)
  (setq popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                win
                                (floor (frame-height) 3)
				(floor (frame-height) 3))))
  (popper-mode)
  (popper-echo-mode))

;; Allow eshell to use any fish completions
(use-package fish-completion
  :vc
  (fish-completion :url "https://github.com/LemonBreezes/emacs-fish-completion"
		   :branch "master")
  :ensure-system-package
  fish
  :config
  (global-fish-completion-mode))

;; (use-package eshell-prompt-extras
;;   :requires
;;   virtualenvwrapper
;;   :config
;;   (with-eval-after-load "esh-opt"
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-prompt-function 'epe-theme-lambda)))


;;; Academic

;; AucTeX improved Tex experience
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
 
;; TODO add citar embark
;; Reference management 
(use-package citar
  :custom
  (citar-bibliography '("~/Sync/Roam/biblio.bib"))
  :hook
  ((LaTeX-mode org-mode) . citar-capf-setup)
)

;; Use vale prose linter with Flymake
(use-package flymake-vale
  :vc
  (flymake-vale :url "https://github.com/tpeacock19/flymake-vale"
		:branch "main")
  :ensure-system-package
  vale
  :hook
  ((LaTeX-mode text-mode org-mode markdown-mode message-mode) . flymake-vale-load))

;; Jinx spell checker
(use-package jinx
  :hook
  ((text-mode LaTeX-mode org-mode prog-mode conf-mode) . jinx-mode)
  :bind
  ("M-$" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-faces
	       '(LaTeX-mode font-lock-constant-face))
  (add-to-list 'jinx-exclude-faces
	       '(prog-mode font-lock-string-face)))

;; Powerthesaurus integration
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

;; Local Variables:
;; jinx-local-words: "Dabbrev Powerthesaurus"
;; eval: (outline-hide-sublevels 2)
;; End:
