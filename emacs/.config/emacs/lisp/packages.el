;; -*- lexical-binding: t; -*-

;;; Package Manmagement Configuration

;; Replace with built in at Emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(setopt use-package-always-ensure t)
(require 'vc-use-package)

;; Allows for ensuring a system package is present and installing if not
(use-package use-package-ensure-system-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Emacs Configuration

;;;;; emacs
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
  (setq warning-minimum-level :error)

  ;; Dont resize echo area
  (setq message-truncate-lines t)

  ;; Remember location in file
  (save-place-mode t)

  ;; Use bar cursor
  (setq-default cursor-type 'box)
  )


;;;;; exec-path-from-shell
;; Load PATH from fish config
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;;;; no-littering
;; Dont litter folders with autosave or backup files
(use-package no-littering
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;;;;; savehist
(use-package savehist
  :init
  (savehist-mode))

;;;;; recentf
(use-package recentf
  ;; :bind
  ;; ("C-x C-r" . recentf-open)
  :init
  (setq recentf-max-menu-items 15
	recentf-max-saved-items 100)
  :config
  (if (boundp 'recentf-exclude)
      (setq recentf-exclude (append recentf-exclude '("bookmark-default.el")))
    (setq recentf-exclude '("bookmark-default.el")))
  :hook
  (after-init . recentf-mode))

;;;;; nano-theme

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


;;;;; eros
;; Display eval result (including for debug) in popup instead of echo area.
;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/
(use-package eros
  :config
  (defun adviced:edebug-compute-previous-result (_ &rest r)
    "Adviced `edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
		(edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))

  (advice-add #'edebug-compute-previous-result
              :around
              #'adviced:edebug-compute-previous-result)
  
  (defun adviced:edebug-previous-result (_ &rest r)
    "Adviced `edebug-previous-result'."
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))
  
  (advice-add #'edebug-previous-result
              :around
              #'adviced:edebug-previous-result)

  (setq eval-expression-print-length nil
	eval-expression-print-level nil
	edebug-print-length nil
	edebug-print-level nil)
  
  (eros-mode))


;;;;; persistent-scratch
;; Auto save scratch buffer
(use-package persistent-scratch
  :demand t
  :config
  (persistent-scratch-setup-default))
;;;;; tramp
(use-package tramp-sh
  :ensure
  nil
  :init
  (setq tramp-use-ssh-controlmaster-options nil)
  :config
  (setq tramp-remote-path (append tramp-remote-path (list "~/.local/bin" "~/.cargo/bin"))))

;;; Help

;;;; which-key
;; Which-key shows the available keybindings after a key press	      
(use-package which-key
  :config
  (which-key-mode))

;;;; helpful
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

;;;; Casual
;;;;;; casual-isearch
;; Transient I-Search menu
(use-package casual-isearch
  :bind
  (:map isearch-mode-map
	("C-o" . casual-isearch-tmenu)))

;;; Editor

;;;; avy
;; Tree based point selection
(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-c C-j" . 'avy-resume)
   ("C-:" . 'avy-goto-char-in-line)
   ("C-;" . 'avy-goto-char-2)))

;;;; expand-region
;; C-= to expand selection intelligently
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

;;;; outli
;; Better keybindings for outline-minor-mode
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

;;;; flymake 
;; Flymake error checking
(use-package flymake
  :hook
  ((LaTeX-mode text-mode org-mode markdown-mode message-mode) . flymake-mode))

;;;; vundo
;; Visualise undo tree and step between
(use-package vundo)

;;;; popper
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
	  "eshell.*\\*$" eshell-mode
          "eat\\*" eat-mode
	  "ielm\\*" ielm-mode
	  "Python\\*" inferior-python-mode
	  "shell\\*" shell-mode)
	;; popper-group-function #'popper-group-by-directory
	)
  ;; Set min popup height to 1/3 of frame height
  (setq popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                win
                                (floor (frame-height) 3)
				(floor (frame-height) 3))))
  (popper-mode)
  (popper-echo-mode))

;;;; ibuffer-vc
;; Group buffers in ibuffer by project
(use-package ibuffer-vc
  :hook
  (ibuffer . (lambda ()
	       (ibuffer-vc-set-filter-groups-by-vc-root)
	       (unless (eq ibuffer-sorting-mode 'alphabetic)
		 (ibuffer-do-sort-by-alphabetic)))))

;;;; hl-todo
;; Highlight reminders
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;;;; consult-todo
;; Show todos in consult
(use-package consult-todo
  :bind
  ("M-g t" . consult-todo))

;;;; mixed-pitch
;; Allow mixing of variable pitch and monospaced fonts
(use-package mixed-pitch
  :config
  (setq mixed-pitch-set-height t
	mixed-pitch-fixed-pitch-faces (delete 'font-latex-sectioning-5-face mixed-pitch-fixed-pitch-faces)
	mixed-pitch-variable-pitch-cursor nil)
  :hook
  ((text-mode . mixed-pitch-mode)
   (toml-ts-mode . (lambda () (mixed-pitch-mode -1)))))

;;;; Monad Stack

;;;;; corfu
;; Use M-SPC to add Corfu separator for orderless searching
(use-package corfu
  ;; Recommended: Enable Corfu globally. This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customisation variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;;;;; cape
;; Extra completion functions for corfu
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev) 
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-tex)
  )

;;;;; orderless
;; Orderless completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
	;; Initialism alows `eif' to find elp-instrument-function
	orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))

;;;;; vertico
;; Vertico minibuffer 
(use-package vertico
  :config
  (vertico-mode)
  ;; Use different vertico displays for different completion categories
  (setq vertico-multiform-categories
	'((file reverse)
	  (minor-mode reverse)
	  (imenu buffer)
	  (jinx grid (vertico-grid-annotate . 20))
	  (consult-grep buffer)
	  (t reverse)
	  ))
  ;; Same but for commands
  (setq vertico-multiform-commands
	'((consult-location buffer)
	  (consult-flymake buffer)
	  (consult-line buffer)
	  (consult-buffer unobtrusive)
	  (consult-outline buffer)
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

(use-package vertico-multiform
  :after
  vertico
  :ensure
  nil
  :bind
  (:map vertico-map
	("C-'" . vertico-quick-exit)))


;;;;; marginalia
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


;;;;; consult
;; Stupid powerful completion package
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x C-r" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
	 ("M-i"   . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  :config
  (setq consult-preview-excluded-files '("\\`/[^/|:]+:" "\\.pdf\\'"))
  )

;;;;; wgrep
;; Allow editing grep buffers
(use-package wgrep)

;;;;; embark

;; Super powered ability to act on anything the point is over
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; TODO add karthinks code when I install avy
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-'" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map embark-file-map
   ("S" . sudo-find-file))
  :init
  ;; Press a prefix and then C-h to pull up minibuffer completion of prefix with keybindings
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :config
  (setq embark-indicators 
	'(embark-mixed-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator)
	embark-mixed-indicator-delay 2)
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (find-file (if (file-remote-p file)
		   (concat "/" (file-remote-p file 'method) ":"
			   (file-remote-p file 'user) "@" (file-remote-p file 'host)
			   "|sudo:root@"
			   (file-remote-p file 'host) ":" (file-remote-p file 'localname))
		 (concat "/sudo:root@localhost:" file)))))

;; Support for embark-{collect|export} with consult buffers
(use-package embark-consult
  :hook
  (embark-collect . consult-preview-at-point-mode))

;;; Languages

;;;; Meta

;;;;; treesit-auto
;; Automatically install treesitter languages if available
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;; hideshow
;; Code folding for non treesitter languages (elisp)
(use-package hideshow
  :hook
  (emacs-lisp-mode . hs-minor-mode))

;;;;; treesit-fold

;; Code-folding using treesitter
(use-package treesit-fold
  :vc
  (treesit-fold :url "https://github.com/emacs-tree-sitter/treesit-fold"
		:branch "master")
  :config
  (global-treesit-fold-mode))

;;;;; Custom Functions

;; Conditional toggling
;; The idea is that M-<tab> folds big things like headings and S-<tab> folds smaller things like code blocks
(defun my/conditional-big-toggle ()
  (interactive)
  "Change the function called from M-<tab> depending on the active minor modes."
  (cond
   ;; Code folding
   ((bound-and-true-p outline-minor-mode) (outline-cycle))
   (t (backward-button 1))))

(defun my/conditional-small-toggle ()
  (interactive)
  "Change the function called from S-<tab> depending on the active minor modes"
  (cond
   ((bound-and-true-p treesit-fold-mode) (treesit-fold-toggle))
   ((bound-and-true-p hs-minor-mode) (hs-toggle-hiding))
   (t (message "No minor modes match for C-<tab>"))))

(global-set-key (kbd "M-<tab>") 'my/conditional-big-toggle)
(global-set-key (kbd "C-<tab>") 'my/conditional-small-toggle)


;;;;; eglot
;; :ensure-system-package doesn't work with python packages as they are not in the PATH
;; Requires
(use-package eglot
  :bind
  (:map eglot-mode-map
	("C-c C-d" . eldoc)
	("C-c C-e" . eglot-rename)
	("C-c C-f" . eglot-format-buffer))
  :hook
  (((python-base-mode c-ts-mode) . eglot-ensure))
  :config
  (setq-default eglot-workspace-configuration
		'(:basedpyright (:disableOrganizeImports t)))
  (setq enable-remote-dir-locals t)
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode)
		 "basedpyright-langserver" "--stdio"))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
	eldoc-echo-area-display-truncation-message nil
	eldoc-echo-area-prefer-doc-buffer 'maybe
	eldoc-echo-area-use-multiline-p nil
	eldoc-idle-delay 1.0)
  
  (defun my/eglot--executable-find-advice (fn &rest args)
    "If `python-base-mode' is active and `python-shell-virtualenv-root' bound, search there first for lsp servers.
FN is `eglot--executable-find', ARGS is the arguments to `eglot--executable-find'."
    (pcase-let ((`(,command . ,_) args))
      (if (and (derived-mode-p 'python-base-mode)(member command '("pylsp" "pyls" "pyright-langserver" "jedi-language-server" "ruff-lsp" "python" "basedpyright-langserver")))
	  (if python-shell-virtualenv-root
	      (or (my/executable-find-dir command (list (expand-file-name "bin" python-shell-virtualenv-root)) t) (apply fn args))
	    (apply fn args))
	(apply fn args))))

  (advice-add 'eglot--executable-find :around #'my/eglot--executable-find-advice)
  )

;;;;; eglot-booster
;; Boost emacs using emacs-lsp-booster
;; Requires emacs-lsp-booster to be installed
(use-package eglot-booster
  :vc
  (eglot-booster :url "https://github.com/jdtsmith/eglot-booster"
		 :branch "main")
  :after eglot
  :config (eglot-booster-mode))

;;;;; jupyter
(use-package org-src
  :ensure nil
  :demand t
  :config
  (add-to-list 'org-src-lang-modes '("python" . python-ts))
  (setq org-confirm-babel-evaluate nil)) ;; Fix issue with jupyter not working with ts python

(use-package jupyter
  :after
  org-src
  :ensure-system-package
  jupyterlab
  :init
  (setq org-babel-load-languages '((emacs-lisp . t)
				   (python . t)
				   (jupyter . t)))
  (if (boundp 'recentf-exclude)
      (add-to-list 'recentf-exclude "^/tmp/jupyter")
    (setq recentf-exclude '("^/tmp/jupyter")))
  :bind
  (:map jupyter-repl-interaction-mode-map
	("C-c C-i" . jupyter-inspect-at-point)
	("C-c I" . jupyter-repl-interrupt-kernel)
	("M-i" . consult-imenu))
  (:map jupyter-org-interaction-mode-map
	("C-c C-i" . jupyter-inspect-at-point)
	("C-c I" . jupyter-org-interrupt-kernel)
	("M-i" . consult-imenu)))



;;;;; apheleia
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff-isort ruff))
  :hook
  (python-base-mode emacs-lisp-mode lisp-mode LaTeX-mode TeX-mode))

;;;; CSV

;;;;; csv-mode
(use-package csv-mode)

;;;; Python
;; Use IPython as interpreter

(use-package python
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True --profile=emacs")
  (indent-tabs-mode nil))







;;;;; eshell-venv
;; Custom package to allow Eshell venv activation
(use-package eshell-venv
  :ensure
  nil
  :hook
  (eshell-mode . eshell-venv-mode)
  )


;;;;; flymake-ruff
(use-package flymake-ruff
  :hook
  (eglot-managed-mode . flymake-ruff-load))

;;;; Shell
;;;;;; emacs-fish
;; Syntax for fish scripts
(use-package fish-mode)

;;;; Rust
;;;;;; rust-mode
;; Official Rust mode
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;;;;;; ob-rust
;; Rust in org-babel
(use-package ob-rust)

;;;; C
;;;;;; c-ts-mode
(use-package c-ts-mode
  :if
  (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux))

;;; External Tools

;;;; Git

;;;;; magit
;; The best git porcelain
(use-package magit)

;;;;; diff-hl
;; Highlight git diff in gutter
(use-package diff-hl
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-mode)
  (global-diff-hl-mode))

;;;; Terminal

;;;;; eat
;; Eat is a full terminal emulator written in elisp
(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  :custom
  (eshell-visual-commands nil))

;;;;; fish-completion
;; Allow eshell to use any fish completions
(use-package fish-completion
  :vc
  (fish-completion :url "https://github.com/LemonBreezes/emacs-fish-completion"
		   :branch "master")
  :ensure-system-package
  fish
  :config
  (global-fish-completion-mode))

;;;;; eshell

;; Need to demand vc-git and magit so that the custom eshell prompt works
;; Somewhat expensive but not the end of the word since I use daemon mode
(use-package vc-git
  :demand t
  :ensure nil)

(use-package magit
  :demand t)

(use-package eshell
  :requires
  (vc-git magit)
  :demand t
  :config
  (defun my/vc-git-state (file)
    "`vc-state' which does not include ignored files."
    (let* ((args
	    `("status" "--porcelain" "-z"
	      ;; Just to be explicit, it's the default anyway.
	      "--untracked-files"
	      "--"))
	   (status (apply #'vc-git--run-command-string file args)))
      (if (null status)
	  ;; If status is nil, there was an error calling git, likely because
	  ;; the file is not in a git repo.
	  'unregistered
	;; If this code is adapted to parse 'git status' for a directory,
	;; note that a renamed file takes up two null values and needs to be
	;; treated slightly more carefully.
	(vc-git--git-status-to-vc-state
	 (mapcar (lambda (s)
		   (substring s 0 2))
		 (split-string status "\0" t))))))
  
  
  (defun my/eshell-prompt-function ()
    (concat
     "\n"
     (propertize (replace-regexp-in-string
		  (getenv "HOME")
		  "~"
		  (eshell/pwd))
		 'face `(:foreground "#5e81ac"))
     " "
     (propertize (concat (let
			     ((git-tag (magit-get-current-tag))
			      (git-branch (magit-get-current-branch)))
			   (if git-tag
			       git-tag
			     (if git-branch
				 git-branch)))
			 (if (eq (my/vc-git-state (eshell/pwd)) 'edited)
			     "*"))
		 'face 'default)	    
     "\n"
     (propertize (if (bound-and-true-p python-shell-virtualenv-root)
		     ".venv"
		   "")
		 'face `(:foreground "#b48ead"))
     (propertize " λ " 'face 'default)))
  
  (setq eshell-prompt-function #'my/eshell-prompt-function
	eshell-prompt-regexp ".* λ "))


;;; Academic

;;;; auctex

;; AucTeX improved Tex experience
(use-package tex
  :demand t
  :ensure
  auctex
  :hook
  ((LaTeX-mode . reftex-mode))
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")
				     (output-html "xdg-open"))
	TeX-auto-save t
	TeX-parse-self t
	TeX-source-correlate-mode t
	TeX-source-correlate-start-server t))

;;;; openwith
(use-package openwith
  :config
  (openwith-mode)
  (setq openwith-associations
	'(("\\.pdf\\'" "sioyek" (file)))))

;;;; citar

;; reference management 
(use-package citar
  :demand t
  :after
  tex latex
  :custom
  (citar-bibliography '("~/Sync/Roam/biblio.bib"))
  :hook
  ((LaTeX-mode TeX-mode org-mode) . citar-capf-setup)
  :bind
  (:map LaTeX-mode-map
	("C-c c" . citar-insert-citation)
	:map TeX-mode-map
	("C-c c" . citar-insert-citation)))

(use-package embark
  :demand t)

;; embark actions
(use-package citar-embark
  :after
  citar embark
  :hook 
  ((LaTeX-mode org-mode) . citar-embark-mode))

;;;; flymake-vale

;; Use vale prose linter with Flymake
(use-package flymake-vale
  :vc
  (flymake-vale :url "https://github.com/tpeacock19/flymake-vale"
		:branch "main")
  :ensure-system-package
  vale
  :hook
  ((LaTeX-mode text-mode org-mode markdown-mode message-mode) . flymake-vale-load))

;;;; jinx

;; Jinx spell checker
(use-package jinx
  :hook
  (((text-mode LaTeX-mode org-mode prog-mode conf-mode) . jinx-mode)
   (toml-ts-mode . (lambda () (jinx-mode -1))))
  :bind
  ("M-$" . jinx-correct)
  :config
  (add-to-list 'jinx-exclude-faces
	       '(LaTeX-mode font-lock-constant-face))
  (add-to-list 'jinx-exclude-faces
	       '(prog-mode font-lock-string-face)))

;;;; powerthesaurus

;; Powerthesaurus integration
(use-package powerthesaurus
  :bind
  ("C-$" . powerthesaurus-transient))

;;;; Custom Functions

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

(defun my/executable-find-dir (command dirs &optional remote)
  "Implementation of `executable-find' which just searches DIRS."
  (if (and remote (file-remote-p default-directory))
      (let ((res (locate-file
		  command
		  (mapcar
		   (lambda (x) (concat (file-remote-p default-directory) x))
		   dirs)
		  exec-suffixes 'file-executable-p)))
	(when (stringp res) (file-local-name res)))
    (let ((default-directory (file-name-quote default-directory 'top)))
      (locate-file command dirs exec-suffixes 1))))

;; Local Variables:
;; jinx-local-words: "Dabbrev Powerthesaurus"
;; eval: (outline-hide-sublevels 2)
;; End:
