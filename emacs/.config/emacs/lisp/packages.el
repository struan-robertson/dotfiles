;; -*- lexical-binding: t; -*-

;; ;;; Package Manmagement Configuration
(setopt use-package-always-ensure t)

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;;; Custom Functions

(defun doas-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting doas"))
  (find-file (if (file-remote-p file)
		 (concat "/" (file-remote-p file 'method) ":"
			 (file-remote-p file 'user) "@" (file-remote-p file 'host)
			 "|doas:root@"
			 (file-remote-p file 'host) ":" (file-remote-p file 'localname))
	       (concat "/doas:root@localhost:" file))))

(defmacro my/execute-locally (sexp)
  "Execute SEXP on the local machine, even when the buffer is associated with a remote TRAMP host."
  `(let ((default-directory "~/"))
     ,sexp))

(defun my/conditional-toggle ()
  (interactive)
  "Change the function called from S-<tab> depending on the active minor modes"
  (cond
   ((bound-and-true-p treesit-fold-mode) (treesit-fold-toggle))
   ((bound-and-true-p hs-minor-mode) (hs-toggle-hiding))
   (t (message "No minor modes match for C-<tab>"))))
(global-set-key (kbd "C-<tab>") 'my/conditional-toggle)

(defun my/detect-venv (dir)
  "Check if a .venv directory exists either at project root of DIR or at DIR.
If so, return path to .venv/bin"
  (let
      ;; Expands to just $PWD/.venv/bin if not in a git repo
      ((venv (expand-file-name ".venv" (vc-git-root dir))))
    (if (file-directory-p venv)
	venv
      nil)))

(defmacro my/execute-with-venv-vars (sexp venv)
  "Execute SEXP with virtual environment at VENV and set appropriate variables."
  `(let* ((venv-bin (file-name-concat ,venv "bin"))
	  (remote-host (file-remote-p default-directory))
	  (local-venv-bin (tramp-file-local-name venv-bin))
	  (local-local-bin (tramp-file-local-name
			    (if remote-host
				(tramp-handle-expand-file-name (concat remote-host "~/.local/bin"))
			      (expand-file-name "~/.local/bin"))))
	  (local-cargo-bin (tramp-file-local-name
			    (if remote-host
				(tramp-handle-expand-file-name (concat remote-host "~/.cargo/bin"))
			      (expand-file-name "~/.cargo/bin"))))
          (exec-path (cons venv-bin exec-path))
          (python-shell-virtualenv-root ,venv)
	  (process-environment (append (list
					(format "PATH=%s:%s:%s:%s"
						local-venv-bin
						local-local-bin
						local-cargo-bin
						(getenv "PATH"))
					(format "VIRTUAL_ENV=%s" venv))
				       process-environment))
	  (tramp-remote-path (append local-venv-bin
				     local-local-bin
				     local-cargo-bin
				     tramp-remote-path))
	  (tramp-remote-process-environment (cons (format "VIRTUAL_ENV=%s" venv) tramp-remote-process-environment)))
     ,sexp))


(defun my/toggle-writing-zen ()
  "Disable language improvement tools to allow for dumping text on the page."
  (interactive)
  (if (bound-and-true-p jinx-mode)
      (progn
	(jinx-mode -1)
	(flymake-mode -1))
    (progn
      (jinx-mode 1)
      (flymake-mode 1))))


;;; Emacs Configuration

;;;;; emacs
(use-package emacs
  :ensure nil
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

  ;; Use box cursor
  (setq-default cursor-type 'box)

  :custom
  ;; Improve performance by decreasing the number of garbage collections
  ;; This increases memory pressure, but I have plenty RAM
  ;; Do not increase the original value (800000) by a factor of more than 100
  (gc-cons-threshold (* 800000 50))

  ;; Set date style
  (calendar-date-style 'european))


;;;;; Built In

;;;;;; bookmark
;; Built in bookmark package
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)  ;; Save bookmark list after every change
  :hook
  (bookmark-bmenu-mode . hl-line-mode))

;;;;;; ibuffer
;; Built in ibuffer package
(use-package ibuffer
  :ensure nil
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  (ibuffer-mode . hl-line-mode)
  :bind
  ("C-x C-b" . ibuffer))

;;;;;; savehist
;; Built in
(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

;;;;;; recentf
;; Built in
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-menu-items 15
	recentf-max-saved-items 100)
  :config
  (if (boundp 'recentf-exclude)
      (setq recentf-exclude (append recentf-exclude '("bookmark-default.el")))
    (setq recentf-exclude '("bookmark-default.el")))
  :hook
  (elpaca-after-init . recentf-mode))

;;;;;; transient
;; Built in version is too low for upstream packages that depend on it
(use-package transient)

;;;;;; tramp
;; Use system ssh settings and search .local paths on remote
(use-package tramp-sh
  :ensure nil
  :init
  (setq tramp-use-connection-share nil)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;;;;; flymake 
;; Flymake error checking
(use-package flymake
  :ensure nil
  :hook
  ((LaTeX-mode markdown-mode shell-mode bash-ts-mode) . flymake-mode))

;;;;;; jsonrpc
;; Built in version is too low for upstream packages that depend on it
(use-package jsonrpc)

;;;;;; project
(use-package project
  :ensure nil
  :config
  (setq project-switch-commands '((project-find-file "Find file")
				  (project-find-regexp "Find regexp")
				  (project-find-dir "Find directory")
				  (magit-project-status "Magit" ?m)
				  (project-eshell "Eshell")
				  (eat-project "Shell" ?s))))

;;;;; External


;;;;;; no-littering
;; Dont litter folders with autosave or backup files
(use-package no-littering
  :init
  (setq user-emacs-directory "~/.config/emacs")
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;;;;;; nano-theme
;; Nano theme 
(use-package nano-theme
  :ensure
  (:host github :repo "rougier/nano-theme" :branch "master")
  :config
  (nano-dark)
  :custom
  (nano-dark-foreground "#bbc6d9"))

;;;;;; eros
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


;;;;;; persistent-scratch
;; Auto save scratch buffer
(use-package persistent-scratch
  :demand t
  :config
  (persistent-scratch-setup-default))

;;;;;; openwith
;; Open file type with external program instead of Emacs
(use-package openwith
  :config
  (openwith-mode)
  (setq openwith-associations
	'(("\\.pdf\\'" "sioyek" (file))))
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler))

;;;;;; ace-window
;; More efficient window selection
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame))

;;;;;; ultra-scroll
;; Smoother scrolling
(use-package ultra-scroll
  :ensure
  (:host github :repo "jdtsmith/ultra-scroll" :branch "main") 
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

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
   ("C-c C-d" . helpful-at-point))
  :hook
  (helpful-mode . hl-line-mode))

;;; Editor

;;;; avy
;; Tree based point selection
(use-package avy
  :config
  (avy-setup-default)
  :bind
  (("C-c C-j" . 'avy-resume)
   ("C-M-;" . 'avy-goto-char-in-line)
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
  :ensure
  (:host github :repo "jdtsmith/outli" :branch "main")
  :bind
  (:map outli-mode-map
	("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook
  ((prog-mode text-mode) . outli-mode)
  (outli-mode . (lambda () (outline-hide-sublevels 2)))
  :config
  (setq outli-blend nil))

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
          compilation-mode
	  "eshell.*\\*$" ;; eshell-mode
          "eat\\*" ;; eat-mode
	  "ielm\\*" ielm-mode
	  "Python\\*" inferior-python-mode
	  "julia\\*"
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
  :config
  (setq ibuffer-vc-skip-if-remote nil)
  :hook
  (ibuffer . (lambda ()
	       (ibuffer-vc-set-filter-groups-by-vc-root)
	       (unless (eq ibuffer-sorting-mode 'alphabetic)
		 (ibuffer-do-sort-by-alphabetic)))))

;;;; diminish
;; Hide specific minor-modes from the modeline
(use-package diminish)

;;;; hl-todo
;; Highlight reminders
(use-package hl-todo
  :config
  (global-hl-todo-mode))

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
	       '(prog-mode font-lock-string-face))
  :custom
  (jinx-languages "en_GB"))

;;;; Monad Stack

;;;;; corfu
;; Use M-SPC to add Corfu separator for orderless searching
(use-package corfu
  ;; Recommended: Enable Corfu globally. This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customisation variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
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
	  (consult-line-multi buffer)
	  (consult-outline buffer)
	  (mu4e-context-switch flat)
	  (mu4e flat)
	  (mu4e-view-action flat)
	  (mu4e-compose-new flat)
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
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
	("C-'" . vertico-quick-exit)))


;;;;; marginalia
;; Rich annotations in minibuffer
(use-package marginalia
  :init
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
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
	 ;; Search accross multiple buffers
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

  (setq consult-preview-excluded-files '("\\`/[^/|:]+:" "\\.pdf\\'"))

  (defun buffer-remote-p (buf)
    "Return t when BUF is remote."
    (if-let ((fp (buffer-file-name buf)))
	(file-remote-p fp)
      nil))

  (setq consult-preview-excluded-buffers 'buffer-remote-p))

;;;;; consult-todo
;; Show todos in consult
(use-package consult-todo
  :bind
  ("M-g t" . consult-todo))

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
   ("S" . doas-find-file)
   :map org-mode-map
   ("C-'" . embark-dwim))
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
	embark-mixed-indicator-delay 2))


;; Use helpful.el keybindings if installed
(use-package embark
  :ensure nil
  :if (elpaca-installed-p 'helpful)
  :bind
  (:map embark-command-map ("h" . helpful-symbol)
	:map embark-function-map ("h" . helpful-symbol)
	:map embark-symbol-map ("h" . helpful-symbol)
	:map embark-variable-map ("h" . helpful-symbol)))

;; Support for embark-{collect|export} with consult buffers
(use-package embark-consult
  :hook
  (embark-collect . consult-preview-at-point-mode))

;;;; gptel
;; LLM support in emacs

(use-package gptel
  :config
  ;; Together.ai offers an OpenAI compatible API
  (setf (gptel-get-backend "ChatGPT") nil)
  (setq
   gptel-model   'deepseek-ai/DeepSeek-R1
   gptel-default-mode 'org-mode
   gptel-backend (gptel-make-openai "DeepSeek"         ; Any name you want
		   :host "api.together.xyz"
		   :key (my/execute-locally (shell-command-to-string "gpg -q --for-your-eyes-only --no-tty -d ~/.config/emacs/together_api_key.gpg"))                   ; Can be a function that returns the key
		   :stream t
		   :models '(;; has many more, check together.ai
			     deepseek-ai/DeepSeek-R1
			     deepseek-ai/DeepSeek-V3))
   pulse-flag t
   gptel-prompt-prefix-alist '((markdown-mode . "# ")
			       (org-mode . "* ")
			       (text-mode . "# "))
   gptel-include-reasoning nil)

  ;; Org latex previews get messed up if `default-directory' of a buffer is on a remote machine
  (defun my/local-gptel ()
    (interactive)
    "Run `gptel' command on local machine."
    (my/execute-locally (call-interactively 'gptel)))

  (defun my/local-gptel-menu ()
    (interactive)
    "run `gptel-menu' command on local machine"
    (my/execute-locally (call-interactively 'gptel-menu)))

  (defun my/local-gptel-add ()
    (interactive)
    "run `gptel-add' command on local machine"
    (my/execute-locally (call-interactively 'gptel-add)))

  (setq gptel-api-key (my/execute-locally (shell-command-to-string "gpg -q --for-your-eyes-only --no-tty -d ~/.config/emacs/together_api_key.gpg")))

  (define-prefix-command 'my/gptel-map)
  :bind-keymap ("C-x c" . my/gptel-map)
  :bind (:map my/gptel-map
	      ("c" . 'my/local-gptel)
	      ("m" . 'my/local-gptel-menu)
	      ("a" . 'my/local-gptel-add)))

;;;; move-text
;; Move text or region up or down using M-<up> or M-<down>
(use-package move-text
  :config
  (move-text-default-bindings))

;;; Org
;;;; org
;; Organisational fun
(use-package org
  :ensure nil
  :config
  (cond ((string= (shell-command-to-string "hostname") "alpinelaptop\n")
	 (plist-put org-format-latex-options :scale 0.66))
	((string= (shell-command-to-string "hostname") "alpine\n")
	 (plist-put org-format-latex-options :scale 1.5)))

  ;; Org TODOs
  (defun my/org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
	(setq should-skip-entry t))
      (save-excursion
	;; If previous sibling exists and is TODO,
	;; skip this entry
	(while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (let ((num-ancestors (org-current-level))
            (ancestor-level 1))
	(while (and (not should-skip-entry) (<= ancestor-level num-ancestors))
          (save-excursion
            ;; When ancestor (parent, grandparent, etc) exists
            (when (ignore-errors (outline-up-heading ancestor-level t))
              ;; If ancestor is WAITING, skip entry
              (if (string= "WAITING" (org-get-todo-state))
                  (setq should-skip-entry t)
		;; Else if ancestor is TODO, check previous siblings of
		;; ancestor ("uncles"); if any of them are TODO, skip
		(when (org-current-is-todo)
                  (while (and (not should-skip-entry) (org-goto-sibling t))
                    (when (org-current-is-todo)
                      (setq should-skip-entry t)))))))
          (setq ancestor-level (1+ ancestor-level))
          ))
      (when should-skip-entry
	(or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))
  
  (add-to-list 'org-modules 'org-habit t)
  :custom
  ;; LaTeX
  (org-preview-latex-default-process 'dvisvgm)
  (org-startup-with-latex-preview t)
  (org-preview-latex-image-directory ".ltximg/")

  ;; Layout
  (org-startup-folded t)
  (org-startup-indented t)
  (org-indent-mode-turns-on-hiding-stars nil)
  
  ;; TODOs
  (org-agenda-custom-commands
   '(("n" "Next Actions" ((tags-todo "@doctorate"
				     ((org-agenda-overriding-header "Doctorate")
				      (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
			  (tags-todo "@home"
				     ((org-agenda-overriding-header "Home")
				      (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))))
     ("d" "Doctorate" tags-todo "@doctorate"
      ((org-agenda-overriding-header "Doctorate")
       (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
     ("h" "Home" tags-todo "@home"
      ((org-agenda-overriding-header "Home")
       (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
     ("p" "Personal" tags-todo "@personal"
      ((org-agenda-overriding-header "Personal")
       (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)))
     ("f" "First Action" tags-todo "@first"
      ((org-agenda-overriding-header "First Action")))
     ("W" "Waiting" todo "WAITING")))

  (org-stuck-projects '("+LEVEL>=2+LEVEL<=3-@notstuck/-CANCELLED-DONE" ("TODO" "WAITING") nil ""))
  
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
			      (todo . " %i %-12:c%l")
			      (tags . " %i %-12:c%l")
			      (search . " %i %-12:c%l")))
  (org-agenda-files '("~/Sync/Notes/Tasks/projects.org"
		      "~/Sync/Notes/Tasks/inbox.org"
		      "~/Sync/Notes/Tasks/ticker.org"))
  (org-refile-targets '(("~/Sync/Notes/Tasks/projects.org" :maxlevel 2)
			("~/Sync/Notes/Tasks/inbox.org" :level 1)
			("~/Sync/Notes/Tasks/future.org" :maxlevel 2)))
  (org-archive-location "~/Sync/Notes/Tasks/archive.org::datetree/* Finished Tasks")
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))
  (org-todo-keyword-faces '(("WAITING" . org-warning)))
  (org-tag-alist '(("@doctorate" . ?d)
		   ("@home" . ?h)
		   ("@personal" . ?p)))
  (org-refile-use-outline-path 'file)
  ;; Doesn't work with vertico yet
  (org-outline-path-complete-in-steps nil)

  (org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/Sync/Notes/Tasks/inbox.org" "Unfiled")
                            "* TODO %i%?" :empty-lines 2)
			   ("l" "Todo [inbox] linked" entry
			    (file+headline "~/Sync/Notes/Tasks/inbox.org" "Unfiled")
			    "* TODO %i%?\n %a" :empty-lines 2)
                           ("T" "Ticker" entry
                            (file "~/Sync/Notes/Tasks/ticker.org")
                            "* TODO %i%?" :empty-lines 2)))
  
  :hook
  (org-mode . visual-line-mode)
  :bind
  ("C-x M-a" . org-agenda)
  ("C-x M-l" . org-store-link)
  ("C-x M-c" . org-capture)
  ("C-x M-s" . org-switchb))

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
;; Code folding for non treesitter languages (elisp etc)
;; Built in
(use-package hideshow
  :ensure nil
  :hook
  (emacs-lisp-mode . hs-minor-mode))

;;;;; treesit-fold

;; Code-folding using treesitter
(use-package treesit-fold
  :ensure
  (:host github :repo "emacs-tree-sitter/treesit-fold" :branch "master")
  :config
  (global-treesit-fold-mode))

;;;;; eglot
;; LSP protocol for emacs
(use-package eglot
  :ensure nil
  :bind
  (:map eglot-mode-map
	("C-c C-d" . eldoc)
	("C-c C-e" . eglot-rename)
	("C-c C-f" . eglot-format-buffer))
  :hook
  (((python-base-mode c-ts-mode) . eglot-ensure))
  :config
  (setq enable-remote-dir-locals t)
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode)
		 "pyright-langserver" "--stdio"))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
	eldoc-echo-area-display-truncation-message nil
	eldoc-echo-area-prefer-doc-buffer 'maybe
	eldoc-echo-area-use-multiline-p nil
	eldoc-idle-delay 1.0)
  
  (defun my/eglot--connect-advice (fn &rest args)
    (if (derived-mode-p 'python-base-mode)
	(when-let ((venv (my/detect-venv default-directory)))
	  (my/execute-with-venv-vars
	   (apply fn args)
	   venv))
      (apply fn args)))
  
  (advice-add 'eglot--connect :around #'my/eglot--connect-advice))


;;;;; eglot-booster
;; Faster processing of LSP JSON
;; Requires emacs-lsp-booster executable to be installed
(use-package eglot-booster
  :ensure
  (:host github :repo "jdtsmith/eglot-booster" :branch "main")
  :after eglot
  :config (eglot-booster-mode))


;;;;; apheleia
;; Auto format files on save
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff-isort ruff))
  (setq apheleia-remote-algorithm 'local)
  :hook
  (python-base-mode emacs-lisp-mode lisp-mode LaTeX-mode TeX-mode))

;;;;; comint-mime
;; Display graphics and other MIME attachments in Emacs shells
(use-package comint-mime
  :hook
  (inferior-python-mode . comint-mime-setup))

;;;;; eshell-venv
;; Custom package to allow Eshell venv activation
(use-package eshell-venv
  :ensure nil
  :hook
  (eshell-mode . eshell-venv-mode))

;;;;; flymake-ruff
;; Allow ruff file checking using flymake
(use-package flymake-ruff
  :ensure
  (:host github :repo "erickgnavar/flymake-ruff"
	 :remotes ("fork" :repo "struan-robertson/flymake-ruff"
		   :protocol ssh
		   :branch "master"))
  :config
  (add-hook 'eglot-managed-mode-hook #'flymake-ruff-load))


;;;; csv-mode
;; Easier viewing of CSV files in Emacs
(use-package csv-mode)

;;;; python
;; Built in python.el package
(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True --profile=emacs --classic"
	python-indent-offset 4
	python-indent-def-block-scale 1
	python-indent-guess-indent-offset-verbose nil)
  (indent-tabs-mode nil)

  (defun my/run-python-advice (fn &rest args)
    (if (derived-mode-p 'python-base-mode)
	(when-let ((venv (my/detect-venv default-directory)))
	  (my/execute-with-venv-vars
	   (apply fn args)
	   venv))
      (apply fn args)))

  (advice-add 'run-python :around #'my/run-python-advice)
  :bind (:map python-ts-mode-map
	      ("C-c C-c" . python-shell-send-statement)
	      ("C-c C-b" . python-shell-send-buffer))
  :hook
  (inferior-python-mode . (lambda () (setq-local electric-pair-inhibit-predicate (lambda
										   (char)
										   (if (char-equal ?\( char)
										       t
										     nil))))))

;;;; Julia
;;;;; julia-mode
;; Official mode for Julia language
(use-package julia-mode)

;;;;; julia-snail
;; An Emacs development environment for Julia 
(use-package julia-snail
  :ensure t
  :custom
  (julia-snail-terminal-type :eat)
  (julia-snail-use-emoji-mode-lighter nil)
  (julia-snail-extra-args "--threads=auto")
  :hook
  (julia-mode . julia-snail-mode))

;;;; Rust
;;;;; rust-mode
;; Official Rust mode
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;;;;; ob-rust
;; Rust in org-babel
(use-package ob-rust)

;;;; C
;;;;; c-ts-mode
;; Built-in C ts-mode
(use-package c-ts-mode
  :ensure nil
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux)
  :init
  ;; Remap the standard C mode
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

;; Ensure it is added to org babel
(use-package c-ts-mode
  :ensure nil
  :if (treesit-language-available-p 'c)
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("C" . c-ts)))

;;;; Toml
;;;;; toml-ts-mode
;; Treesitter for TOML
(use-package toml-ts-mode
  :ensure nil
  :custom
  (toml-ts-mode-indent-offset 4))

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
   (magit-post-refresh . diff-hl-magit-post-refresh)))

;;;; Terminal

;;;;; eat
;; Eat is a full terminal emulator written in elisp
(use-package eat
  :hook
  (eshell-mode . eat-eshell-mode)
  :custom
  (eshell-visual-commands nil)
  (eat-tramp-shells '(("docker" . "/bin/sh")
		      ("ssh" . "/bin/bash")
		      ("doas" . "/bin/sh")))
  :config
  (customize-set-variable ;; has :set code and needs eat-semi-char-non-bound-keys to be bound
   'eat-semi-char-non-bound-keys
   (append
    (list (vector meta-prefix-char ?o)   ;; Ace window
	  (vector ?\e ?`))  ;; Popper
    eat-semi-char-non-bound-keys))
  (customize-set-variable ;; has :set code and needs eat-semi-char-non-bound-keys to be bound
   'eat-eshell-semi-char-non-bound-keys
   (append
    (list (vector meta-prefix-char ?o)   ;; Ace window
	  (vector ?\e ?`))  ;; Popper
    eat-eshell-semi-char-non-bound-keys)))


;;;;; EShell
;;;;;; em-hist
;; EShell history
(use-package em-hist
  :demand t
  :ensure nil
  :after eshell
  :hook
  (kill-emacs . eshell-save-some-history))

;;;;;; eshell
;; Elisp based emacs shell
(use-package eshell
  :demand t
  :ensure nil
  :config
  (defun my/eshell-prompt-function ()
    (concat
     "\n"
     (propertize (replace-regexp-in-string
		  (getenv "HOME")
		  "~"
		  (eshell/pwd))
		 'face `(:foreground "#5e81ac"))
     "\n"
     (propertize (if (bound-and-true-p python-shell-virtualenv-root)
		     ".venv"
		   "")
		 'face `(:foreground "#b48ead"))
     (propertize " λ " 'face 'default)))
  (setq eshell-prompt-function #'my/eshell-prompt-function
	eshell-prompt-regexp ".* λ ")

  ;;Aliases
  (setq eshell-command-aliases-list '(("ll" "ls -l")
				      ("la" "ls -al")))
  (add-to-list 'eshell-modules-list 'eshell-tramp))

;;;; IRC
;;;;;; circe
;; IRC in Emacs
(use-package circe
  :custom
  (circe-network-options
   '(("Libera Chat"
      :tls t
      :nick "struan"
      :channels ("#emacs" "#emacs-til"))
     ("OFTC"
      :tls t
      :nick "struan"
      :channels ("#alpine-linux" "#alpine-devel" "#alpine-offtopic")))))

;;;; Accounting

;;;;;; ledger-mode
;; Emacs more for ledger plain text accounting
(use-package ledger-mode
  :config
  (defun insert-pound-sterling ()
    "Insert a pound sterling symbol (£) at the current cursor position."
    (interactive)
    (insert "£"))
  :bind
  (:map ledger-mode-map
	("C-c C-4" . insert-pound-sterling)))

;;;; Files
;;;;; dired-rsync
(use-package dired-rsync
  :bind (:map dired-mode-map
	      ("C-c C-r" . dired-rsync)))

;;;;; dired-rsync-transient
(use-package dired-rsync-transient
  :bind (:map dired-mode-map
	      ("C-c C-x" . dired-rsync-transient)))

;;;; Email
;;;;; mu4e
;; Read mail based on the mu mail indexing system
(use-package mu4e
  :ensure nil
  :demand t
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :config
  (setq mu4e-maildir "~/.local/share/mail"
	mu4e-headers-date-format "%d/%m/%y"
	mu4e-contexts (list
		       ;; Personal account
		       (make-mu4e-context
			:name "Personal"
			:match-func
			(lambda (msg)
			  (when msg
			    (string-prefix-p "/purelymail" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "contact@struan.tech")
				(mu4e-sent-folder . "/purelymail/Sent")
				(mu4e-drafts-folder . "/purelymail/Drafts")
				(mu4e-trash-folder . "/purelymail/Trash")
				(mu4e-refile-folder . "/purelymail/Archive")
				(mu4e-maildir-shortcuts . ((:maildir "/purelymail/Inbox" :key ?i)
							   (:maildir "/purelymail/Sent" :key ?s)
							   (:maildir "/purelymail/Trash" :key ?t)
							   (:maildir "/purelymail/Drafts" :key ?d)
							   (:maildir "/purelymail/Archive" :key ?a))))
			:enter-func '(lambda () (when (bound-and-true-p org-msg-mode) (org-msg-mode -1))))
		       ;; University outlook account
		       (make-mu4e-context
			:name "University"
			:match-func
			(lambda (msg)
			  (when msg

			    (string-prefix-p "/dundee" (mu4e-message-field msg :maildir))))
			:vars '((user-mail-address . "s.j.y.robertson@dundee.ac.uk")
				(mu4e-sent-folder . "/dundee/Sent")
				(mu4e-drafts-folder . "/dundee/Drafts")
				(mu4e-trash-folder . "/dundee/Trash")
				(mu4e-refile-folder . "/dundee/Archive")
				(mu4e-maildir-shortcuts . ((:maildir "/dundee/Inbox" :key ?i)
							   (:maildir "/dundee/Sent" :key ?s)
							   (:maildir "/dundee/Trash" :key ?t)
							   (:maildir "/dundee/Drafts" :key ?d)
							   (:maildir "/dundee/Archive" :key ?a)))
				)
			:enter-func '(lambda () (unless (bound-and-true-p org-msg-mode) (org-msg-mode 1)))
			))
	
	mu4e-update-interval 300
	mu4e-get-mail-command "mbsync -c ~/.config/emacs/mail/mbsyncrc -a"
	mu4e-change-filenames-when-moving t

	;; Compose settings
	user-full-name "Struan Robertson"

	;; Use vertico instead of built in mu4e completing read functions
	mu4e-completing-read-function 'completing-read
	mu4e-read-option-use-builtin nil
	
	;; Use msmtp for sending mail
	message-send-mail-function 'message-send-mail-with-sendmail
	sendmail-program "msmtp"
	message-sendmail-envelope-from 'header

	;; Delete email buffer on exit
	message-kill-buffer-on-exit t
	
	;; Enable HTML rendering
	mu4e-view-show-images t
	mu4e-view-show-addresses t
	
	;; Enable threading
	mu4e-headers-show-threads t

	;; Bookmarks
	mu4e-bookmarks
	'((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :key ?w))
	
	;; Save attachments to Downloads
	mu4e-attachment-dir "~/Downloads")

  ;; Prefer plaintext in multi-mime emails
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")
    (add-to-list 'mm-discouraged-alternatives "multipart/related"))

  (define-prefix-command 'my/mu4e-map)
  :bind-keymap ("C-x m" . my/mu4e-map)
  :bind (:map my/mu4e-map
	      ("m" . mu4e)
	      ("c" . mu4e-compose-new)))

;;;;; mu4e-icalendar
;; Accept icalendar invites with mu4e
(use-package mu4e-icalendar
  :ensure nil
  :config
  (gnus-icalendar-setup)
  (setq mu4e-icalendar-trash-after-reply t))

;;;;; org-msg
;; Use org-mode to compose HTML emails
(use-package org-msg
  :ensure
  (:host github :repo "jeremy-compostella/org-msg"
	 :remotes ("pull_request" :repo "spencerjackson/org-msg"
		   :branch "remove_forbidden_properties"))
  :config
  (setq mail-user-agent 'mu4e-user-agent
	org-msg-default-alternatives '((new		. (text html))
				       (reply-to-html	. (text html))
				       (reply-to-text	. (text)))
	org-msg-convert-citation t)

  ;; Fix issue with message being saved to drafts
  (defun org-msg-edit-mode-mu4e ()
    "Setup mu4e faces, addresses completion and run mu4e."
    (org-msg--mu4e-fun-call "compose-remap-faces")
    (unless (mu4e-running-p)
      (org-msg--mu4e-fun-call "start"))
    (when mu4e-compose-complete-addresses
      (org-msg--mu4e-fun-call "compose-setup-completion"))))

;;; Academic

;;;; auctex
;; AucTeX improved Tex experience
(use-package tex
  :demand t
  :ensure
  (:repo "https://git.savannah.gnu.org/git/auctex.git"
	 :branch "main"
	 :pre-build (("make" "elpa"))
	 :build (:not elpa--compile-info) ;; Make will take care of this step
	 :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
	 :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :hook
  ((LaTeX-mode . reftex-mode))
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")
				     (output-html "xdg-open"))
	TeX-auto-save t
	TeX-parse-self t
	TeX-auto-regexp-list 'TeX-auto-full-regexp-list
	TeX-auto-parse-length 999999
	TeX-source-correlate-mode t
	TeX-source-correlate-start-server t
	TeX-engine 'luatex
	LaTeX-flymake-chktex-options '("-n1" "-n24" "-n35") ;; Disable warnings 
	)
  (setq-default TeX-master "main") 
  ;; Word count that actually works
  (defun latex-word-count ()
    (interactive)
    (let ((file-name
	   ;; If region selected then count words in that
	   (if (use-region-p)
	       (let ((temp-file (make-temp-file "emacs-temp-"))
		     (selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
		 (with-temp-file temp-file
		   (insert selected-text))
		 temp-file)
	     (if (or (derived-mode-p 'latex-mode)
		     (derived-mode-p 'tex-mode))
		 (buffer-file-name)
	       (progn
		 (message "Not in LaTeX or TeX buffer.")
		 nil)))))
      (if file-name
	  (shell-command (concat "/usr/bin/texcount "
				 "-inc "
				 (shell-quote-argument (expand-file-name file-name))))))))

;;;; citar
;; reference management
(use-package bibtex
  :ensure nil
  :demand t)

(use-package citar
  :after
  bibtex
  :custom
  (org-cite-global-bibliography '("~/Sync/Notes/library.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :hook
  ((org-mode tex) . citar-capf-setup)
  :bind (("C-x M-b" . citar-open)
	 :map org-mode-map
	 ("C-c c" . org-cite-insert)
	 :map TeX-mode-map
	 ("C-c c" . citar-insert-citation)))

;; LaTeX setup is a bit more complex because LaTeX-mode-map is initialised after
;; AucTeX is initialised
(use-package citar
  :ensure nil
  :after
  latex
  :hook
  (LaTeX-mode . citar-capf-setup)
  :bind
  (:map LaTeX-mode-map
	("C-c c" . citar-insert-citation)))

;;;; citar-embark
;; embark actions
(use-package citar-embark
  :after
  (citar embark)
  :diminish citar-embark-mode
  :config
  (citar-embark-mode)
  :no-require)

;;;; flymake-vale
;; Use vale prose linter with Flymake
(use-package flymake-vale
  :ensure
  (:host github :repo "tpeacock19/flymake-vale" :branch "main")
  :hook
  ((LaTeX-mode org-mode markdown-mode) . flymake-vale-load))

;;;; powerthesaurus
;; Powerthesaurus integration
(use-package powerthesaurus
  :bind
  ("C-$" . powerthesaurus-transient))

