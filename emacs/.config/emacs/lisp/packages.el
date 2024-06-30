;; === Elpaca setup  === -*- lexical-binding: t; -*-
;; Maybe consider getting rid of this at Emacs 30
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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
  :ensure
  (:host github
	 :repo "rougier/nano-theme"
	 :branch "master")
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

(use-package magit
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Is it worth switching to zsh for eat integration? Probably not but idk
(use-package eat
  :ensure
  (:host codeberg
	 :repo "akib/emacs-eat"
	 :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el")))
  :hook
  (eshell-load . eat-eshell-mode)
  :config
  (setq eshell-visual-commands nil))

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-selection '((output-pdf "Sioyek")
				     (output-html "xdg-open")))
  :hook
  (LaTeX-mode . reftex-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))
