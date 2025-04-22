;; init.el  -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.config/emacs/lisp")

;;; Packages

;;;; elpaca

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;; packages.el

(load "packages")

;;; Keybindings
(load "keybindings")

;;; Faces
(set-face-attribute 'variable-pitch nil :family "Roboto" :height 140 :weight 'light :foreground "#bbc6d9")
(set-face-attribute 'default nil :font "Roboto Mono" :height 130 :foreground "#bbc6d9")

;;; Custom Variables 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(pdf-annot-default-annotation-properties
   '((t (label . "Struan Robertson"))
     (text (color . "#bf616A") (icon . "Note"))
     (highlight (color . "#81a1c1")) (underline (color . "#b48aed"))
     (squiggly (color . "#d08770")) (strike-out (color . "#bf616A"))))
 '(safe-local-variable-values
   '((python-shell-virtualenv-root
      . "/home/struan/Development/Doctorate/santa-gui/.venv/")
     (python-shell-virtualenv-root
      . "/home/struan/Development/Doctorate/santa/.venv/")
     (python-shell-virtualenv-root . "/home/srobertson/santa/.venv/")
     (python-shell-virtualenv-root
      . "/home/srobertson/StyleGAN/.venv/")
     (python-shell-virtualenv-root
      . "/home/struan/Development/Doctorate/sefa/.venv/")
     (citar-bibliography . "biblio.bib")
     (python-shell-virtualenv-root
      . "/home/struan/Development/Doctorate/dataset-similarities/.venv/")
     (python-shell-process-environment
      "HSA_OVERRIDE_GFX_VERSION=10.3.0")
     (python-shell-virtualenv-root
      . "/home/struan/Development/Doctorate/shoeprint-image-retrieval/.venv/")
     (python-shell-interpreter
      . "/home/srobertson/Template Matching/.venv/bin/python")
     (python-shell-virtualenv-root
      . "/home/srobertson/Template Matching/.venv/")
     (python-shell-process-environment quote
				       ("HSA_OVERRIDE_GFX_VERSION=10.3.0"))
     (python-shell-virtualenv-root
      . "~/Development/Doctorate/shoeprint-image-retrieval/.venv/")
     (python-shell-interpreter
      . "~/Development/Doctorate/shoeprint-image-retrieval/.venv/bin/python")
     (python-shell-virtualenv-root . ".venv/")
     (python-shell-interpreter . ".venv/bin/python")
     (eshell-toggle-use-git-root . t) (eval outline-hide-sublevels 2))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2E3440" :foreground "#bbc6d9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 128 :width normal :foundry "GOOG" :family "Roboto Mono"))))
 '(avy-lead-face ((t (:background "#bf616a" :foreground "white"))))
 '(avy-lead-face-0 ((t (:background "#5e81ac" :foreground "white"))))
 '(avy-lead-face-1 ((t (:background "#bbc6d9" :foreground "white"))))
 '(avy-lead-face-2 ((t (:background "#b48ead" :foreground "white"))))
 '(bold ((t (:inherit nano-strong :weight medium))))
 '(circe-highlight-nick-face ((t (:foreground "#8FBCBB" :weight bold))))
 '(circe-prompt-face ((t (:background "#5E81AC" :foreground "#2E3440" :weight bold))))
 '(circe-server-face ((t (:foreground "#81A1C1"))))
 '(dired-broken-symlink ((t (:background "#bf616a" :weight bold))))
 '(eat-term-color-10 ((t (:inherit ansi-color-bright-black :foreground "#a3be8c"))))
 '(eat-term-color-11 ((t (:inherit ansi-color-bright-black :foreground "#ebcb8b"))))
 '(eat-term-color-12 ((t (:inherit ansi-color-bright-black :foreground "#5e81ac"))))
 '(eat-term-color-2 ((t (:inherit ansi-color-green :foreground "#a3be8c"))))
 '(eat-term-color-3 ((t (:inherit ansi-color-yellow :foreground "#ebcb8b"))))
 '(eat-term-color-4 ((t (:inherit ansi-color-blue :foreground "#5e81ac"))))
 '(eat-term-color-5 ((t (:inherit ansi-color-magenta :foreground "#b48ead"))))
 '(eat-term-color-6 ((t (:inherit ansi-color-cyan :foreground "#88c0d0"))))
 '(eat-term-color-8 ((t (:inherit nano-faded))))
 '(eglot-inlay-hint-face ((t (:inherit shadow))))
 '(eros-result-overlay-face ((t (:background "#434c5e" :box (:line-width (1 . -1) :color "#434c5e")))))
 '(escape-glyph ((t (:foreground "#5e81ac"))))
 '(flymake-error ((t (:underline (:color "#bf616a" :style wave :position nil)))))
 '(flymake-note ((t (:underline (:color "#a3be8c" :style wave :position nil)))))
 '(flymake-warning ((t (:underline (:color "#d08770" :style wave :position nil)))))
 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-italic-face ((t (:inherit italic :foreground "#a3be8c"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#ECEFF4" :weight normal))))
 '(font-latex-sedate-face ((t (:foreground "#81a1c1"))))
 '(font-latex-string-face ((t (:foreground "#bbc6d9"))))
 '(font-latex-warning-face ((t (:inherit bold :foreground "#bf616a"))))
 '(gptel-context-highlight-face ((t (:extend t :background "#3b4252"))))
 '(gptel-rewrite-highlight-face ((t (:extend t :background "#3b4252"))))
 '(hi-aquamarine ((t (:background "#8fbcbb" :foreground "#2e3440"))))
 '(hi-blue ((t (:background "#88c0d0" :foreground "#2e3440"))))
 '(hi-green ((t (:background "#a3be8c" :foreground "#2e3440"))))
 '(hi-green-b ((t (:foreground "#a3be8c" :weight bold))))
 '(hi-pink ((t (:background "#b48ead" :foreground "#2e3440"))))
 '(hi-red-b ((t (:foreground "#bf616a" :weight bold))))
 '(hi-salmon ((t (:background "#D08770" :foreground "#2e3440"))))
 '(hi-yellow ((t (:background "#ebcb8b" :foreground "#2e3440"))))
 '(jupyter-eval-overlay ((t (:foreground "#5e81ac" :weight bold))))
 '(jupyter-repl-input-prompt ((t (:foreground "#a3be8c"))))
 '(jupyter-repl-output-prompt ((t (:foreground "#bf616a"))))
 '(jupyter-repl-traceback ((t nil)))
 '(ledger-font-payee-uncleared-face ((t (:inherit bold))))
 '(lui-button-face ((t (:foreground "#88c0d0" :underline t))))
 '(lui-time-stamp-face ((t (:foreground "#5E81AC" :weight bold))))
 '(magit-diff-added-highlight ((t (:inherit (highlight nano-salient nano-strong) :extend t :background "#3B4252" :foreground "#81A1C1"))))
 '(magit-diff-context-highlight ((t (:inherit (highlight nano-faded) :extend t :background "#2e3440" :foreground "#FFFFFF"))))
 '(nano-critical ((t (:foreground "#bf616a" :weight normal))) t)
 '(nano-critical-i ((t (:background "#bf616a" :foreground "#2E3440" :weight normal))) t)
 '(nano-default ((t (:foreground "#bbc6d9"))) t)
 '(nano-default-i ((t (:background "#bbc6d9" :foreground "#2E3440"))) t)
 '(nano-popout ((t (:foreground "#ebcb8b"))) t)
 '(nano-popout-i ((t (:background "#ebcb8b" :foreground "#2E3440"))) t)
 '(nano-strong ((t (:foreground "#bbc6d9" :weight normal))) t)
 '(nano-strong-i ((t (:background "#bbc6d9" :foreground "#2E3440" :weight normal))) t)
 '(org-habit-alert-face ((t (:background "#ebcb8b" :foreground "#2e3440"))))
 '(org-habit-alert-future-face ((t (:background "#d08770" :foreground "#2e3440"))))
 '(org-habit-clear-future-face ((t (:background "#5e81ac"))))
 '(org-habit-overdue-face ((t (:background "#bf616a" :foreground "#2e3440"))))
 '(org-habit-overdue-future-face ((t (:background "#bf616a" :foreground "#4c566a"))))
 '(org-habit-ready-face ((t (:background "#a3be8c" :foreground "#2e3440"))))
 '(org-level-1 ((t (:inherit nano-strong :extend nil :weight bold))))
 '(org-pomodoro-mode-line ((t nil)))
 '(org-pomodoro-mode-line-break ((t (:foreground "#a3be8c"))))
 '(org-pomodoro-mode-line-overtime ((t (:foreground "#bf616a" :weight bold))))
 '(org-scheduled ((t (:inherit nano-default))))
 '(org-scheduled-today ((t (:inherit nano-salient))))
 '(outline-1 ((t (:inherit nano-strong :weight bold))))
 '(outline-2 ((t (:inherit outline-1))))
 '(outline-3 ((t (:inherit outline-1))))
 '(outline-4 ((t (:inherit outline-1))))
 '(outline-5 ((t (:inherit outline-1))))
 '(outline-6 ((t (:inherit outline-1))))
 '(outline-7 ((t (:inherit outline-1))))
 '(outline-8 ((t (:inherit outline-1))))
 '(pulse-highlight-face ((t (:foreground "#5e81ac"))))
 '(pulse-highlight-start-face ((t (:foreground "#5e81ac"))))
 '(show-paren-match ((t (:inherit nano-strong :foreground "#b48ead"))))
 '(shr-text ((t (:inherit nano-default))))
 '(transient-argument ((t (:inherit bold :weight bold))))
 '(vundo-highlight ((t (:inherit vundo-node :foreground "#5e81ac" :weight bold))))
 '(vundo-saved ((t (:inherit vundo-node :foreground "#a3be8c"))))
 '(wgrep-delete-face ((t (:foreground "#bf616a"))))
 '(wgrep-done-face ((t (:foreground "#88c0d0"))))
 '(wgrep-face ((t (:foreground "#a3be8c"))))
 '(wgrep-file-face ((t (:foreground "#a3be8c"))))
 '(wgrep-reject-face ((t (:foreground "#d08770" :weight bold))))
 '(writegood-duplicates-face ((t (:underline (:color "#b48ead" :style wave :position nil)))))
 '(writegood-passive-voice-face ((t (:underline (:color "#88c0d0" :style wave :position nil)))))
 '(writegood-weasels-face ((t (:underline (:color "#d08770" :style wave :position nil))))))

;; Allow for using ROCM with my GPU
(setenv "HSA_OVERRIDE_GFX_VERSION" "10.3.0")

(message "init.el loaded successfully")
