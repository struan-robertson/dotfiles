;;; eshell-venv.el --- Use emacs-pet to activate and deactivate python venvs in Eshell -*- lexical-binding: t; -*-

;; Author: Struan Robertson contact@struanrobertson.co.uk
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (vc-git) (eshell))

;;; Commentary:
;; A simple package providing functions for activating and deactivating Python virtual environments in Eshell.
;; The eshell-venv-mode automatically activates and deactivates virtual environments by checking if a ".venv" directory is present at either PWD or the root directory of the current git project.

;;; Code:

(require 'vc-git)
(require 'eshell)

(defvar active-venv)
(defvar venv-bin-dir)

(defun eshell-venv--remove-from-PATH (dir)
  "Safely remove DIR from the current PATH environment variable."
  (let
      ((current-path (getenv "PATH")))
    (let
	((parts (split-string current-path ":")))
      (let
	  ((dirs-fixed (remove dir parts)))
	(eshell-set-path (mapconcat #'identity dirs-fixed ":"))))))

(defun eshell/activate-venv (&optional venv)
  "Activate a Python virtual environment in Eshell.
Optionally pass the venv root (VENV) or find using `eshell-venv--find-venv'
at the `default-directory'."
  (interactive)
  (if-let ((venv (or venv (eshell-venv--find-venv default-directory))))
      (if (not (bound-and-true-p active-venv))
	  (progn
	    (let
		((parts (file-name-split venv)))
	      (setq-local active-venv (nth (- (length parts) 2) parts)))
	    (setq-local venv-bin-dir (expand-file-name "bin" venv))
	    (let
		  ((current-path (getenv "PATH")))
		(eshell-set-path (concat venv-bin-dir ":" current-path)))
	    (message "Activated venv %s." active-venv)))
    (message "No venv found.")))

(defun eshell/deactivate-venv ()
  "Deactivate the currently active Python virtual environment in Eshell."
  (interactive)
  (if (bound-and-true-p active-venv)
	  (progn
	    (eshell-venv--remove-from-PATH venv-bin-dir)
	    (message "Deactivated venv %s" active-venv)
	    (setq-local active-venv nil
			venv-bin-dir nil))
    (message "No currently active venv")))

(defun eshell-venv--find-venv (dir)
  "Check if a .venv directory exists either at project root of DIR or at DIR.
If so, return path to .venv."
  (let
      ;; Expands to just $PWD/.venv if not in a git repo
      ((venv (expand-file-name ".venv" (vc-git-root dir))))
    (if (file-directory-p venv)
	venv
      nil)))

(defun eshell-venv--auto-activate-venv ()
  "Automatically detect and activate/deactivate Python venv based on Eshell PWD.
Uses `eshell-venv--find-venv' to determine if venv exits."
  (let ((venv (eshell-venv--find-venv (eshell/pwd))))
    (if (and venv (not (bound-and-true-p active-venv)))
	(eshell/activate-venv venv)
      (if (and (not venv) (bound-and-true-p active-venv))
	  (eshell/deactivate-venv)))))

(define-minor-mode eshell-venv-mode
  "Minor mode for automatic Python venv detection on Eshell directory change.
Uses the `eshell-venv--auto-activate-venv' function."
  :init-value nil
  :lighter " eshell-venv"
  (if eshell-venv-mode
      (progn
	(add-hook 'eshell-directory-change-hook 'eshell-venv--auto-activate-venv)
	(eshell-venv--auto-activate-venv))
     (remove-hook 'eshell-directory-change-hook 'eshell-venv--auto-activate-venv)))

(provide 'eshell-venv)

;;; eshell-venv.el ends here
