;; -*- lexical-binding: t; -*-

;;; eshell-pet.el --- Use emacs-pet to activate and deactivate python venvs in Eshell

;; Author: Struan Robertson contact@struanrobertson.co.uk
;; Version: 1.0
;; Package-Requires: ((emacs "28.0") (pet "3.1.0") (vc-git) (eshell))

;;; Commentary:
;; A simple package providing functions for activating and deactivating Python virtual environments in Eshell.
;; The eshell-pet-mode automatically activates and deactivates virtual environments by checking if a ".venv" directory is present at either PWD or the root directory of the current git project.

;; TODO: Use pet specific variables for system bin name and venv file names

;;; Code:

(require 'vc-git)
(require 'pet)
(require 'eshell)

(defun eshell-pet--remove-from-PATH (dir)
  "Safely remove a directory from the current PATH environment variable."
  (let
      ((current-path (getenv "PATH")))
    (let
	((parts (split-string current-path ":")))
      (let
	  ((dirs-fixed (remove dir parts)))
	(eshell-set-path (mapconcat #'identity dirs-fixed ":"))))))

(defun eshell/activate-venv (&optional venv)
  "Activate a Python virtual environment in Eshell.
   The path to the virtual environment optionally passed or found using pet."
  (interactive)
  (let ((venv (or venv (pet-virtualenv-root))))
    (if venv
	(if (not (bound-and-true-p active-venv))
	    (progn
	      (let
		  ((parts (file-name-split venv)))
		(setq-local active-venv (nth (- (length parts) 2) parts)))
	      (setq-local venv-bin-dir (expand-file-name "bin" venv))
	      (let
		  ((current-path (getenv "PATH")))
		(eshell-set-path (concat venv-bin-dir ":" current-path))
		)
	      (pet-mode t)
	      (message "Activated venv %s" active-venv)))
	    
      (message "No venv found."))))

(defun eshell/deactivate-venv ()
  "Deactivate the currently active Python virtual environment in Eshell."
  (interactive)
  (if (bound-and-true-p active-venv)
	  (progn
	    (eshell-pet--remove-from-PATH venv-bin-dir)
	    (pet-mode -1)
	    (message "Deactivated venv %s" active-venv)
	    (setq-local active-venv nil
			venv-bin-dir nil))
    (message "No currently active venv")))

(defun eshell-pet--auto-activate-venv ()
  "Automatically detect and activate/deactivate Python virtual environments based on Eshell PWD.
   Works by checking PWD or the root directory of the Git repository the PWD is in for a folder named \".venv\"."
  (let
      ;; Expands to just $PWD/.venv if not in a git repo
      ((venv (expand-file-name ".venv" (vc-git-root (eshell/pwd))))) 
    (if (and (file-directory-p venv) (not (bound-and-true-p active-venv)))
	(eshell/activate-venv venv)
      (if (and (not (file-directory-p venv)) (bound-and-true-p active-venv))
	  (eshell/deactivate-venv)))))

(define-minor-mode eshell-pet-mode
  "Minor mode for automatic Python virtual environment detection on Eshell directory change using `eshell-pet--auto-activate-venv'."
  :init-value nil
  :lighter " Eshell-pet"
  (if eshell-pet-mode
      (progn
	(add-hook 'eshell-directory-change-hook 'eshell-pet--auto-activate-venv)
	(eshell-pet--auto-activate-venv))
     (remove-hook 'eshell-directory-change-hook 'eshell-pet--auto-activate-venv)))

(provide 'eshell-pet)

;;; eshell-pet.el ends here
