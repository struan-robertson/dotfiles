;;; eshell-venv.el --- Use emacs-pet to activate and deactivate python venvs in Eshell -*- lexical-binding: t; -*-

;; Author: Struan Robertson contact@struan.tech
;; Version: 1.0
;; Package-Requires: ((emacs "28.0"))

;;; Commentary:
;; A simple package providing functions for activating and deactivating Python virtual environments in Eshell.
;; The eshell-venv-mode automatically activates and deactivates virtual environments by checking if a ".venv" directory is present at either PWD or the root directory of the current git project.

;;; Code:

(require 'vc-git)
(require 'eshell)
(require 'cl-lib)

(defvar python-shell-virtualenv-root nil
  "Variable to store the path to a Python virtual environment.")

(defvar python-shell-virtualenv-bin nil
  "Variable to store the path to the bin directory of a Python virtual
environment.")

(defun eshell-venv--remove-from-PATH (dir)
  "Safely remove `dir' from the current Eshell PATH."
  (let ((current-path (eshell-get-path t))) ;; TODO should this be t or nil?
    (when (member dir current-path)
      (eshell-set-path (cl-remove dir current-path)))))

(defun eshell-venv--activate-venv (venv)
  "Setup the variables required for using virtual environment `venv' in Eshell."
  (setq-local python-shell-virtualenv-root venv
	      python-shell-virtualenv-bin (expand-file-name "bin" venv))
  (eshell-set-path (cons (tramp-file-local-name python-shell-virtualenv-bin) (eshell-get-path t)))
  (eshell-reset))

(defun eshell/activate-venv (&optional venv)
  "Activate a Python virtual environment in Eshell.
Optionally pass the venv root `venv', otherwise find using `eshell-venv--find-venv'."
  (interactive)
  (if-let ((venv (or venv (eshell-venv--find-venv default-directory))))
      (if python-shell-virtualenv-root
	  (if (string= python-shell-virtualenv-root venv)
	      (message "venv already activated")
	    (progn
	      (eshell/deactivate-venv)
	      (eshell-venv--activate-venv venv)
	      (message "venv activated")))
	(progn
	  (eshell-venv--activate-venv venv)
	  (message "venv activated")))
    (message "venv not found")))

(defun eshell/deactivate-venv ()
  "Deactivate the currently active Python virtual environment in Eshell."
  (interactive)
  (if python-shell-virtualenv-root
      (progn
	(eshell-venv--remove-from-PATH (tramp-file-local-name python-shell-virtualenv-bin))
	(message "Deactivated venv %s" python-shell-virtualenv-root)
	(setq-local python-shell-virtualenv-root nil
		    python-shell-virtualenv-bin nil))
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
    (cond
     ;; venv found and python-shell-virtualenv-root nil -> activate venv
     ((and venv (not python-shell-virtualenv-root)) 
      (eshell-venv--activate-venv venv))
     ;; venv not found and python-shell-virtualenv-root bound -> deactivate venv
     ((and (not venv) python-shell-virtualenv-root) 
      (eshell/deactivate-venv))
     ;; venv found and python-shell-virtualenv-root not equal to venv -> deactivate venv and then activate venv
     ((and venv (not (string= venv python-shell-virtualenv-root))) 
      (eshell/deactivate-venv)
      (eshell-venv--activate-venv venv))
     ;; venv found and python-shell-virtualenv-root equal to venv -> do nothing
     (t t))))

(define-minor-mode eshell-venv-mode
  "Minor mode for automatic Python venv detection on Eshell directory change.
Uses the `eshell-venv--auto-activate-venv' function."
  :init-value nil
  :lighter " Eshell-venv"
  (if eshell-venv-mode
      (progn
	(add-hook 'eshell-directory-change-hook 'eshell-venv--auto-activate-venv)
	(eshell-venv--auto-activate-venv))
    (remove-hook 'eshell-directory-change-hook 'eshell-venv--auto-activate-venv)))

(provide 'eshell-venv)

;;; eshell-venv.el ends here
