(in-package #:nyxt-user)

;; Commands
(nyxt::load-lisp "~/.config/nyxt/commands.lisp")

;; KeepassXC
(nyxt::load-lisp "~/.config/nyxt/password.lisp")

;; Emacs config
(define-configuration buffer
  ((default-modes
       (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))
   (override-map (let ((map (make-keymap "override-map")))
		   (define-key map "M-i" 'nyxt/mode/hint:follow-hint)
		   (define-key map "C-h m" 'describe-mode)
		   (define-key map "C-c p" 'copy-password)
		   (define-key map "C-c u" 'copy-username)
		   (define-key map "C-c y" 'autofill)
		   (define-key map "C-i" :input-edit-mode)
		   (define-key map "M-:" 'eval-expression)
		   (define-key map "C-s" :search-buffer)
		   (define-key map "C-M-s" :remove-search-marks)))))

;; Privacy config
(define-configuration web-buffer
    ((default-modes
	 (append (list 'nyxt/mode/no-webgl:no-webgl-mode
		       'nyxt/mode/no-script:no-script-mode
		       'nyxt/mode/reduce-tracking:reduce-tracking-mode
		       'nyxt/mode/force-https:force-https-mode
		       'nyxt/mode/blocker:blocker-mode)
		 %slot-value%))))

;; Use en_GB by default
(defclass SPELL_CHECK_MODE ()
  ((spell-check-language :initform "en_GB")))

;; allow remote control over nyxt
(define-configuration browser
(
;; Whether code sent to the socket gets executed. You must understand the
;; risks before enabling this: a privileged user with access to your system
;; can then take control of the browser and execute arbitrary code under your
;; user profile.
(remote-execution-p t)))

;; Search
(nyxt::load-lisp "~/.config/nyxt/search.lisp")

;; Theme
(nyxt::load-lisp "~/.config/nyxt/stylesheet.lisp")

;; Extensions
(nyxt::load-lisp "~/.config/nyxt/extensions.lisp")

;; Status line
(nyxt::load-lisp "~/.config/nyxt/status.lisp")
