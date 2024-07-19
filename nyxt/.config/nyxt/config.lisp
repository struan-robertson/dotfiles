(define-configuration buffer
  ((default-modes
       (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))
   (override-map (let ((map (make-keymap "override-map")))
		   (define-key map "M-i" 'nyxt/mode/hint:follow-hint)))))

;; Use en_GB by default
(defclass SPELL_CHECK_MODE ()
  ((spell-check-language :initform "en_GB")))

(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(defun make-kagi-completion (&key request-args)
  "Helper that generates Kagi search completion functions."
  (make-search-completion-function
   :base-url "https://kagi.com/api/autosuggest?q=~a"
   :processing-function
   #'(lambda (results)
       (when results
	 (second (json:decode-json-from-string results))))
   :request-args request-args))

(define-configuration context-buffer
  "Add Kagi search engine."
  ((search-engines
    (append
     %slot-value%
     (list
      (make-instance 'search-engine :name "Kagi" :shortcut "k"
                     :search-url "https://kagi.com/search?q=~a"
                     :fallback-url "https://kagi.com"
                     :completion-function (make-kagi-completion)
                     ))))))
