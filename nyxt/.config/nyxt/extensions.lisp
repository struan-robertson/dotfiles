(in-package #:nyxt-user)

;; Dark reader extension
(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/user-script:user-script-mode %slot-value%)))) 
(define-nyxt-user-system-and-load "nyxt-user/dark-reader"
  ;; Remove this line if you don't need the file.
  :components ("dark-reader.lisp")
  :depends-on (:nx-dark-reader))

;; Zotero extension
(nyxt:define-nyxt-user-system-and-load "nyxt-user/nx-zotero-proxy"
  :description "This proxy system saves us if nx-zotero fails to load.
Otherwise it will break all the config loading."
  :depends-on ("nx-zotero"))
(define-configuration web-buffer
   ((default-modes
     (pushnew 'zotero-mode %slot-value%))))

