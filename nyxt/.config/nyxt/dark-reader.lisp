;;; ~/.config/nyxt/dark-reader.lisp

(define-configuration nx-dark-reader:dark-reader-mode
  ((nxdr:selection-color "#5E81AC")
   (nxdr:background-color "#2E3440")
   (nxdr:text-color "#ECEFF4")))

(define-configuration web-buffer
  ((default-modes `(nxdr:dark-reader-mode ,@%slot-value%))))
