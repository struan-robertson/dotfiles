(in-package #:nyxt-user)

(define-command-global eval-expression ()
  "Prompt for the expression and evaluate it, echoing result to the `message-area'.
   Similar to what can be done with M-x but allows for debugging"
  (let ((expression-string
          ;; Read an arbitrary expression. No error checking, though.
          (first (prompt :prompt "Expression to evaluate"
                         :sources (list (make-instance 'prompter:raw-source))))))
    ;; Message the evaluation result to the message-area down below.
    (echo "~S" (eval (read-from-string expression-string)))))

