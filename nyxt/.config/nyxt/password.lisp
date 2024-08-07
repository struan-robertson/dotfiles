(in-package #:nyxt-user)

(defmethod initialize-instance :after
           ((interface password:keepassxc-interface)
            &key &allow-other-keys)
  "It's obviously not recommended to set master password here,
as your config is likely unencrypted and can reveal your password to someone
peeking at the screen."
  (setf (password:password-file interface)
          "/home/struan/Documents/Keys/keepass.kdbx"
        (password:key-file interface) ""
        (password:yubikey-slot interface) ""))

(define-configuration nyxt/mode/password:password-mode
  ((nyxt/mode/password:password-interface
    (make-instance 'password:keepassxc-interface))))

(define-configuration buffer
  ((default-modes
    (append (list 'nyxt/mode/password:password-mode) %slot-value%))))
