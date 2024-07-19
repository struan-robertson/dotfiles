(in-package #:nyxt-user)

(define-configuration :status-buffer
  "Display modes as short glyphs."
  ((glyph-mode-presentation-p t)))

(define-configuration :force-https-mode ((glyph "ϕ")))
(define-configuration :user-script-mode ((glyph "u")))
(define-configuration :blocker-mode ((glyph "β")))
(define-configuration :no-webgl-mode ((glyph "π")))
(define-configuration :no-script-mode ((glyph "s")))
(define-configuration :reduce-tracking-mode ((glyph "τ")))
(define-configuration :certificate-exception-mode ((glyph "χ")))
(define-configuration :zotero-mode ((glyph "ᵶ")))

(define-configuration status-buffer
  "Hide most of the status elements but URL and modes."
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
	     `("#controls,#tabs"
	       :display none !important))))))


