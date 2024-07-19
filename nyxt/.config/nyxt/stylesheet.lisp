(in-package #:nyxt-user)

;; Browser Theme
(define-configuration browser
    ((theme
      (make-instance 'theme:theme
		     :dark-p t

		     :font-family "ShureTechMono Nerd Font"
		     :monospace-font-family "ShureTechMono Nerd Font"
		     
		     :action-color- "#81A1C1"
		     :action-color "#5E81AC"
		     :action-color+ "#5E81AC"
		     :on-action-color "#ECEFF4"

		     :background-color- "#3B4252"
                     :background-color "#2E3440"
		     :background-color+ "#2E3440"
		     :on-background-color "#ECEFF4"

		     :codeblock-color- "#3B4252"
		     :codeblock-color "#3B4252"
		     :codeblock-color+ "#3B4252"
		     :on-codeblock-color "#ECEFF4"

		     :text-color- "#D8DEE9"
		     :text-color "#ECEFF4"
		     :text-color+ "#D8DEE9"
		     :contrast-text-color "#4C566A"

		     :highlight-color- "#B48EAD"
		     :highlight-color "#B48EAD"
		     :highlight-color+ "#B48EAD"
		     :on-highlight-color "#5E81AC"

		     :primary-color- "#677691"
		     :primary-color "#677691"
		     :primary-color+ "#677691"
		     :on-primary-color "#E5E9F0"

		     :secondary-color- "#4C566A"
		     :secondary-color "#4C566A"
		     :secondary-color "#4C566A"
		     :on-secondary-color "#E5E9F0"

		     :sucess-color- "#A3BE8C"
		     :sucess-color "#A3BE8C"
		     :sucess-color+ "#A3BE8C"
		     :on-sucess-color "#E5E9F0"

		     :warning-color- "#EBCB8B"
		     :warning-color "#D08770"
		     :warning-color+ "BF616A"
		     :on-warning-color "#E5E9F0"

		     ))))


