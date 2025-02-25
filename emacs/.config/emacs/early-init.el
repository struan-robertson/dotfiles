;; early-init.el -*- lexical-binding: t; -*-

;; Disable package.el
(setq package-enable-startup nil)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable the scrollbar
(scroll-bar-mode -1)
;; Disable the tooltips
(tooltip-mode -1)
;; Disable the menu bar
(menu-bar-mode -1)

(column-number-mode)

(message "early-init.el loaded successfully")
