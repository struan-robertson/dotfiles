;; early-init.el -*- lexical-binding: t; -*-

;; Disable the toolbar
(tool-bar-mode -1)
;; Disable the scrollbar
(scroll-bar-mode -1)
;; Disable the tooltips
(tooltip-mode -1)
;; Disable the menu bar
(menu-bar-mode -1)

;; Pixel scrolling
(pixel-scroll-precision-mode 1)

(column-number-mode)

(setq package-enable-at-startup nil)

(message "early-init.el loaded successfully")
