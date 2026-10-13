;;; ace-window-sway.el --- Sway-aware ace-window candidates -*- lexical-binding: t; -*-

;;; Commentary:

;; Wayland cannot activate a window the compositor is not showing, so
;; candidates are limited to frames on the current workspace and the
;; target frame's sway container is focused before Emacs switches to it.
;;
;; Frames are matched to containers by title because pgtk leaves the
;; `title' frame parameter nil, while the frame `name' tracks
;; `frame-title-format' -- the string sway reports as the container name.
;;
;; swaymsg runs on the host through flatpak-spawn when Emacs is inside a
;; toolbox container, where sway is not installed.

;;; Code:

(require 'ace-window)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup ace-window-sway nil
  "Sway integration for `ace-window'."
  :group 'ace-window
  :prefix "aw-sway-")

(defcustom aw-sway-swaymsg-program "swaymsg"
  "Program used to talk to the sway IPC socket."
  :type 'string)

(defcustom aw-sway-host-program "flatpak-spawn"
  "Program used to run `aw-sway-swaymsg-program' on the host.
Used when swaymsg is not available locally, e.g. inside a toolbox."
  :type 'string)

(defcustom aw-sway-app-ids '("emacs")
  "Wayland app IDs that identify Emacs frames."
  :type '(repeat string))

(defcustom aw-sway-window-classes '("Emacs")
  "X11 window classes that identify Emacs frames."
  :type '(repeat string))

(defun aw-sway--swaymsg (&rest args)
  (let* ((local (executable-find aw-sway-swaymsg-program))
         (host (executable-find aw-sway-host-program))
         (program (or local host))
         (arguments (if local
                        args
                      (append (list "--host" aw-sway-swaymsg-program) args))))
    (unless program
      (error "Neither %s nor %s found"
             aw-sway-swaymsg-program aw-sway-host-program))
    (with-temp-buffer
      (let ((status (apply #'call-process program nil t nil arguments)))
        (unless (and (integerp status) (zerop status))
          (error "swaymsg failed (%s): %s"
                 status (string-trim (buffer-string))))
        (buffer-string)))))

(defun aw-sway--json (string)
  (json-parse-string string :object-type 'alist))

(defun aw-sway--tree ()
  (aw-sway--json (aw-sway--swaymsg "-t" "get_tree")))

(defun aw-sway--current-workspace ()
  (let ((workspaces (aw-sway--json (aw-sway--swaymsg "-t" "get_workspaces"))))
    (alist-get 'name
               (seq-find (lambda (workspace)
                           (eq t (alist-get 'focused workspace)))
                         workspaces))))

(defun aw-sway--run-command (command)
  (let ((results (aw-sway--json (aw-sway--swaymsg command))))
    (and (not (seq-empty-p results))
         (seq-every-p (lambda (result) (eq t (alist-get 'success result)))
                      results))))

(defun aw-sway--window-p (node)
  (or (alist-get 'app_id node)
      (alist-get 'window_properties node)))

(defun aw-sway--emacs-window-p (node)
  (or (member (alist-get 'app_id node) aw-sway-app-ids)
      (member (alist-get 'class (alist-get 'window_properties node))
              aw-sway-window-classes)))

(defun aw-sway--leaves (node &optional workspace)
  (let ((workspace (if (equal (alist-get 'type node) "workspace")
                       (alist-get 'name node)
                     workspace)))
    (nconc (when (aw-sway--window-p node)
             (list (cons workspace node)))
           (cl-mapcan (lambda (child) (aw-sway--leaves child workspace))
                      (append (alist-get 'nodes node) nil)))))

(defun aw-sway--emacs-cons-on-current-workspace ()
  (let ((workspace (aw-sway--current-workspace))
        (leaves (aw-sway--leaves (aw-sway--tree))))
    (cl-loop for (ws . node) in leaves
             when (and (equal ws workspace)
                       (aw-sway--emacs-window-p node))
             collect node)))

(defun aw-sway--frame-title (frame)
  ;; pgtk leaves `title' nil; `name' tracks `frame-title-format'.
  (or (frame-parameter frame 'title)
      (frame-parameter frame 'name)))

(defun aw-sway--toplevel-frame (frame)
  (while (and (frame-live-p frame) (frame-parent frame))
    (setq frame (frame-parent frame)))
  frame)

(defun aw-sway--frame-pairs ()
  ;; Titles are not unique, so pair the focused frame with the focused
  ;; container first, then give unfocused frames the remaining
  ;; containers: two frames showing the same buffer focus each other.
  (let ((nodes (aw-sway--emacs-cons-on-current-workspace))
        (selected (aw-sway--toplevel-frame (selected-frame)))
        (frames (cl-remove-if-not
                 (lambda (frame)
                   (and (window-system frame) (not (frame-parent frame))))
                 (frame-list)))
        used pairs)
    (dolist (frame frames)
      (let* ((title (aw-sway--frame-title frame))
             (candidates
              (when (and (stringp title) (not (string-empty-p title)))
                (cl-remove-if (lambda (node)
                                (or (memq node used)
                                    (not (equal (alist-get 'name node) title))))
                              nodes))))
        (when candidates
          (let ((node (if (eq frame selected)
                          (or (seq-find (lambda (n) (eq t (alist-get 'focused n)))
                                        candidates)
                              (car candidates))
                        (or (seq-find (lambda (n) (not (eq t (alist-get 'focused n))))
                                      candidates)
                            (car candidates)))))
            (push node used)
            (push (cons frame node) pairs)))))
    pairs))

(defun aw-sway--frames-on-current-workspace ()
  (mapcar #'car (aw-sway--frame-pairs)))

(defun aw-sway--con-for-frame (frame)
  (cdr (assq frame (aw-sway--frame-pairs))))

(defun aw-sway--focus-frame (frame)
  (when-let* ((node (aw-sway--con-for-frame frame))
              (id (alist-get 'id node)))
    ;; Focusing a container on another workspace switches to it.
    (aw-sway--run-command (format "[con_id=%d] focus" id))))

(defun aw-sway-filter-window-list (windows)
  (condition-case nil
      (let ((allowed (aw-sway--frames-on-current-workspace)))
        (if (memq (aw-sway--toplevel-frame (selected-frame)) allowed)
            (cl-remove-if-not
             (lambda (window)
               (memq (aw-sway--toplevel-frame (window-frame window)) allowed))
             windows)
          ;; The selected frame is always on the current workspace, so
          ;; failure to match it means the titles are unreliable: keep
          ;; every candidate rather than hide other frames.
          windows))
    (error windows)))

(defun aw-sway-switch-to-window (orig window)
  (let ((frame (window-frame window)))
    (if (and (window-system frame)
             (not (eq frame (selected-frame)))
             (ignore-errors (aw-sway--focus-frame frame)))
        ;; Sway has focused the container; asking Wayland to activate the
        ;; window again only produces the urgent outline.
        (cl-letf (((symbol-function 'select-frame-set-input-focus)
                   (lambda (frame &optional no-enter)
                     (select-frame frame no-enter))))
          (funcall orig window))
      (funcall orig window))))

(unless (advice-member-p #'aw-sway-filter-window-list 'aw-window-list)
  (advice-add 'aw-window-list :filter-return #'aw-sway-filter-window-list))
(unless (advice-member-p #'aw-sway-switch-to-window 'aw-switch-to-window)
  (advice-add 'aw-switch-to-window :around #'aw-sway-switch-to-window))

(provide 'ace-window-sway)
