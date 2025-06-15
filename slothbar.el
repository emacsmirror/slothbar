;;; slothbar.el --- Emacs X window manager status bar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;;         Agnes Li <agnes.li@mailfence.com>
;; Version: 0.28.2
;; Homepage: https://codeberg.org/agnes-li/slothbar
;; Package-Requires: ((backlight "1.4") (compat "29.1") (dash "2.1.0") (f "0.20.0") (fontsloth "0.19.1") (log4e "0.3.3") (s "1.12.0") (volume "1.0") (xelb "0.18") (emacs "28.0"))
;; Keywords: window-manager, status-bar, exwm

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package uses xelb to build a dock bar for displaying status
;; information.

;; An example configuration with use-package:

;; (use-package slothbar
;;   :config
;;   (require 'slothbar-module-requires)
;;   (setq slothbar-modules '(:left
;;                            slothbar-tray-create slothbar-date-create
;;                            slothbar-workspaces-create
;;                            :right
;;                            slothbar-wifi-create slothbar-volume-create
;;                            slothbar-backlight-create
;;                            slothbar-battery-create))
;;   ;; to enable multi-screen support
;;   (slothbar-randr-mode))

;; then M-x: slothbar
;; To exit:
;; M-x: slothbar-exit

;;; Code:

(require 'cl-lib)
(require 'fontsloth)
(require 'pcase)
(require 'seq)
(require 'xcb)
(require 'xcb-icccm)
(require 'xcb-ewmh)

(require 'slothbar-module)
(require 'slothbar-util)
(require 'slothbar-log)

(defgroup slothbar nil
  "Slothbar is a status bar that displays as a dock window in X."
  :group 'display)

(defvar slothbar--connection nil "The X connection.")
(defvar slothbar--window nil "The parent window.")
(defvar slothbar--gc nil "The graphics context.")

(defvar slothbar--enabled nil "Non-nil if slothbar is enabled.")

(defcustom slothbar-width (display-pixel-width)
  "Slothbar width.

Defaults to the width obtained from `display-pixel-width'"
  :type 'integer
  :group 'slothbar)

(require 'slothbar-layout)

(defcustom slothbar-height 20
  "Slothbar height."
  :type 'integer
  :group 'slothbar)

(require 'slothbar-font)

(add-hook 'slothbar-before-init-hook
          (lambda ()
            (add-variable-watcher 'slothbar-height
                                  #'slothbar-font--watch-px-size)))

(add-hook 'slothbar-after-exit-hook
          (lambda ()
            (remove-variable-watcher 'slothbar-height
                                     #'slothbar-font--watch-px-size)))

(defcustom slothbar-offset-x 0
  "Bar display x offset in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-offset-y 0
  "Bar display y offset in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-margin-y 2
  "Bar vertical margin in pixels."
  :type 'integer
  :group 'slothbar)

(defcustom slothbar-preferred-display "eDP-1"
  "If multiple displays are connected:

- nil indicates to automatically choose one.  If the slothbar-randr
  extension is enabled, this will be the primary display
- string should be the display name as reported by
  `display-monitor-attributes-list`"
  :type '(choice (const :tag "auto" nil)
		 string)
  :group 'slothbar)

(defcustom slothbar-is-bottom nil
  "Non-nil to position slothbar at the bottom of the display and nil for top."
  :type 'boolean
  :group 'slothbar)

(defcustom slothbar-modules nil
  "List of slothbar module constructor names with optional layout instructions.

E.g.: (:left
       `slothbar-tray-create' `slothbar-date-create'
       :right
       `slothbar-wifi-create' `slothbar-volume-create'
       `slothbar-backlight-create' `slothbar-battery-create')"
  :type '(repeat (choice (radio :tag "Layout instruction keyword" :value :left
                                (const :left) (const :right) (const :center))
                         (function :tag "Module constructor or lambda"
                                   :value slothbar-date-create)))
  :group 'slothbar
  :require 'slothbar-module-requires)

(defcustom slothbar-before-init-hook nil
  "Functions to run when before slothbar is initialized."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-after-init-hook nil
  "Functions to run when after slothbar is initialized."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-before-exit-hook nil
  "Functions to run when before slothbar exits."
  :type 'hook
  :group 'slothbar)

(defcustom slothbar-after-exit-hook nil
  "Functions to run when after slothbar exits."
  :type 'hook
  :group 'slothbar)

(defvar slothbar--connection)

(defvar slothbar--modules nil
  "List of slothbar modules with optional layout instructions.")

(defmacro slothbar--global-minor-mode-body (name &optional init exit)
  "Global minor mode body for mode with NAME.

The INIT and EXIT functions are added to `slothbar-after-init-hook' and
`slothbar-before-exit-hook' respectively.  If an X connection exists,
the mode is immediately enabled or disabled."
  (declare (indent 1) (debug t))
  (let* ((mode (intern (format "slothbar-%s-mode" name)))
         (init (or init (intern (format "slothbar-%s--init" name))))
         (exit (or exit (intern (format "slothbar-%s--exit" name)))))
    `(progn
       (cond
        (,mode
         (add-hook 'slothbar-after-init-hook #',init)
         (add-hook 'slothbar-before-exit-hook #',exit)
         (when slothbar--connection (,init)))
        (t
         (remove-hook 'slothbar-after-init-hook #',init)
         (remove-hook 'slothbar-before-exit-hook #',exit)
         (when slothbar--connection (,exit)))))))

(defsubst slothbar-enabled-p ()
  "Return t if slothbar is enabled."
  slothbar--enabled)

(defun slothbar--find-display-geometry (&optional display)
  "Find DISPLAY geometry as an alist of x-offset, y-offset, width, and height.

If DISPLAY is not found, the value chosen is the first found in
`display-monitor-attributes-list'."
  (let* ((all-attrs (display-monitor-attributes-list))
	 (display-attrs
	  (or (seq-some
	       (lambda (l) (when (equal (alist-get 'name l) display) l)) all-attrs)
	      (car all-attrs)))
	 (geom (alist-get 'geometry display-attrs)))
    `((x-offset . ,(car geom))
      (y-offset . ,(cadr geom))
      (width . ,(caddr geom))
      (height . ,(cadddr geom)))))

(defun slothbar--refresh ()
  "Refresh the bar."
  (xcb:+request slothbar--connection
      (make-instance 'xcb:UnmapWindow
                     :window slothbar--window))
  (let ((ecw (xcb:+request-checked+request-check slothbar--connection
                 (make-instance 'xcb:ConfigureWindow
                                :window slothbar--window
                                :value-mask (logior xcb:ConfigWindow:X
						    xcb:ConfigWindow:Y
                                                    xcb:ConfigWindow:Width
                                                    xcb:ConfigWindow:Height)
                                :x slothbar-offset-x
                                :y slothbar-offset-y
                                :width slothbar-width
                                :height (+ slothbar-height)))))
    (slothbar--log-debug* "slothbar-refresh: configure window errors: %s" ecw))
  (xcb:+request slothbar--connection
      (make-instance 'xcb:MapWindow
                     :window slothbar--window))
  (xcb:flush slothbar--connection)
  ;; configure struts
  (pcase-let* (((eieio (height root-window-height))
                (xcb:+request-unchecked+reply
                    slothbar--connection
                    (make-instance 'xcb:GetGeometry
                                   :drawable (slothbar-util--find-root-window-id))))
               ((map ('y-offset mon-y-offset) ('height mon-height))
                (slothbar--find-display-geometry slothbar-preferred-display))
               (bottom-strut (if slothbar-is-bottom
                                   (+ slothbar-height slothbar-margin-y
                                      (- root-window-height
                                         (+ mon-y-offset mon-height)))
                                 0)))
    (xcb:+request slothbar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STRUT
                       :window slothbar--window
                       :left slothbar-offset-x
                       :right 0
                       :top (if slothbar-is-bottom
                                0
                              (+ slothbar-height slothbar-offset-y
                                 slothbar-margin-y))
                       :bottom bottom-strut))
    (xcb:+request slothbar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STRUT_PARTIAL
                       :window slothbar--window
                       :left slothbar-offset-x
                       :right 0
                       :top (if slothbar-is-bottom 0
                              (+ slothbar-height slothbar-offset-y
                                 slothbar-margin-y))
                       :bottom bottom-strut
                       :left-start-y 0
                       :left-end-y 0
                       :right-start-y 0
                       :right-end-y 0
                       :top-start-x (if slothbar-is-bottom 0
                                      slothbar-offset-x)
                       :top-end-x (if slothbar-is-bottom 0
                                    (1- (+ slothbar-offset-x slothbar-width)))
                       :bottom-start-x (if slothbar-is-bottom
                                           slothbar-offset-x
                                         0)
                       :bottom-end-x (if slothbar-is-bottom
                                         (1- (+ slothbar-offset-x slothbar-width))
                                       0))))
  ;; (xcb:+request slothbar--connection
  ;;     (make-instance 'xcb:MapWindow
  ;;                    :window slothbar--window))
  (xcb:flush slothbar--connection))

(defun slothbar--on-DestroyNotify (data _synthetic)
  "DestroyNotify.
DATA the event data"
  (slothbar--log-trace* "received destroynotify %s" data))

(defun slothbar--on-ReparentNotify (data _synthetic)
  "ReparentNotify.
DATA the event data"
  (slothbar--log-trace* "received reparentnotify %s" data))

(defun slothbar--on-ResizeRequest (data _synthetic)
  "ResizeRequest.
DATA the event data"
  (slothbar--log-trace* "received resizerequest %s" data))

(defun slothbar--on-PropertyNotify (data _synthetic)
  "PropertyNotify.
DATA the event data"
  (slothbar--log-trace* "received propertynotify %s" data))

(defun slothbar--on-ClientMessage (data _synthetic)
  "Handle client messages.
DATA the event data"
  (slothbar--log-trace* "received clientmessage %s" data))

(defun slothbar--on-KeyPress (data _synthetic)
  "Forward all KeyPress events to Emacs frame.
DATA the event data"
  ;; This function a workspace frame has the input focus and the pointer is
  ;; over a tray icon.
  (let ((dest (frame-parameter (selected-frame) 'exwm-outer-id))
        (obj (make-instance 'xcb:KeyPress)))
    (xcb:unmarshal obj data)
    (setf (slot-value obj 'event) dest)
    (xcb:+request slothbar--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0
                       :destination dest
                       :event-mask xcb:EventMask:NoEvent
                       :event (xcb:marshal obj slothbar--connection)))
    (slothbar--log-trace* "key press %s" obj))
  (xcb:flush slothbar--connection))

(defun slothbar--selectively-clear-areas (prev-extents new-extents)
  "Clear old areas that the new extents do not cover.
PREV-EXTENTS the previous layout extents
NEW-EXTENTS the new layout extents"
  (let ((to-clear (slothbar-layout-subtract-extents new-extents prev-extents)))
    (pcase-dolist (`(,l ,r) to-clear)
      (xcb:+request slothbar--connection
          (make-instance 'xcb:ClearArea
                         :exposures 0
                         :window slothbar--window
                         :x l :y 0
                         :width (- r l) :height slothbar-height)))))

(defun slothbar--copy-areas (layout)
  "Copy a LAYOUT's modules' pixmaps into their respective areas."
  (dolist (m layout)
    (pcase-let ((`((,x ,y) ,(cl-struct slothbar-module width xcb)) m))
      (when (alist-get 'pixmap xcb)
        (xcb:+request slothbar--connection
            (make-instance 'xcb:CopyArea
                           :src-drawable (alist-get 'pixmap xcb)
                           :dst-drawable slothbar--window
                           :gc (alist-get 'gc xcb)
                           :src-x 0 :src-y 0 :dst-x x :dst-y y
                           :width width :height slothbar-height)))
      (slothbar-module-reposition (cadr m) x y))))

(defvar slothbar--geometry-changed? nil "Held by `slothbar--on-Expose'.")
(cl-defun slothbar-refresh-modules (&optional modules)
  "Ask the modules to refresh and see whether the layout has changed.
MODULES optional modules to refresh and compare with prev-extents"
  (when slothbar--geometry-changed?
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (setf (slothbar-module-needs-refresh? m) t
              (slothbar-module-cache m) (make-hash-table :test 'equal))))
    (slothbar--refresh))
  ;; (message "refreshing modules")
  ;; refresh modules to update to latest dimensions
  (let ((prev-extents
         (slothbar-layout-extents
          (slothbar-layout-coordinate (slothbar-layout slothbar--modules) 0 0)))
        (slothbar--modules (or modules slothbar--modules)))
    ;; (message "prev extents %s" prev-extents)
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (slothbar-module-refresh m)))
    (let* ((new-layout
            (slothbar-layout-coordinate (slothbar-layout slothbar--modules) 0 0))
           (new-extents (slothbar-layout-extents new-layout)))
      ;; (message "prev extents %s new extents %s" prev-extents new-extents)
      (when (not (equal prev-extents new-extents))
        ;; (message "layout has changed")
        (slothbar--selectively-clear-areas prev-extents new-extents))
      (slothbar--copy-areas new-layout)))
  (xcb:flush slothbar--connection))

(defun slothbar--watch-modules (_ nval oper where)
  "With OPER eq \\='set and nil WHERE, refresh `slothbar--modules' with NVAL."
  (when (and slothbar--enabled (not where) (eq 'set oper))
    ;; exit modules that have been removed
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (unless (seq-contains-p nval m #'eq)
          (slothbar-module-exit m))))
    ;; check for uninitialized modules
    (dolist (m nval)
      (when (slothbar-module-p m)
        (unless (slothbar-module-xcb m)
          (slothbar-module-init m))))
    (slothbar-refresh-modules nval)))

(add-variable-watcher 'slothbar--modules #'slothbar--watch-modules)

(defvar slothbar--module-refresh-timer nil)
(defun slothbar--start-module-refresh-timer ()
  "Start a timer to periodically refresh the modules."
  (setq slothbar--module-refresh-timer
        (run-at-time nil 10 #'slothbar-refresh-modules)))

(defun slothbar--on-Expose (data _synthetic)
  "Can draw things after Expose.
DATA the event data"
  (slothbar--log-debug* "slothbar received expose %s" data)
  (ignore data)
  (when slothbar--enabled
    (when slothbar--module-refresh-timer
      (slothbar--log-debug* "slothbar restarting module refresh timer")
      (cancel-timer slothbar--module-refresh-timer)
      (setq slothbar--module-refresh-timer nil))
    (slothbar--start-module-refresh-timer))
  (when (and slothbar--enabled slothbar--geometry-changed?)
    (setq slothbar--geometry-changed? nil)
    (run-at-time 0 nil #'slothbar-refresh-modules)))

(defun slothbar--watch-height (_ _ oper where)
  "Refresh modules when changing `slothbar-height'.

Refresh only when OPER eq \\='set and WHERE is nil."
  (when (and slothbar--enabled (not where) (eq 'set oper))
    (setq slothbar--geometry-changed? t)
    (run-at-time 0 nil #'slothbar-refresh-modules)))

(add-variable-watcher 'slothbar-height #'slothbar--watch-height)

(defun slothbar--construct-modules ()
  "Construct modules from layout given in `slothbar-modules'."
    (setq slothbar--modules
          (mapcar (lambda (val)
                    (cond
                     ((or (keywordp val) (slothbar-module-p val))
                      val)
                     ((functionp val)
                      (funcall val))
                     ((and (listp val) (functionp (car val)))
                      (apply (car val) (cdr val)))
                     (t (error "Unsupported type in slothbar-modules: %s" val))))
                  slothbar-modules)))

(defun slothbar--watch-slothbar-modules (_ _ oper where)
  "(Re)construct modules when `slothbar-modules' is modified.

Do this only when OPER eq \\='set and WHERE is nil."
  (when (and (not where) (eq 'set oper))
    (run-at-time 0 nil #'slothbar--construct-modules)))

(defun slothbar--start ()
  "Start slothbar.
Initialize the connection, window, graphics context, and modules."
  (run-hook-with-args 'slothbar-before-init-hook)
  (cl-assert (not slothbar--connection))
  (cl-assert (not slothbar--window))
  (slothbar--log-enable-logging)
  (let ((geom (slothbar--find-display-geometry slothbar-preferred-display)))
    (setq slothbar-offset-x (alist-get 'x-offset geom)
	  slothbar-offset-y (if slothbar-is-bottom
                               (- (+ (alist-get 'y-offset geom)
                                     (alist-get 'height geom))
                                  slothbar-height)
                             (alist-get 'y-offset geom))
	  slothbar-width (alist-get 'width geom)))
  (setq slothbar-font-px-size
        (slothbar-font--precompute-px-sizes
         slothbar-height slothbar-font--color-code-map))
  (setq slothbar--connection (xcb:connect))
  ;; apparently ewmh initializes icccm automatically
  (xcb:ewmh:init slothbar--connection)
  ;; (xcb:icccm:init slothbar--connection)
  (set-process-query-on-exit-flag (slot-value slothbar--connection
                                              'process)
                                  nil)
  ;; initialize the bar window
  (let ((id (xcb:generate-id slothbar--connection))
        (background-pixel (slothbar-util--color->pixel
                           (slothbar-util--find-background-color)))
        (y slothbar-offset-y)
        parent depth)
    (setq slothbar--window id)
    (slothbar--log-debug* "Slothbar window id: %s" slothbar--window)
    (setq parent (slothbar-util--find-root-window-id)
          depth (slot-value (xcb:+request-unchecked+reply
                                slothbar--connection
                                (make-instance 'xcb:GetGeometry
                                               :drawable parent))
                            'depth))
    (xcb:+request slothbar--connection
        (make-instance 'xcb:CreateWindow
                       :depth depth
                       :wid id
                       :parent parent
                       :override-redirect 1
                       :x slothbar-offset-x
                       :y y
                       :width 1
                       :height (+ slothbar-height)
                       :border-width 1
                       :class xcb:WindowClass:InputOutput
                       :visual 0
                       :value-mask (logior xcb:CW:BackPixmap
                                           (if background-pixel
                                               xcb:CW:BackPixel 0)
                                           xcb:CW:EventMask)
                       :background-pixmap xcb:BackPixmap:ParentRelative
                       :background-pixel background-pixel
                       :event-mask (logior xcb:EventMask:Exposure
                                           xcb:EventMask:KeyPress
                                           xcb:EventMask:PointerMotion
                                           xcb:EventMask:PropertyChange
                                           xcb:EventMask:SubstructureNotify)))
    ;; Set WM_NAME and WM_CLASS.
    (xcb:+request slothbar--connection
        (make-instance 'xcb:icccm:set-WM_NAME
                       :window id
                       :data "slothbar"))
    (xcb:+request slothbar--connection
        (make-instance 'xcb:icccm:set-WM_CLASS
                       :window id
                       :instance-name "slothbar"
                       :class-name "Slothbar"))
    ;; dock the window
    (xcb:+request slothbar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK)))
    ;; state is sticky and above
    (xcb:+request slothbar--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window id
                       :data `(,xcb:Atom:_NET_WM_STATE_STICKY
                               ,xcb:Atom:_NET_WM_STATE_ABOVE)))
    ;; create gc
    (setq slothbar--gc (xcb:generate-id slothbar--connection))
    (let ((egc
           (xcb:+request-checked+request-check slothbar--connection
               (make-instance 'xcb:CreateGC
                              :cid slothbar--gc
                              :drawable slothbar--window
                              :value-mask (logior xcb:GC:Background
                                                  xcb:GC:Foreground)
                              :background (slothbar-util--color->pixel
                                           (slothbar-util--find-background-color))
                              :foreground (slothbar-util--color->pixel
                                           (slothbar-util--find-foreground-color))))))
      (slothbar--log-debug* "slothbar init create gc errors: %s" egc))
    ;; initialize modules
    (slothbar--construct-modules)
    (add-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
    (dolist (m slothbar--modules)
      (when (slothbar-module-p m)
        (slothbar-module-init m)))
    (xcb:flush slothbar--connection)
    ;; Attach event listeners.
    (xcb:+event slothbar--connection 'xcb:DestroyNotify
                #'slothbar--on-DestroyNotify)
    (xcb:+event slothbar--connection 'xcb:ReparentNotify
                #'slothbar--on-ReparentNotify)
    (xcb:+event slothbar--connection 'xcb:ResizeRequest
                #'slothbar--on-ResizeRequest)
    (xcb:+event slothbar--connection 'xcb:PropertyNotify
                #'slothbar--on-PropertyNotify)
    (xcb:+event slothbar--connection 'xcb:ClientMessage
                #'slothbar--on-ClientMessage)
    (xcb:+event slothbar--connection 'xcb:KeyPress
                #'slothbar--on-KeyPress)
    (xcb:+event slothbar--connection 'xcb:Expose
                #'slothbar--on-Expose)
    (slothbar--refresh)
    (setq slothbar--enabled t)
    (run-hook-with-args 'slothbar-after-init-hook)))

(defun slothbar-refresh ()
  "Refresh slothbar according to geometry and settings."
  (interactive)
  (let ((geom (slothbar--find-display-geometry slothbar-preferred-display)))
    (setq slothbar-offset-x (alist-get 'x-offset geom)
	  slothbar-offset-y (if slothbar-is-bottom
                                (- (+ (alist-get 'y-offset geom)
                                      (alist-get 'height geom))
                                   slothbar-height)
                              (alist-get 'y-offset geom))
	  slothbar-width (alist-get 'width geom)
          slothbar--geometry-changed? t)
    (run-at-time 0 nil #'slothbar-refresh-modules)))

;;;###autoload
(defun slothbar ()
  "Start slothbar."
  (interactive)
  (if (and (display-graphic-p) (eq 'x window-system))
      (fontsloth-async-load-and-cache-fonts
       (slothbar-font-map-candidates)
       :finish-func (lambda (_) (slothbar--start)))
    (message "Slothbar requires an X window system display to run")))

;;;###autoload
(defun slothbar-exit ()
  "Exit slothbar."
  (interactive)
  (run-hook-with-args 'slothbar-before-exit-hook)
  ;; exit modules
  (when slothbar--module-refresh-timer
    (cancel-timer slothbar--module-refresh-timer)
    (setq slothbar--module-refresh-timer nil))
  (setq slothbar--enabled nil)
  (dolist (m slothbar--modules)
    (when (slothbar-module-p m)
      (slothbar-module-exit m)))
  (remove-variable-watcher 'slothbar-modules #'slothbar--watch-slothbar-modules)
  (when slothbar--connection
    (when slothbar--window
      (xcb:+request slothbar--connection
          (make-instance 'xcb:UnmapWindow
                         :window slothbar--window)))
    (when slothbar--gc
      (xcb:+request slothbar--connection
          (make-instance 'xcb:FreeGC
                         :gc slothbar--gc)))
    (xcb:disconnect slothbar--connection)
    (setq slothbar--connection nil
          slothbar--window nil
          slothbar--gc nil)
    (run-hook-with-args 'slothbar-after-exit-hook)))

(provide 'slothbar)

;;; slothbar.el ends here
