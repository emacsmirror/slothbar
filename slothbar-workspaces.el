;;; slothbar-workspaces.el --- An slothbar workspaces module  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.28.2
;; Homepage: https://codeberg.org/agnes-li/slothbar
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

;; This is an implementation of `slothbar-module' for active/current
;; workspaces information.

;; To use this module, add it to `slothbar-modules' with any desired
;; layout insructions.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'rx)
(require 'xcb)
(require 'xcb-ewmh)

(require 'slothbar-log)
(require 'slothbar-module-)

(defgroup slothbar-workspaces nil
  "An Slothbar workspaces module."
  :group 'slothbar)

(defcustom slothbar-workspaces-generate-list-fn (lambda () '())
  "A function generating a workspaces status list.

E.g.:
\(lambda ()
  \\='((\"0\" (:window))
    (\"1\" (:current))
    (\"2\" (:blankish))
    (\"3\" (:current :blankish))
    (\"4\" (:current :window))))
would indicate:
- that a workspace named 0 is not current, but has a window
- workspace 1 is current and may or may not have a window
- workspace 2 is not current and is blank(ish)
- workspace 3 is current and is blank(ish)
- workspace 4 is current and has a window

The possible status keywords are :window, :current, and :blankish. If
:current appears in the status list for a workspace, it is expected at
the head of the list."
  :type 'function
  :group 'slothbar-workspaces)

(cl-defstruct (slothbar-workspaces
               (:include slothbar-module (name "workspaces") (icon ?󰯉)
                         (format "^f2^2^[^5^f4%i^]%w")
                         (format-fn #'slothbar-workspaces-format-format))
               (:constructor slothbar-workspaces-create)
               (:copier nil)))

(defcustom slothbar-workspaces-before-init-hook nil
  "Hook to run before module init."
  :type 'hook
  :group 'slothbar-workspaces)

(defun slothbar-workspaces--format-fn-spec (ws-list)
  "Generate a spec suitable for `format-spec' from data in WS-LIST.

See `slothbar-workspaces-generate-list-fn' for the expected structure of
WS-LIST."
  (cl-flet ((sanitize (s)
              (string-replace " " "-"
               (replace-regexp-in-string "[\\^]" "^^" (string-trim s)))))
    `((?w . ,(cl-loop for (ws status) in ws-list
                      for ws-safe = (sanitize ws)
                      concat
                      (pcase status
                        ((or '(:current) '(:current :window))
                         (concat "^[^1 [" ws-safe "]^]"))
                        ((or '(:window) `(:window ,_))
                         (concat "^[^1 " ws-safe "^]"))
                        (`(:current ,_) (concat "^[^0 [" ws-safe "]^]"))
                        ('(:blankish) (concat "^[^2 " ws-safe "^]"))))))))

(defun slothbar-workspaces-format-format (m)
  "Format M's format string."
  (let ((ws-list (or (map-elt (slothbar-module-cache m) 'ws-list)
                     (funcall slothbar-workspaces-generate-list-fn))))
    (format-spec (slothbar-module-format m)
                 (slothbar-workspaces--format-fn-spec ws-list) t)))

(defun slothbar-workspaces--format-spec (icon)
  "Build the `format-spec' spec used to generate module text given ICON."
  `((?i . ,(string icon))))

(cl-defmethod slothbar-module-update-status ((m slothbar-workspaces))
  "Update M's text and format spec."
  (let* ((format-spec (slothbar-workspaces--format-spec (slothbar-module-icon m)))
         (ws-list (funcall slothbar-workspaces-generate-list-fn))
         (txt (format-spec (slothbar-module-format m) format-spec t))
         (cache (slothbar-module-cache m)))
    (unless (equal ws-list (map-elt cache 'ws-list))
      (when cache
        (map-put! cache 'ws-list ws-list))
      (setf (slothbar-module-format-spec m) format-spec
            (slothbar-module-text m) txt
            (slothbar-module-needs-refresh? m) t))))

(cl-defmethod slothbar-module-init :before ((m slothbar-workspaces))
  "Set the M's icon and update the text."
  (run-hooks 'slothbar-workspaces-before-init-hook)
  (slothbar-module-update-status m))

(defun slothbar-workspaces--refresh-hook-fn (&rest _)
  "Refresh all workspaces modules, suitable for hooks."
  (slothbar-module--refresh-all-by-name "workspaces"))


;;; Begin slothbar-workspaces-generate-list-fn implementations

(declare-function shorten-strings "shorten" (lst &optional tail-count))

;;; EXWM

(defvar exwm-workspace--list)

(defun slothbar-workspaces--shorten-frame-buffer-names-exwm ()
  "Return an alist mapping EXWM frame buffer names to shortened names.

If shorten is not available, return nil."
  (when (featurep 'shorten)
    (shorten-strings
     (mapcar (lambda (frame)
               (buffer-name (car (buffer-list frame))))
             exwm-workspace--list))))

(declare-function exwm-workspace--count "exwm-workspace")
(declare-function exwm-workspace--position "exwm-workspace" (frame))

(defcustom slothbar-workspaces-exwm-shorten-names t
  "Non-nil to display shortened buffer names or nil for numbers.

This only happens if the shorten library is available and the particular
window manager integration supports it."
  :type 'boolean
  :group 'slothbar-workspaces)

(defcustom slothbar-workspaces-exwm-add-change-functions nil
  "Non-nil to update when changing the window selection or buffer.

Nil to update lazily."
  :type 'boolean
  :group 'slothbar-workspaces)

(defun slothbar-workspaces--watch-exwm-add-change-functions (_ new oper where)
  "Watch variable `slothbar-workspaces-exwm-add-change-functions'.

If ever OPER is equal to \\='set and the NEW value is not a buffer local
value indicated by WHERE, call
`slothbar-workspaces--exwm-modify-change-functions' with the value of
NEW."
  (when (and (not where) (eq 'set oper))
    (slothbar-workspaces--exwm-modify-change-functions new)))

(add-variable-watcher 'slothbar-workspaces-exwm-add-change-functions
                      'slothbar-workspaces--watch-exwm-add-change-functions)

(defvar exwm--id-buffer-alist)
(defvar exwm--frame)
(defvar exwm-workspace--current)

(defun slothbar-workspaces-generate-list-fn-exwm ()
  "Implement `slothbar-workspaces-generate-list-fn' for EXWM.

If the shorten library is available (from circe), the displayed names
are shortened buffer names.  Otherwise, names are the workspace numbers."
  (let ((not-empty (make-vector (exwm-workspace--count) nil))
        (shorts (slothbar-workspaces--shorten-frame-buffer-names-exwm)))
    (dolist (i exwm--id-buffer-alist)
      (with-current-buffer (cdr i)
        (when exwm--frame
          (setf (aref not-empty
                      (exwm-workspace--position exwm--frame))
                t))))
    (cl-loop for frame in exwm-workspace--list
             for pos = (exwm-workspace--position frame)
             for sn = (map-elt shorts (buffer-name (car (buffer-list frame))))
             collect
             `(,(or (and slothbar-workspaces-exwm-shorten-names sn)
                    (number-to-string pos))
               (,@(when (eq exwm-workspace--current frame)
                    '(:current))
                ,@(when (aref not-empty pos)
                    '(:window))
                ,@(unless (aref not-empty pos)
                    '(:blankish)))))))

(defun slothbar-workspaces--exwm-modify-change-functions (add?)
  "Add window buffer and selection change functions when ADD? is non-nil.
Remove chnage functions when ADD? is non-nil."
  (if add?
      (progn (add-hook 'window-selection-change-functions
                       #'slothbar-workspaces--refresh-hook-fn)
             (add-hook 'window-buffer-change-functions
                       #'slothbar-workspaces--refresh-hook-fn))
    (remove-hook 'window-selection-change-functions
                 #'slothbar-workspaces--refresh-hook-fn)
    (remove-hook 'window-buffer-change-functions
                 #'slothbar-workspaces--refresh-hook-fn)))

(defvar exwm--connection)

(defun slothbar-workspaces-setup-defaults-exwm ()
  "Configure slothbar-workspaces to display EXWM workspaces."
  (when (and (boundp 'exwm--connection) exwm--connection)
    (when (and slothbar-workspaces-exwm-shorten-names
               (locate-library "shorten"))
      (require 'shorten))
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-exwm)
    (add-hook 'exwm-workspace-switch-hook
              #'slothbar-workspaces--refresh-hook-fn)
    (slothbar-workspaces--exwm-modify-change-functions
     slothbar-workspaces-exwm-add-change-functions)))

(when (locate-library "exwm")
  (add-hook 'slothbar-before-init-hook #'slothbar-workspaces-setup-defaults-exwm))

;;; herbstluftwm

(defun slothbar-workspaces-generate-list-fn-herbstluftwm ()
  "Implement `slothbar-workspaces-generate-list-fn' for herbstluftwm."
  (when (executable-find "herbstclient")
    (when-let ((tags (with-temp-buffer
                       (when (eq 0 (call-process "herbstclient" nil t nil "tag_status"))
                         (buffer-string)))))
      (cl-loop for tag in (string-split tags)
               for name = (substring tag 1)
               collect
               `(,name
                 (,@(pcase tag
                    ((rx (seq ?# (+ anychar))) '(:current :window))
                    ((rx (seq ?: (+ anychar))) '(:window))
                    ((rx (seq ?. (+ anychar))) '(:blankish)))))))))

(defun slothbar-workspaces--make-herbstluft-events-filter ()
  "Return a process filter for herbstclient -i to monitor tags."
  (let ((processed-lines 0))
    (lambda (proc string)
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              ;; Insert the text, advancing the process marker.
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc))))
          (save-excursion
            (goto-char (point-max))
            (beginning-of-line (and (looking-at-p "^$") 0))
            (when (> (line-number-at-pos) processed-lines)
              (setq processed-lines (line-number-at-pos))
              (when (string-prefix-p
                     "tag_changed"
                     (buffer-substring-no-properties (point) (line-end-position)))
                (slothbar-module--refresh-all-by-name "workspaces")))))))))

(defvar slothbar-workspaces--herbstluft-evt-listener-proc nil)

(defun slothbar-workspaces--start-herbstluft-event-listener ()
  "Start a process to listen for herbstluftwm events."
  (let* ((stdout (generate-new-buffer " *herbstluftwm events*"))
         (stderr (generate-new-buffer " *herbstluftwm errors*"))
         (process (make-process :name "herbstclient"
                      :command '("herbstclient" "-i")
                      :buffer stdout
                      :filter (slothbar-workspaces--make-herbstluft-events-filter)
                      :stderr stderr)))
    (unless process
      (error "Process unexpectedly nil"))
    (setq slothbar-workspaces--herbstluft-evt-listener-proc process)))

(defun slothbar-workspaces--stop-herbstluft-event-listener ()
  "Stop process to listen for herbstluftwm events."
  (when (and slothbar-workspaces--herbstluft-evt-listener-proc
             (process-live-p slothbar-workspaces--herbstluft-evt-listener-proc))
    (kill-process slothbar-workspaces--herbstluft-evt-listener-proc)))

(defun slothbar-workspaces-setup-defaults-herbstluftwm ()
  "Configure slothbar-workspaces to display herbstluftwm tags."
  (when (and (executable-find "herbstclient")
             (equal "herbstluftwm" (getenv "DESKTOP_SESSION")))
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-herbstluftwm)
    (slothbar-workspaces--start-herbstluft-event-listener)
    (add-hook 'slothbar-after-exit-hook #'slothbar-workspaces--stop-herbstluft-event-listener)))

(add-hook 'slothbar-before-init-hook #'slothbar-workspaces-setup-defaults-herbstluftwm)

;;; xmonad

(defvar slothbar-workspaces--xmonad-dbus-last-val nil
  "Holds the most recent dbus message from xmonad.")

(defun slothbar-workspaces-generate-list-fn-xmonad ()
  "Implement `slothbar-workspaces-generate-list-fn' for xmonad.

In order for this to work, a section in xmonad.hs like the following:

slothbarHook :: D.Client -> PP
slothbarHook dbus =
  let wrapper c s | s /= \"NSP\" = wrap (\":\" <> c <> \" \") \" . \" s
                  | otherwise  = mempty
      cur   = \"current\"
      vis   = \"visible\"
      urg   = \"urgent\"
      hid   = \"window\"
      bla   = \"blankish\"
  in  def { ppOutput          = dbusOutput dbus
          , ppVisible         = wrapper vis
          , ppHidden          = wrapper hid
          , ppHiddenNoWindows = wrapper bla
          , ppUrgent          = wrapper urg
          , ppCurrent         = wrapper cur
          , ppTitle           = wrapper \"title\" . shorten 90
          }

mySlothbarLogHook dbus = myLogHook <+> dynamicLogWithPP (slothbarHook dbus)"
  (when (stringp slothbar-workspaces--xmonad-dbus-last-val)
    (cl-loop for (status name)
             in (mapcar 'string-split
                        (butlast (string-split
                                  slothbar-workspaces--xmonad-dbus-last-val
                                  " \\. "
                                  t)))
             collect `(,name
                       (,(intern status))))))

(defun slothbar-workspaces--watch-xmonad-dbus-last-val (_ _ oper where)
  "Refresh if the dbus message variable is modified.

Only do this when WHERE is nil and OPER eq \\='set."
  (when (and (not where) (eq 'set oper))
    (run-with-timer 0 nil (lambda () (slothbar-module--refresh-all-by-name "workspaces")))))

(add-variable-watcher 'slothbar-workspaces--xmonad-dbus-last-val
                      #'slothbar-workspaces--watch-xmonad-dbus-last-val)

(defun slothbar-workspaces--xmonad-dbus-monitor (&optional workspaces)
  "Set `slothbar-workspaces--xmonad-dbus-last-val' to the value of WORKSPACES.

This is intended as a dbus monitor to set a value for
`slothbar-workspaces-generate-list-fn-xmonad' to parse."
  (when workspaces
    (setq slothbar-workspaces--xmonad-dbus-last-val workspaces)))

(defvar slothbar-workspaces--xmonad-dbus-object nil
  "The state object for the xmonad dbus monitor.")

(declare-function dbus-unregister-object "dbus")

(defun slothbar-workspaces--unregister-xmonad-dbus-monitor ()
  "Unregister the dbus object for the xmonad monitor."
  (dbus-unregister-object slothbar-workspaces--xmonad-dbus-object))

(declare-function dbus-register-monitor "dbus")

(defun slothbar-workspaces-setup-defaults-xmonad ()
  "Try to start a dbus monitor and set `slothbar-workspaces-generate-list-fn'.

This checks DESKTOP_SESSION to determine if xmonad is the current session.

Note that the xmonad config must send dbus events.  See the
`slothbar-workspaces-generate-list-fn-xmonad' docstring for an example."
  (when (and (locate-library "dbus")
             (equal "xmonad" (getenv "DESKTOP_SESSION")))
    (setq
     slothbar-workspaces--xmonad-dbus-object
     (dbus-register-monitor :session
                            #'slothbar-workspaces--xmonad-dbus-monitor
                            :path "/org/xmonad/Log"
                            :interface "org.xmonad.Log"
                            :member "Update")
     slothbar-workspaces-generate-list-fn
     #'slothbar-workspaces-generate-list-fn-xmonad)
    (add-hook 'slothbar-after-exit-hook #'slothbar-workspaces--unregister-xmonad-dbus-monitor)))

(add-hook 'slothbar-before-init-hook #'slothbar-workspaces-setup-defaults-xmonad)

;;; stumpwm

(defcustom slothbar-workspaces-stumpwm-log-prop-name "_STUMPWM_LOG"
  "The name for a root window property containing the stumpwm log."
  :type 'string
  :group 'slothbar-workspaces)

(defvar slothbar-workspaces--stumpwm-log-atom nil
  "Holds the numeric value for the interned _stumpwm_log atom.")

(defun slothbar-workspaces--intern-stumpwm-log-atom ()
  "Intern and the atom for the _stumpwm_log property.

Set `slothbar-workspaces--stumpwm-log-atom' to hold its value."
  (when-let ((slothbar--connection)
             (prop-name slothbar-workspaces-stumpwm-log-prop-name))
    (unless slothbar-workspaces--stumpwm-log-atom
      (setq slothbar-workspaces--stumpwm-log-atom
            (slot-value (xcb:+request-unchecked+reply slothbar--connection
                            (make-instance 'xcb:InternAtom
                                           :only-if-exists 0
                                           :name-len (length "_STUMPWM_LOG")
                                           :name "_STUMPWM_LOG"))
                        'atom)))))

(defun slothbar-workspaces--read-stumpwm-log-prop ()
  "Read the current value of the _stumpwm_log root window property if any.

Returns nil if no property is found or if an error occurs reading the
value as an s-expression."
  (slothbar-workspaces--intern-stumpwm-log-atom)
  (when slothbar--connection
    (when-let* ((req (make-instance
                      'xcb:ewmh:-GetProperty-utf8
                      :window (slothbar-util--find-root-window-id)
                      :property slothbar-workspaces--stumpwm-log-atom))
                (log (xcb:+request-unchecked+reply slothbar--connection req))
                (val (slot-value log 'value)))
      (condition-case err
          (car (read-from-string val))
        (error (slothbar--log-warn*
                "slothbar-workspaces: failed to read %s as sexp: %s"
                val err))))))

(defvar slothbar-workspaces--stumpwm-group-status-rx
  (rx (seq (group (seq (? (any ?- ?+))
                       (+ digit)))
           (group (any ?* ?= ?+ ?-))
           (group (+ (not space)))))
  "A regular expression to match the expected value of a group status entry.

See `slothbar-workspaces-generate-list-fn-stumpwm' for the expected
structure of the root window property value.")

(defvar slothbar-workspaces--stumpwm-log-last-val nil
  "To store the last read value of the _stumpwm_log property.")

(defun slothbar-workspaces-generate-list-fn-stumpwm ()
  "Implement `slothbar-workspaces-generate-list-fn' for stumpwm.

This expects a root window property called _STUMPWM_LOG to contain a
utf-8 string representation of a list of strings.  Each element of the
list should be of the format <number>status><name>.

Number is the group number.

Status is a single char indicator \\='*' for current, \\='=' for hidden
with window, and \\='-' for hidden with no windows.  It is expected that
\\='*' and \\='=' take precedence over \\='+' for previously selected
group.

Name is the group name."
  (cl-loop for group-info in slothbar-workspaces--stumpwm-log-last-val
           when (string-match
                 slothbar-workspaces--stumpwm-group-status-rx
                 group-info)
           collect
           `(,(concat (match-string 1 group-info)
                      " " (match-string 3 group-info))
             (,@(pcase (match-string 2 group-info)
                ("*" '(:current))
                ("=" '(:window))
                ((or "-" "+") '(:blankish)))))))

(defun slothbar-workspaces--on-property-notify-stumpwm (data _synthetic)
  "Listen for PropertyNotify events for the _stumpwm_log root window property.

DATA contains the marshalled event data.

If the event is for the root window and the _stumpwm_log property, set
`slothbar-workspaces--stumpwm-log-last-val' to the current property
value and refresh all instances of the workspaces module."
  (let ((obj (make-instance 'xcb:PropertyNotify))
        atom id)
    (xcb:unmarshal obj data)
    (setq id (slot-value obj 'window)
          atom (slot-value obj 'atom))
    (when (and (= id (slothbar-util--find-root-window-id))
               (= atom slothbar-workspaces--stumpwm-log-atom))
      (setq slothbar-workspaces--stumpwm-log-last-val
            (slothbar-workspaces--read-stumpwm-log-prop))
      (slothbar-module--refresh-all-by-name "workspaces"))))

(defun slothbar-workspaces-setup-defaults-stumpwm ()
  "If in stumpwm, set `slothbar-workspaces-generate-list-fn' and add a listener.

The event listener listens for PropertyNotify events.

This checks DESKTOP_SESSION to determine if stumpwm is the current session."
  (when (equal "stumpwm" (getenv "DESKTOP_SESSION"))
    (setq slothbar-workspaces-generate-list-fn
          #'slothbar-workspaces-generate-list-fn-stumpwm)
    (add-hook 'slothbar-workspaces-before-init-hook
              (lambda ()
                (setq slothbar-workspaces--stumpwm-log-last-val
                      (slothbar-workspaces--read-stumpwm-log-prop))
                (xcb:+event
                 slothbar--connection
                 'xcb:PropertyNotify
                 #'slothbar-workspaces--on-property-notify-stumpwm)))))

(add-hook 'slothbar-before-init-hook #'slothbar-workspaces-setup-defaults-stumpwm)

(provide 'slothbar-workspaces)
;;; slothbar-workspaces.el ends here
