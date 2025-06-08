;;; exlybar-workspaces.el --- An exlybar workspaces module  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.27.0
;; Package-Requires: ((cl-lib "0.5") (xelb "0.18") (dash "2.1.0") (f "0.20.0") (s "1.12.0") (emacs "27.1"))
;; Keywords: window-manager, status-bar, exwm

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

;; This is an implementation of `exlybar-module' for active/current
;; workspaces information.

;; To use this module, add it to `exlybar-modules' with any desired
;; layout insructions.

;;; Code:

(require 'cl-lib)
(require 'rx)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(defgroup exlybar-workspaces nil
  "An Exlybar workspaces module."
  :group 'exlybar)

(defcustom exlybar-workspaces-generate-list-fn (lambda () '())
  "A function generating a workspaces status list.

E.g.:
(lambda ()
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
the head of the list.
"
  :type 'function
  :group 'exlybar-workspaces)

(cl-defstruct (exlybar-workspaces
               (:include exlybar-module (name "workspaces") (icon ?󰯉)
                         (format "^f2^2^[^5^f4%i^]%w")
                         (format-fn 'exlybar-workspaces-format-format))
               (:constructor exlybar-workspaces-create)
               (:copier nil)))

(defun exlybar-workspaces--format-fn-spec (ws-list)
  "Generate a spec suitable for `format-spec' from data in WS-LIST.

See `exlybar-workspaces-generate-list-fn' for the expected structure of
WS-LIST."
  (cl-flet ((sanitize (s)
              (string-replace " " "-"
               (replace-regexp-in-string "[\\^]" "^^" (string-trim s)))))
    `((?w . ,(cl-loop for (ws status) in ws-list
                      for ws = (sanitize ws)
                      concat
                      (pcase status
                        ('(:current :window) (concat "^[^1 [" ws "]^]"))
                        ((or '(:window) `(:window ,_)) (concat "^[^1 " ws "^]"))
                        (`(:current ,_) (concat "^[^0 [" ws "]^]"))
                        ('(:blankish) (concat "^[^2 " ws "^]"))))))))

(defun exlybar-workspaces-format-format (m)
  "Format M's format string."
  (let ((ws-list (funcall exlybar-workspaces-generate-list-fn)))
    (format-spec (exlybar-module-format m)
                 (exlybar-workspaces--format-fn-spec ws-list) t)))

(defun exlybar-workspaces--format-spec (icon)
  "Build the `format-spec' spec used to generate module text given ICON."
  `((?i . ,(string icon))))

(cl-defmethod exlybar-module-update-status ((m exlybar-workspaces))
  "Update M's text and format spec."
  (let* ((format-spec (exlybar-workspaces--format-spec (exlybar-module-icon m)))
         (txt (format-spec (exlybar-module-format m) format-spec t)))
    (setf (exlybar-module-format-spec m) format-spec
          (exlybar-module-text m) txt
          (exlybar-module-needs-refresh? m) t)))

(cl-defmethod exlybar-module-init :before ((m exlybar-workspaces))
  "Set the M's icon and update the text."
  (exlybar-module-update-status m))

(defun exlybar-workspaces--refresh-hook-fn (&rest _)
  "Refresh all workspaces modules, suitable for hooks."
  (exlybar-module-refresh-all-by-name "workspaces"))


;;; Begin exlybar-workspaces-generate-list-fn implementations

(declare-function shorten-strings "shorten" (lst &optional tail-count))

;;; EXWM

(defvar exwm-workspace--list)

(defun exlybar-workspaces--shorten-frame-buffer-names-exwm ()
  "Return an alist mapping EXWM frame buffer names to shortened names.

If shorten is not available, return nil."
  (when (featurep 'shorten)
    (shorten-strings
     (mapcar (lambda (frame)
               (buffer-name (car (buffer-list frame))))
             exwm-workspace--list))))

(declare-function exwm-workspace--count "exwm-workspace")
(declare-function exwm-workspace--position "exwm-workspace" (frame))

(defcustom exlybar-workspaces-exwm-shorten-names t
  "Non-nil to display shortened buffer names (if the shorten library is
available) or nil to display workspace numbers."
  :type 'boolean
  :group 'exlybar-workspaces)

(defcustom exlybar-workspaces-exwm-add-change-functions nil
  "Non-nil to ensure immediate status update when changing the window
selection or window buffer or nil to update lazily."
  :type 'boolean
  :group 'exlybar-workspaces)

(defun exlybar-workspaces--watch-exwm-add-change-functions (_ new oper where)
  "Watch variable `exlybar-workspaces-exwm-add-change-functions' and if
ever OPER is equal to \\='set and the NEW value is not a buffer local
value indicated by WHERE, call
`exlybar-workspaces--exwm-modify-change-functions' with the value of
NEW."
  (when (and (not where) (eq 'set oper))
    (exlybar-workspaces--exwm-modify-change-functions new)))

(add-variable-watcher 'exlybar-workspaces-exwm-add-change-functions
                      'exlybar-workspaces--watch-exwm-add-change-functions)

(defvar exwm--id-buffer-alist)
(defvar exwm--frame)
(defvar exwm-workspace--current)

(defun exlybar-workspaces-generate-list-fn-exwm ()
  "Implement `exlybar-workspaces-generate-list-fn' for EXWM.

If the shorten library is available (from circe), the displayed names
are shortened buffer names. Otherwise, names are the workspace numbers."
  (let ((not-empty (make-vector (exwm-workspace--count) nil))
        (shorts (exlybar-workspaces--shorten-frame-buffer-names-exwm)))
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
             `(,(or (and exlybar-workspaces-exwm-shorten-names sn)
                    (number-to-string pos))
               (,@(when (eq exwm-workspace--current frame)
                    '(:current))
                ,@(when (aref not-empty pos)
                    '(:window))
                ,@(unless (aref not-empty pos)
                    '(:blankish)))))))

(defun exlybar-workspaces--exwm-modify-change-functions (add?)
  "If ADD? is non-nil, add window buffer and window selection change
functions to update module status on changes, otherwise remove."
  (if add?
      (progn (add-hook 'window-selection-change-functions
                       'exlybar-workspaces--refresh-hook-fn)
             (add-hook 'window-buffer-change-functions
                       'exlybar-workspaces--refresh-hook-fn))
    (remove-hook 'window-selection-change-functions
                 'exlybar-workspaces--refresh-hook-fn)
    (remove-hook 'window-buffer-change-functions
                 'exlybar-workspaces--refresh-hook-fn)))

(defvar exwm--connection)

(defun exlybar-workspaces-setup-defaults-exwm ()
  "Configure exlybar-workspaces to display EXWM workspaces."
  (when (and (boundp 'exwm--connection) exwm--connection)
    (when (and exlybar-workspaces-exwm-shorten-names
               (locate-library "shorten"))
      (require 'shorten))
    (setq exlybar-workspaces-generate-list-fn
          'exlybar-workspaces-generate-list-fn-exwm)
    (add-hook 'exwm-workspace-switch-hook
              'exlybar-workspaces--refresh-hook-fn)
    (exlybar-workspaces--exwm-modify-change-functions
     exlybar-workspaces-exwm-add-change-functions)))

(when (locate-library "exwm")
  (add-hook 'exlybar-before-init-hook 'exlybar-workspaces-setup-defaults-exwm))

;;; herbstluftwm

(defun exlybar-workspaces-generate-list-fn-herbstluftwm ()
  "Implement `exlybar-workspaces-generate-list-fn' for herbstluftwm."
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

(defun exlybar-workspaces--make-herbstluft-events-filter ()
  "Return a process filter for herbstclient -i that looks for tag changes."
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
                (exlybar-module-refresh-all-by-name "workspaces")))))))))

(defvar exlybar-workspaces--herbstluft-evt-listener-proc nil)
(defvar exlybar-workspaces--herbstluft-evt-listener-proc-stderr nil)

(defun exlybar-workspaces--start-herbstluft-event-listener ()
  "Start a process to listen for herbstluftwm events."
  (let* ((stdout (generate-new-buffer " *herbstluftwm events*"))
         (stderr (generate-new-buffer " *herbstluftwm errors*"))
         (process (make-process :name "herbstclient"
                      :command '("herbstclient" "-i")
                      :buffer stdout
                      :filter (exlybar-workspaces--make-herbstluft-events-filter)
                      :stderr stderr))
         (stderr-process (get-buffer-process stderr)))
    (unless (and process stderr-process)
      (error "Process unexpectedly nil"))
    (setq exlybar-workspaces--herbstluft-evt-listener-proc
          process
          exlybar-workspaces--herbstluft-evt-listener-proc-stderr
          stderr-process)))

(defun exlybar-workspaces--stop-herbstluft-event-listener ()
  "Start a process to listen for herbstluftwm events."
  (when exlybar-workspaces--herbstluft-evt-listener-proc
    (kill-process exlybar-workspaces--herbstluft-evt-listener-proc))
  (when exlybar-workspaces--herbstluft-evt-listener-proc-stderr
    (kill-process exlybar-workspaces--herbstluft-evt-listener-proc-stderr)))

(defun exlybar-workspaces-setup-defaults-herbstluftwm ()
  "Configure exlybar-workspaces to display herbstluftwm tags."
  (when (and (executable-find "herbstclient")
             (equal "herbstluftwm" (getenv "DESKTOP_SESSION")))
    (setq exlybar-workspaces-generate-list-fn
          'exlybar-workspaces-generate-list-fn-herbstluftwm)
    (add-hook 'exlybar-before-init-hook 'exlybar-workspaces--start-herbstluft-event-listener)
    (add-hook 'exlybar-after-exit-hook 'exlybar-workspaces--stop-herbstluft-event-listener)))

(when (executable-find "herbstclient")
  (add-hook 'exlybar-before-init-hook 'exlybar-workspaces-setup-defaults-herbstluftwm))

(provide 'exlybar-workspaces)
;;; exlybar-workspaces.el ends here
