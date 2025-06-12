;;; exlybar-randr.el --- Exlybar randr event listeners -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.27.5
;; Homepage: https://github.com/jollm/exlybar
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

;; Part of exlybar.

;; This module provides randr awareness for exlybar.

;;; Code:

(require 'xcb-randr)

(require 'exlybar)
(require 'exlybar-util)

(defgroup exlybar-randr nil
  "Exlybar RandR."
  :group 'exlybar-randr)

(defcustom exlybar-randr-screen-change-hook nil
  "Normal hook run when screen changes."
  :type 'hook
  :group 'exlybar-randr)

;;;###autoload
(define-minor-mode exlybar-randr-mode
  "Toggle exlybar randr support."
  :global t
  :group 'exlybar-randr
  (exlybar--global-minor-mode-body randr))

(defvar exlybar-randr--last-timestamp 0 "Used for debouncing events.")

(defvar exlybar-randr--prev-screen-change-seqnum nil
  "The most recent ScreenChangeNotify sequence number.")

(defvar exlybar-randr--compatibility-mode nil
  "Non-nil when the server does not support RandR 1.5 protocol.")

(defun exlybar-randr--on-ScreenChangeNotify (data _synthetic)
  "Handle `ScreenChangeNotify' event.

Run `exlybar-randr-screen-change-hook' (usually user scripts to
configure RandR)."
  (let ((evt (make-instance 'xcb:randr:ScreenChangeNotify)))
    (xcb:unmarshal evt data)
    (let ((seqnum (slot-value evt '~sequence)))
      (unless (equal seqnum exlybar-randr--prev-screen-change-seqnum)
        (setq exlybar-randr--prev-screen-change-seqnum seqnum)
        (run-hooks 'exlybar-randr-screen-change-hook)))))

(defun exlybar-randr--on-Notify (data _synthetic)
  "Handle `CrtcChangeNotify' and `OutputChangeNotify' events.

Refresh when any CRTC/output changes."
  (let ((evt (make-instance 'xcb:randr:NotifyData)))
    (xcb:unmarshal evt data)
    (let ((notify (or (slot-value evt 'cc)
                      (slot-value evt 'oc))))
      (when notify
        (with-slots (timestamp) notify
          (when (> timestamp exlybar-randr--last-timestamp)
            (run-at-time 0 nil #'exlybar-randr--refresh)
            (setq exlybar-randr--last-timestamp timestamp)))))))

(defun exlybar-randr--on-ConfigureNotify (data _synthetic)
  "Handle `ConfigureNotify' event.

Refresh when any RandR 1.5 monitor changes."
  (let ((evt (make-instance 'xcb:ConfigureNotify)))
    (xcb:unmarshal evt data)
    (with-slots (window) evt
      (when (eq window exlybar--window)
        (exlybar-randr--refresh)))))

(defun exlybar-randr--init ()
  "Initialize RandR extension and exlybar RandR module.

This is adapted from the EXWM RandR module."
  (when (= 0 (slot-value (xcb:get-extension-data exlybar--connection 'xcb:randr)
                         'present))
    (error "[exlybar] RandR extension is not supported by the server"))
  (with-slots (major-version minor-version)
      (xcb:+request-unchecked+reply exlybar--connection
          (make-instance 'xcb:randr:QueryVersion
                         :major-version 1 :minor-version 5))
    (cond ((and (= major-version 1) (= minor-version 5))
           (setq exlybar-randr--compatibility-mode nil))
          ((and (= major-version 1) (>= minor-version 2))
           (setq exlybar-randr--compatibility-mode t))
          (t
           (error "[exlybar] The server only support RandR version up to %d.%d"
                  major-version minor-version)))
    ;; Listen for `ScreenChangeNotify' to notify external tools to
    ;; configure RandR and `CrtcChangeNotify/OutputChangeNotify' to
    ;; refresh the workspace layout.
    (xcb:+event exlybar--connection 'xcb:randr:ScreenChangeNotify
                #'exlybar-randr--on-ScreenChangeNotify)
    (xcb:+event exlybar--connection 'xcb:randr:Notify
                #'exlybar-randr--on-Notify)
    (xcb:+event exlybar--connection 'xcb:ConfigureNotify
                #'exlybar-randr--on-ConfigureNotify)
    (xcb:+request exlybar--connection
        (make-instance 'xcb:randr:SelectInput
                       :window exlybar--window
                       :enable (logior
                                xcb:randr:NotifyMask:ScreenChange
                                xcb:randr:NotifyMask:CrtcChange
                                xcb:randr:NotifyMask:OutputChange)))
    (xcb:flush exlybar--connection)))

(defun exlybar-randr--exit ()
  "Perform cleanup."
  (setq exlybar-randr--last-timestamp 0
        exlybar-randr--prev-screen-change-seqnum nil
        exlybar-randr--compatibility-mode nil))

(defun exlybar-randr--refresh ()
  "Refresh exlybar according to the updated RandR info."
  (interactive)
  (let ((geom (exlybar--find-display-geometry exlybar-preferred-display)))
    (setq exlybar-offset-x (alist-get 'x-offset geom)
	  exlybar-offset-y (if exlybar-is-bottom
                               (- (+ (alist-get 'y-offset geom)
                                     (alist-get 'height geom))
                                  exlybar-height)
                             (alist-get 'y-offset geom))
	  exlybar-width (alist-get 'width geom)
          exlybar--geometry-changed? t)
    (run-at-time 0 nil #'exlybar-refresh-modules)))

(provide 'exlybar-randr)
;;; exlybar-randr.el ends here
