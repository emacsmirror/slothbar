;;; exlybar-battery.el --- An exlybar battery module  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jo Gay <jo.gay@mailfence.com>

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

;; This is an implementation of `exlybar-module' for battery status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'battery)
(require 'cl-lib)
(require 'f)
(require 'map)

(require 'exlybar-log)
(require 'exlybar-color)
(require 'exlybar-module)

(defgroup exlybar-battery nil
  "An Exlybar battery module."
  :group 'exlybar)

(defcustom exlybar-battery-icons
  '((10 . ?) (35 . ?) (60 . ?) (85 . ?) (101 . ?))
  "Icons for exlybar-battery discharge thresholds.
See `exlybar-color-choose-icon' for how it is used."
  :type 'alist
  :group 'exlybar-battery)

(defcustom exlybar-battery-charge-icon ?
  "Icon for when the battery is charging."
  :type 'character
  :group 'exlybar-battery)

(defcustom exlybar-battery-charge-color-command "^1~"
  "Icon and percentage color command for when the battery is charging."
  :type 'string
  :group 'exlybar-battery)

(defcustom exlybar-battery-color-zones '(49 29 10 t t)
  "Battery percentages indicating icon color changes.
See `exlybar-color-zone'"
  :type 'list
  :group 'exlybar-battery)

(cl-defstruct (exlybar-battery
               (:include exlybar-module (name "battery")
                         (format
                          "^6^[^f1%i^] %b%p%% ^[^2|^] %t ^[^2|^] %r"
                          :documentation
                          "%i is for the battery icon.
For the rest, see `battery-status-function'. Note that for %r,
some versions of battery-status-function include the trailing W
and some do not. `battery-linux-sysfs' where available appears
more precise than e.g. `battery-upower'.")
                         (format-fn
                          'exlybar-battery-format-format
                          :documentation
                          "Pre-format %i in format to use zone colors.
The color is decided based on battery percentage. See `exlybar-color-zone'."))
               (:constructor exlybar-battery-create)
               (:copier nil)))

(defun exlybar-battery--format-fn-spec (zone-color charging?)
  "Build the `format-spec' spec used by the format-fn."
  (let ((icon-fmt (if charging? "^[^f3%s%%i^]" "%s%%i")))
    `((?i . ,(format icon-fmt zone-color))
      (?p . ,(format "%s%%p" zone-color)))))

(defun exlybar-battery-format-format (m)
  "This is the default format-fn that is applied to format."
  (let* ((default-directory (f-full "~")) ; ensure status checks don't remote
         (status (or (map-elt (exlybar-module-cache m) 'status)
                     (funcall battery-status-function)))
         (pct (if-let ((pct (map-elt status ?p))) (string-to-number pct) 100))
         (charging? (equal "+" (map-elt status ?b)))
         (zone-color (if charging? exlybar-battery-charge-color-command
                       (apply #'exlybar-color-zone
                              pct exlybar-battery-color-zones))))
    (format-spec (exlybar-module-format m)
                 (exlybar-battery--format-fn-spec zone-color charging?) t)))

(defun exlybar-battery--format-spec (status)
  "Build the `format-spec' spec used to generate module text."
  (let* ((pct (if-let ((pct (map-elt status ?p))) (string-to-number pct) 100))
         (charging? (equal "+" (map-elt status ?b)))
         (icon (if charging? exlybar-battery-charge-icon
                 (exlybar-color-choose-icon pct exlybar-battery-icons))))
    (map-insert status ?i (string icon))))

(cl-defmethod exlybar-module-update-status ((m exlybar-battery))
  "Poll the battery status and check whether to update M's text."
  (condition-case err
      (let* ((default-directory (f-full "~")) ; ensure status checks don't remote
             (status (funcall battery-status-function))
             (txt (format-spec (exlybar-module-format m) status t)))
        (if status
            (progn
              (when (exlybar-module-cache m)
                (map-put! (exlybar-module-cache m) 'status status))
              (unless (equal txt (exlybar-module-text m))
                (setf (exlybar-module-format-spec m)
                      (exlybar-battery--format-spec status)
                      (exlybar-module-text m) txt
                      (exlybar-module-needs-refresh? m) t)))
          (exlybar--log-info "exlybar-battery: nil status from %s"
                             battery-status-function)))
    (t
     (message "error in battery update %s" (error-message-string err)))))

(cl-defmethod exlybar-module-init :after ((m exlybar-battery))
  "Set M's icon and update the text."
  (exlybar-module-update-status m))

(provide 'exlybar-battery)
;;; exlybar-battery.el ends here
