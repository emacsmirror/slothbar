;;; exlybar-module-types.el --- base type for modules  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.22.4
;; Package-Requires: ((cl-lib "0.5") (emacs "27.1"))
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

;; exlybar-module-types.el:
;; Provides a base type for exlybar modules.

;;; Code:

(require 'cl-lib)

(require 'exlybar-common)

(defvar exlybar-module-min-width 10
  "Let modules have a min width so an empty module is visible.")

;; TODO: allow changing backgrounds colors using color codes
(cl-defstruct
    (exlybar-module-rgb
     (:constructor exlybar-module-rgb-create)
     (:copier nil))
  "Container for a background and a foreground color."
  (background-color (exlybar--color->pixel
                     (exlybar--find-background-color)) :type 'number)
  (foreground-color (exlybar--color->pixel
                     (exlybar--find-foreground-color)) :type 'number))

(cl-defstruct
    (exlybar-module
     (:constructor exlybar-module-create)
     (:copier nil))
  "This is a base type for exlybar modules."
  (name nil :type 'string)
  (text nil :type 'string)
  (format "" :type 'string)
  (format-fn nil :type 'symbol)
  (format-spec nil :type 'string)
  (text-layout nil :type 'list)
  (lpad 14 :type 'fixed)
  (rpad 14 :type 'fixed)
  (colors (exlybar-module-rgb-create) :type 'exlybar-module-rgb)
  (icon nil :type 'character)
  (animation nil :type 'function)
  (cache nil :type 'alist)
  (width exlybar-module-min-width :type 'fixed)
  (xcb nil :type 'list)
  (needs-refresh? nil :type 'boolean))

;;; Constructors for modules under ./modules
;;; This is to generate autoloads

;;;###autoload (autoload 'exlybar-backlight-create "exlybar-module-types")
(cl-defstruct (exlybar-backlight
               (:include exlybar-module (name "backlight") (icon ?)
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn 'exlybar-backlight-format-format))
               (:constructor exlybar-backlight-create)
               (:copier nil)))

;;;###autoload (autoload 'exlybar-battery-create "exlybar-module-types")
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
The color is decided based on battery percentage. See `exlybar-zone-color'."))
               (:constructor exlybar-battery-create)
               (:copier nil)))

;;;###autoload (autoload 'exlybar-date-create "exlybar-module-types")
(cl-defstruct (exlybar-date
               (:include exlybar-module (name "date") (icon ?)
                         (format (concat "^2^[^f1%i^] ^["
                                         exlybar-date-color-winter
                                         "%ζ%a, %h %e.^] %l:%M %#p %Z"))
                         (format-fn 'exlybar-date-format-format))
               (:constructor exlybar-date-create)
               (:copier nil)))

;;;###autoload (autoload 'exlybar-tray-create "exlybar-module-types")
(cl-defstruct (exlybar-tray
               (:include exlybar-module
                         (name "tray")
                         (format nil)
                         (lpad 4)
                         (rpad 4))
               (:constructor exlybar-tray-create)
               (:copier nil))
  "A system tray module.")

;;;###autoload (autoload 'exlybar-volume-create "exlybar-module-types")
(cl-defstruct (exlybar-volume
               (:include exlybar-module (name "volume") (icon ?)
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn 'exlybar-volume-format-format))
               (:constructor exlybar-volume-create)
               (:copier nil)))

;;; let's just try a simple display of link quality and ssid
;;;###autoload (autoload 'exlybar-wifi-create "exlybar-module-types")
(cl-defstruct (exlybar-wifi
               (:include exlybar-module (name "wifi") (icon ?)
                         (format "^6^[^f1%i^]^[^2|^]%e^[^2|^]%p")
                         (format-fn 'exlybar-wifi-format-format))
               (:constructor exlybar-wifi-create)
               (:copier nil)))

(provide 'exlybar-module-types)

;;; exlybar-module-types.el ends here
