;;; exlybar-volume.el --- An exlybar volume module  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.25.2
;; Package-Requires: ((cl-lib "0.5") (volume "20201002.1022") (emacs "27.1"))
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

;; This is an implementation of `exlybar-module' for volume status
;; information.

;; To use this module, add it to `exlybar-modules' with any desired layout
;; insructions.

;;; Code:

(require 'volume)
(require 'cl-lib)

(require 'exlybar-module)
(require 'exlybar-module-helpers)

(require 'f)

(defgroup exlybar-volume nil
  "An Exlybar volume module."
  :group 'exlybar)

(defcustom exlybar-volume-progress-increment 10
  "The percent step increment for the volume module progress bar."
  :type 'integer
  :group 'exlybar-volume)

(defcustom exlybar-volume-color-zones '(20 50 80)
  "Volume percentages indicating progress color changes.
See `exlybar-zone-color'"
  :type 'list
  :group 'exlybar-volume)

(cl-defstruct (exlybar-volume
               (:include exlybar-module (name "volume") (icon '((33 . ?) (67 . ?) (110 . ?)))
                         (format "^8^f2^[^f1%i^]%p")
                         (format-fn 'exlybar-volume-format-format))
               (:constructor exlybar-volume-create)
               (:copier nil))
  (channel nil :type 'string
           :documentation "Channel name corresponding to `volume-channels'. When nil, it will be
`volume-default-channel'."))

(defun exlybar-volume-current-progress (pct)
  "Build a progress bar corresponding to the current PCT."
  (exlybar-progress-bar
   pct exlybar-volume-progress-increment exlybar-volume-color-zones))

(defun exlybar-volume--format-fn-spec (pct)
  "Build the `format-spec' spec used by the format-fn given PCT."
  `((?p . ,(exlybar-volume-current-progress pct))))

(defun exlybar-volume-format-format (m)
  "This is the default format-fn that is applied to format."
  (let ((default-directory (f-full "~")))
    (format-spec (exlybar-module-format m)
		 (exlybar-volume--format-fn-spec (exlybar-volume--get-status m)) t)))

(defun exlybar-volume--get-status (m)
  "Return volume status for the given module M."
  (let* ((channel (exlybar-volume-channel m))
         (volume-amixer-current-channel (or channel (volume-default-channel)))
         (volume-osascript-current-channel channel))
    (volume-get)))

(defun exlybar-volume--format-spec (m pct)
  "Build the `format-spec' spec used to generate module text given PCT."
  `((?i . ,(string (exlybar-choose-icon pct (exlybar-module-icon m))))))

(cl-defmethod exlybar-module-update-status ((m exlybar-volume))
  "Query the volume backend and check whether to update M's text."
  (let* ((default-directory (f-full "~"))
	 (pct (exlybar-volume--get-status m))
         (status (exlybar-volume--format-spec m pct))
         (txt (number-to-string pct)))
    (unless (equal txt (exlybar-volume-text m))
      (setf (exlybar-module-format-spec m) status
            (exlybar-module-text m) txt
            (exlybar-module-needs-refresh? m) t))))

(cl-defmethod exlybar-module-init :before ((m exlybar-volume))
  "Set the M's icon and update the text."
  (exlybar-module-update-status m))

(defun exlybar-volume--refresh-advice (&rest _)
  "Refresh the module if the volume is adjusted in Emacs."
  (when (exlybar-enabled-p)
    (let ((ms (seq-filter (lambda (m)
                            (equal "volume"
                                   (when (exlybar-module-p m)
                                     (exlybar-module-name m))))
                          exlybar--modules)))
      (seq-do (lambda (m)
                (exlybar-module-update-status m)
                (exlybar-refresh-modules))
              ms))))

(advice-add 'volume-update :after 'exlybar-volume--refresh-advice)
(advice-add 'volume-set :after 'exlybar-volume--refresh-advice)

(provide 'exlybar-volume)
;;; exlybar-volume.el ends here
