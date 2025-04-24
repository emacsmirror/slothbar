;;; exlybar-module-requires.el --- Requiring this file requires all modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.24.0
;; Package-Requires: ((xelb "0.18") (emacs "27.1"))
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

;; Part of exlybar.

;; This module collects all requires for status bar modules.

;;; Code:

(require 'exlybar-backlight)
(require 'exlybar-battery)
(require 'exlybar-date)
(require 'exlybar-volume)
(require 'exlybar-tray)
(require 'exlybar-wifi)

(provide 'exlybar-module-requires)
;;; exlybar-module-requires.el ends here
