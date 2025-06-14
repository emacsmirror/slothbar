;;; slothbar-font.el --- Organize font data for eventual rendering -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jo Gay <jo.gay@mailfence.com>

;; Author: Jo Gay <jo.gay@mailfence.com>
;; Version: 0.27.5
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

;; Support color codes in format strings like those in stumpwm.

;;; Code:

(require 'cl-lib)
(require 'fontsloth)

(require 'slothbar-log)

;;; Try to use safe fallback font lists where possible

(defcustom slothbar-font-candidates
  '(("Aporetic Sans"
     "IBM Plex Serif"
     "Deja Vu Serif"
     "Cantarell")
    ("Font Awesome")
    ("Aporetic Sans Mono"
     "IBM Plex Mono"
     "DejaVu Sans Mono:style=Book")
    ("all-the-icons")
    ("Symbols Nerd Font Mono"))
  "A list of lists of candidate fonts for color codes ^f0-^f9, where
elements of the outer list correspond to ^f0, ^f1, and so on.

Elements of the inner lists are ordered highest preference first. Each
candidate should be a font name as in `font-family-list'.

In deciding whether a font should be grouped with others or a separate
entry, consider whether the fonts have a similar purpose and cover
similar code-point ranges. For example, it may make sense to have
separate entries grouping variable pitch and monospace fonts. Some icon
fonts may make sense to group and others may not depending on their
code-point ranges."
  :type '(repeat (repeat string))
  :group 'slothbar)

(declare-function font-info "font.c" (name &optional frame))

(cl-defsubst slothbar-font--filename-search (font-name-list)
  "Given FONT-NAME-LIST, return a file path to the first font found, or nil
or none are found."
  (seq-some (lambda (v) (when v v))
	    (cl-mapcar (lambda (name)
			 (when-let ((fuck (font-info name)))
			   (elt fuck 12)))
		       font-name-list)))

(defun slothbar-font-map-candidates (&optional candidates)
  "Given a list of lists of font names CANDIDATES, generate a vector where
each slot value is a file path corresponding to the best match for each
font name list.

Typically each element in the result vector would correspond to a color
code ^f0-^f9.

By default, CANDIDATES is the value of `slothbar-font-candidates'."
  (cl-loop for f-list in (or candidates slothbar-font-candidates)
           for i = 0 then (1+ i)
           with font-map = (make-vector 10 nil)
           do
           (when-let (path (slothbar-font--filename-search f-list))
             (aset font-map i path))
           finally return font-map))

(defvar slothbar-font--color-code-map
  (make-vector 10 nil)
  "The font map corresponding to color codes ^f0-^f9.")

(defun slothbar-font--watch-color-code-map (_ nval oper where)
  "With OPER eq \\='set and nil WHERE, update
`slothbar-font--color-code-map' with NVAL candidates."
  (when (and (not where) (eq 'set oper))
    (setq slothbar-font--color-code-map (slothbar-font-map-candidates nval))))

(add-hook 'slothbar-before-init-hook
          (lambda ()
            (setq slothbar-font--color-code-map
                  (slothbar-font-map-candidates))
            (add-variable-watcher 'slothbar-font-candidates
                                  #'slothbar-font--watch-color-code-map)))

(add-hook 'slothbar-after-exit-hook
          (lambda ()
            (remove-variable-watcher 'slothbar-font-candidates
                                     #'slothbar-font--watch-color-code-map)))

(cl-defun slothbar-font--precompute-px-sizes (height &optional font-map)
  "Given a HEIGHT, compute pixel sizes for all fonts in the FONT-MAP.

With no FONT-MAP argument, the value of `slothbar-font--color-code-map' is used."
  (slothbar--log-trace* "precompute-px-size called %s %s" height font-map)
  (apply
   #'vector
   (cl-loop for font-path across (or font-map slothbar-font--color-code-map) collect
            (when font-path
	      (fontsloth-font-compute-px (fontsloth-load-font font-path) height)))))

(defvar slothbar-font-px-size nil
  "Precomputed font px size map.

There should be no need to recompute pixel sizes unless either the height or
the fonts change.")

(defvar slothbar-height)

(defun slothbar-font--watch-px-size (sym nval oper where)
  "With OPER eq \\='set and nil WHERE, update `slothbar-font-px-size'
appropriately for NVAL with SYM equal to slothbar-height or
slothbar-font--color-code-map."
  (when (and (not where) (eq 'set oper))
    (let ((height (cl-case sym
                    (slothbar-height (when (/= (symbol-value sym) nval) nval))
                    (slothbar-font--color-code-map nil)))
          (font-map (cl-case sym
                      (slothbar-height nil)
                      (slothbar-font--color-code-map
                       (unless (equal (symbol-value sym) nval)
                         nval)))))
      (when (or height font-map slothbar-height)
        (setq slothbar-font-px-size
              (slothbar-font--precompute-px-sizes
               (or height slothbar-height) (or font-map slothbar-font--color-code-map)))))))

(add-hook 'slothbar-before-init-hook
          (lambda ()
            (add-variable-watcher 'slothbar-font--color-code-map
                                  #'slothbar-font--watch-px-size)))

(add-hook 'slothbar-after-exit-hook
          (lambda ()
            (remove-variable-watcher 'slothbar-font--color-code-map
                                     #'slothbar-font--watch-px-size)))

(defcustom slothbar-font-px-delta
  [0.0
   0.0
   0.0
   6.0
   0.0
   0.0
   0.0
   0.0
   0.0
   0.0]
  "These deltas adjust computed px sizes.
This could be helpful for in the same display area swapping between two fonts
with different metrics."
  :type '(vector float float float float float float float float float float)
  :group 'slothbar)

(defun slothbar-font--compute-y-delta (px-delta)
  "Given a vector of PX-DELTA, compute corresponding Y-DELTA."
  (apply #'vector (cl-loop for pd across px-delta collect (/ pd 3))))

(defvar slothbar-font-y-delta
  (slothbar-font--compute-y-delta slothbar-font-px-delta)
  "These deltas to adjust font y offsets.
This is a companion to `slothbar-font-px-delta'.  Note that
changing this setting does not invalidate existing glyph position
caches.  This is automatically recomputed when
`slothbar-font-px-delta' changes.")

(defun slothbar-font--watch-px-delta (_ nval oper where)
  "With OPER eq \\='set and nil WHERE, update `slothbar-font-y-delta'
computed from NVAL."
  (when (and (not where) (eq 'set oper))
    (setq slothbar-font-y-delta (slothbar-font--compute-y-delta nval))))

(add-variable-watcher 'slothbar-font-px-delta #'slothbar-font--watch-px-delta)

(defsubst slothbar-font-find (font-index)
  "Find a font corresponding to color code ^f0-^f9 given FONT-INDEX.

See `slothbar-font-candidates' for information about how fonts are
configured."
  (aref slothbar-font--color-code-map font-index))

(provide 'slothbar-font)
;;; slothbar-font.el ends here
