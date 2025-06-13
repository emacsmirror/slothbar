;;; exlybar-font.el --- Organize font data for eventual rendering -*- lexical-binding: t -*-

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

;; Support color codes in format strings like those in stumpwm.

;;; Code:

(require 'cl-lib)
(require 'fontsloth)

(require 'exlybar-log)

;;; Try to use safe fallback font lists where possible

(defcustom exlybar-font-candidates
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
  :group 'exlybar)

(declare-function font-info "font.c" (name &optional frame))

(cl-defsubst exlybar-font--filename-search (font-name-list)
  "Given FONT-NAME-LIST, return a file path to the first font found,
  or nil or none are found."
  (seq-some #'(lambda (v) (when v v))
	    (cl-mapcar #'(lambda (name)
			   (when-let ((fuck (font-info name)))
			     (elt fuck 12)))
		       font-name-list)))

(defun exlybar-font-map-candidates (&optional candidates)
  "Given a list of lists of font names CANDIDATES, generate a vector where
each slot value is a file path corresponding to the best match for each
font name list.

Typically each element in the result vector would correspond to a color
code ^f0-^f9.

By default, CANDIDATES is the value of `exlybar-font-candidates'."
  (cl-loop for f-list in (or candidates exlybar-font-candidates)
           for i = 0 then (1+ i)
           with font-map = (make-vector 10 nil)
           do
           (when-let (path (exlybar-font--filename-search f-list))
             (aset font-map i path))
           finally return font-map))

(defvar exlybar-font--color-code-map
  (make-vector 10 nil)
  "The font map corresponding to color codes ^f0-^f9.")

(defun exlybar-font--watch-color-code-map (sym nval oper where)
  "Update `exlybar-font--color-code-map' when a relevant change occurs."
  (exlybar--log-trace* "watch-font-map called %s %s %s %s" sym nval oper where)
  (when (and (not where) (eq 'set oper))
    (setq exlybar-font--color-code-map (exlybar-font-map-candidates nval))))

(add-hook 'exlybar-before-init-hook
          (lambda ()
            (setq exlybar-font--color-code-map
                  (exlybar-font-map-candidates))
            (add-variable-watcher 'exlybar-font-candidates
                                  #'exlybar-font--watch-color-code-map)))

(add-hook 'exlybar-after-exit-hook
          (lambda ()
            (remove-variable-watcher 'exlybar-font-candidates
                                     #'exlybar-font--watch-color-code-map)))

(cl-defun exlybar-font--precompute-px-sizes (height &optional font-map)
  "Given a HEIGHT, compute pixel sizes for all fonts in the font map."
  (exlybar--log-trace* "precompute-px-size called %s %s" height font-map)
  (apply
   #'vector
   (cl-loop for font-path across (or font-map exlybar-font--color-code-map) collect
            (when font-path
	      (fontsloth-font-compute-px (fontsloth-load-font font-path) height)))))

(defvar exlybar-font-px-size nil
  "Precomputed font px size map.

There should be no need to recompute pixel sizes unless either the height or
the fonts change.")

(defun exlybar-font--watch-px-size (sym nval oper where)
  "Update `exlybar-font-px-size' when a relevant change occurs."
  (exlybar--log-trace* "watch-px-size called %s %s %s %s" sym nval oper where)
  (when (and (not where) (eq 'set oper))
    (let ((height (cl-case sym
                    (exlybar-height (when (/= (symbol-value sym) nval) nval))
                    (exlybar-font--color-code-map nil)))
          (font-map (cl-case sym
                      (exlybar-height nil)
                      (exlybar-font--color-code-map
                       (unless (equal (symbol-value sym) nval)
                         nval)))))
      (when (or height font-map exlybar-height)
        (setq exlybar-font-px-size
              (exlybar-font--precompute-px-sizes
               (or height exlybar-height) (or font-map exlybar-font--color-code-map)))))))

(add-hook 'exlybar-before-init-hook
          (lambda ()
            (add-variable-watcher 'exlybar-font--color-code-map
                                  #'exlybar-font--watch-px-size)))

(add-hook 'exlybar-after-exit-hook
          (lambda ()
            (remove-variable-watcher 'exlybar-font--color-code-map
                                     #'exlybar-font--watch-px-size)))

(defcustom exlybar-font-px-delta
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
  :group 'exlybar)

(defun exlybar-font--compute-y-delta (px-delta)
  "Given a vector of PX-DELTA, compute corresponding Y-DELTA."
  (apply #'vector (cl-loop for pd across px-delta collect (/ pd 3))))

(defvar exlybar-font-y-delta
  (exlybar-font--compute-y-delta exlybar-font-px-delta)
  "These deltas to adjust font y offsets.
This is a companion to `exlybar-font-px-delta'. Note that
changing this setting does not invalidate existing glyph position
caches. This is automatically recomputed when
`exlybar-font-px-delta' changes.")

(defun exlybar-font--watch-px-delta (sym nval oper where)
  "Update `exlybar-font-y-delta' when `exlybar-font-px-delta' is modified."
  (ignore sym)
  (when (and (not where) (eq 'set oper))
    (setq exlybar-font-y-delta (exlybar-font--compute-y-delta nval))))

(add-variable-watcher 'exlybar-font-px-delta #'exlybar-font--watch-px-delta)

(defsubst exlybar-font-find (font-index)
  "Find a font corresponding to color code ^f0-^f9 given FONT-INDEX.

See `exlybar-font-candidates' for information about how fonts are
configured."
  (aref exlybar-font--color-code-map font-index))

(provide 'exlybar-font)
;;; exlybar-font.el ends here
