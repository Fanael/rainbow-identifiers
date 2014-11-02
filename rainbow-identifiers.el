;;; rainbow-identifiers.el --- Highlight identifiers according to their names -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/rainbow-identifiers
;; Version: 0.2.2
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Minor mode providing highlighting of identifiers based on their
;; names. Each identifier gets a color based on a hash of its name.
;;
;; Use `rainbow-identifiers-mode' to enable/disable.
;;
;; Default colors try to be reasonable, but they can be changed by
;; changing the faces `rainbow-identifiers-identifier-<number>'.

;;; Code:
(require 'color)

(defgroup rainbow-identifiers nil
  "Highlight identifiers according to their names."
  :prefix "rainbow-identifiers-"
  :group 'convenience)

(defcustom rainbow-identifiers-choose-face-function
  'rainbow-identifiers-predefined-choose-face
  "The function used to choose faces used to highlight identifiers.
It should take a single integer, which is the hash of the identifier
currently being highlighting, and return a value suitable to use
as a value of the `face' text property."
  :type 'function
  :group 'rainbow-identifiers)

(defcustom rainbow-identifiers-filter-functions '(rainbow-identifiers-face-overridable)
  "Abnormal hook run to determine whether to rainbow-highlight an identifier.

Two arguments are passed to each function: the position of the beginning and end
of the identifier currently being considered.
Each function should return non-nil if and only if it considers the identifier
to be eligible to rainbow-highlighting.  Identifier is rainbow-highlighted only
when all hook functions consider it eligible."
  :type '(repeat function)
  :group 'rainbow-identifiers)

(defcustom rainbow-identifiers-faces-to-override nil
  "List of faces that `rainbow-identifiers' is allowed to override.

It has an effect only when `rainbow-identifiers-face-overridable' is in
`rainbow-identifiers-filter-functions'."
  :type '(repeat face)
  :group 'rainbow-identifiers)

(defconst rainbow-identifiers--hash-bytes-to-use
  (ceiling (/ (log most-positive-fixnum 2) 8.0))
  "Number of bytes of returned hash to actually use.")

(defun rainbow-identifiers--hash-function (identifier)
  "Hash function used to determine the face of IDENTIFIER."
  (let* ((hash (secure-hash 'sha1 identifier nil nil t))
         (len (length hash))
         (i (- len rainbow-identifiers--hash-bytes-to-use))
         (result 0))
    (while (< i len)
      (setq result (+ (* result 256) (aref hash i)))
      (setq i (1+ i)))
    result))

;; Predefined face chooser:

(defgroup rainbow-identifiers-faces nil
  "Faces for highlighting identifiers."
  :group 'rainbow-identifiers
  :group 'faces)

(eval-when-compile
  (defmacro rainbow-identifiers--define-faces ()
    (let ((faces '())
          (light-colors ["#78683f" "#43783f" "#3f7178" "#513f78" "#783f5a"
                         "#707e4f" "#4f7e67" "#4f5c7e" "#7a4f7e" "#7e544f"
                         "#783778" "#784437" "#5e7837" "#37785e" "#374478"])
          (dark-colors ["#9999bb" "#bb99b4" "#bba699" "#a6bb99" "#99bbb4"
                        "#e0d0a0" "#a3e0a0" "#a0d6e0" "#b6a0e0" "#e0a0bc"
                        "#a7c0b9" "#a7aac0" "#c0a7bd" "#c0afa7" "#b3c0a7"]))
      (dotimes (i 15)
        (push `(defface ,(intern (format "rainbow-identifiers-identifier-%d" (1+ i)))
                 '((((class color) (background dark)) :foreground ,(aref dark-colors i))
                   (((class color) (background light)) :foreground ,(aref light-colors i)))
                 ,(format "Identifier face #%d" (1+ i))
                 :group 'rainbow-identifiers-faces)
              faces))
      `(progn ,@faces))))
(rainbow-identifiers--define-faces)

(defcustom rainbow-identifiers-face-count 15
  "Number of faces used for highlighting identifiers.

You can increase this value if you define enough faces named
rainbow-identifiers-identifier-<number>."
  :type 'integer
  :group 'rainbow-identifiers)

(defun rainbow-identifiers-predefined-choose-face (hash)
  "Use HASH to choose one of the `rainbow-identifiers-identifier-N' faces."
  (intern-soft
   (concat "rainbow-identifiers-identifier-"
           (number-to-string (1+ (mod hash rainbow-identifiers-face-count))))))

;; CIE L*a*b* face chooser:

(defcustom rainbow-identifiers-cie-l*a*b*-lightness 50
  "The lightness of the generated colors.

Internally, this is the L* color coordinate."
  :type 'number
  :group 'rainbow-identifiers)

(defcustom rainbow-identifiers-cie-l*a*b*-saturation 15
  "The saturation of generated colors.

Internally, this is the radius of a circle where the X and Y
coordinates of a point on that circle are the a* and b* color
coordinates, respectively."
  :type 'number
  :group 'rainbow-identifiers)

(defcustom rainbow-identifiers-cie-l*a*b*-color-count 65536
  "The number of different colors to generate."
  :type 'integer
  :group 'rainbow-identifiers)

(defun rainbow-identifiers-cie-l*a*b*-choose-face (hash)
  "Use HASH to choose a face with a generated foreground color.

The colors are chosen from the CIE L*a*b* color space. If a color not
representable in sRGB is chosen, the components are clamped.

The color generation can be influenced by changing
`rainbow-identifiers-cie-l*a*b*-lightness',
`rainbow-identifiers-cie-l*a*b*-saturation' and
`rainbow-identifiers-cie-l*a*b*-color-count'."
  (let* ((bucket (float (mod hash rainbow-identifiers-cie-l*a*b*-color-count)))
         (angle (* 2 float-pi (/ bucket rainbow-identifiers-cie-l*a*b*-color-count)))
         (a (* rainbow-identifiers-cie-l*a*b*-saturation (cos angle)))
         (b (* rainbow-identifiers-cie-l*a*b*-saturation (sin angle))))
    (let ((color (color-lab-to-srgb rainbow-identifiers-cie-l*a*b*-lightness a b)))
      ;; Clamp the color if the result is not representable in sRGB.
      (let ((i color))
        (while i
          (setcar i (max 0.0 (min 1.0 (car i))))
          (setq i (cdr i))))
      (list (list :foreground (apply #'color-rgb-to-hex color))))))


;; Face filter:

(defun rainbow-identifiers-face-overridable (begin _end)
  "Test if the face of the identifier under BEGIN is overridable."
  (let ((face (get-text-property begin 'face)))
    (cond
     ((null face)
      t)
     ((listp face)
      (catch 'rainbow-identifiers--face-overridable
        (dolist (face* face)
          (unless (memq face* rainbow-identifiers-faces-to-override)
            (throw 'rainbow-identifiers--face-overridable nil)))
        t))
     (t
      (memq face rainbow-identifiers-faces-to-override)))))


(defvar rainbow-identifiers--face nil)

(defun rainbow-identifiers--matcher (end)
  "The matcher function to be used by font lock mode."
  (catch 'rainbow-identifiers--matcher
    (while (re-search-forward (rx symbol-start (*? any) symbol-end) end t)
      (let ((beginning (match-beginning 0))
            (end (match-end 0)))
        (when (run-hook-with-args-until-failure 'rainbow-identifiers-filter-functions beginning end)
          (let* ((identifier (buffer-substring-no-properties beginning end))
                 (hash (rainbow-identifiers--hash-function identifier)))
            (setq rainbow-identifiers--face (funcall rainbow-identifiers-choose-face-function hash))
            (throw 'rainbow-identifiers--matcher t)))))
    nil))

;;;###autoload
(define-minor-mode rainbow-identifiers-mode
  "Highlight identifiers according to their names.

Toggle Rainbow Identifiers mode on or off.

With a prefix argument ARG, enable Rainbow Identifiers mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (let ((keywords '((rainbow-identifiers--matcher 0 rainbow-identifiers--face prepend))))
    (font-lock-remove-keywords nil keywords)
    (when rainbow-identifiers-mode
      (font-lock-add-keywords nil keywords 'append)))
  ;; Refresh font locking.
  (when font-lock-mode
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'rainbow-identifiers)
;;; rainbow-identifiers.el ends here
