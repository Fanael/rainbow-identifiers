;;; rainbow-identifiers.el --- Highlight identifiers according to their names -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/rainbow-identifiers
;; Version: 0.1.2
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
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
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

(defgroup rainbow-identifiers nil
  "Highlight identifiers according to their names."
  :prefix "rainbow-identifiers-"
  :group 'convenience)

(defcustom rainbow-identifiers-override-other-highlighting nil
  "If non-nil rainbow-identifiers will override other highlighting.

This can, and almost certainly will, break all syntax highlighting
provided by the current major mode and other minor modes."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'rainbow-identifiers)

(defgroup rainbow-identifiers-faces nil
  "Faces for highlighting identifiers."
  :group 'rainbow-identifiers
  :group 'faces)

(defface rainbow-identifiers-identifier-1
  '((((class color) (background dark)) :foreground "#9999bb")
    (((class color) (background light)) :foreground "#78683f"))
  "Identifier face #1"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-2
  '((((class color) (background dark)) :foreground "#bb99b4")
    (((class color) (background light)) :foreground "#43783f"))
  "Identifier face #2"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-3
  '((((class color) (background dark)) :foreground "#bba699")
    (((class color) (background light)) :foreground "#3f7178"))
  "Identifier face #3"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-4
  '((((class color) (background dark)) :foreground "#a6bb99")
    (((class color) (background light)) :foreground "#513f78"))
  "Identifier face #4"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-5
  '((((class color) (background dark)) :foreground "#99bbb4")
    (((class color) (background light)) :foreground "#783f5a"))
  "Identifier face #5"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-6
  '((((class color) (background dark)) :foreground "#e0d0a0")
    (((class color) (background light)) :foreground "#707e4f"))
  "Identifier face #6"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-7
  '((((class color) (background dark)) :foreground "#a3e0a0")
    (((class color) (background light)) :foreground "#4f7e67"))
  "Identifier face #7"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-8
  '((((class color) (background dark)) :foreground "#a0d6e0")
    (((class color) (background light)) :foreground "#4f5c7e"))
  "Identifier face #8"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-9
  '((((class color) (background dark)) :foreground "#b6a0e0")
    (((class color) (background light)) :foreground "#7a4f7e"))
  "Identifier face #9"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-10
  '((((class color) (background dark)) :foreground "#e0a0bc")
    (((class color) (background light)) :foreground "#7e544f"))
  "Identifier face #10"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-11
  '((((class color) (background dark)) :foreground "#a7c0b9")
    (((class color) (background light)) :foreground "#783778"))
  "Identifier face #11"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-12
  '((((class color) (background dark)) :foreground "#a7aac0")
    (((class color) (background light)) :foreground "#784437"))
  "Identifier face #12"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-13
  '((((class color) (background dark)) :foreground "#c0a7bd")
    (((class color) (background light)) :foreground "#5e7837"))
  "Identifier face #13"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-14
  '((((class color) (background dark)) :foreground "#c0afa7")
    (((class color) (background light)) :foreground "#37785e"))
  "Identifier face #14"
  :group 'rainbow-identifiers-faces)

(defface rainbow-identifiers-identifier-15
  '((((class color) (background dark)) :foreground "#b3c0a7")
    (((class color) (background light)) :foreground "#374478"))
  "Identifier face #15"
  :group 'rainbow-identifiers-faces)

(defcustom rainbow-identifiers-face-count 15
  "Number of faces used for highlighting identifiers.

You can increase this value if you define enough faces named
rainbow-identifiers-identifier-<number>."
  :type 'integer
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

(defun rainbow-identifiers--determine-face (identifier)
  "Return the face IDENTIFIER should be fontified with."
  (intern-soft
   (concat "rainbow-identifiers-identifier-"
           (number-to-string
            (1+
             (mod (rainbow-identifiers--hash-function identifier)
                  rainbow-identifiers-face-count))))))

(defvar rainbow-identifiers--face nil)

(defun rainbow-identifiers--matcher (end)
  "The matcher function to be used by font lock mode."
  (catch 'rainbow-identifiers--matcher
    (while (re-search-forward (rx symbol-start (*? any) symbol-end) end t)
      (let ((identifier (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
        (setq rainbow-identifiers--face (rainbow-identifiers--determine-face identifier))
        (throw 'rainbow-identifiers--matcher t)))
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
  (let ((keywords '((rainbow-identifiers--matcher . rainbow-identifiers--face))))
    (font-lock-remove-keywords nil keywords)
    (when rainbow-identifiers-mode
      (font-lock-add-keywords nil keywords
                              (if rainbow-identifiers-override-other-highlighting
                                  nil
                                'append))))
  ;; Refresh font locking.
  (when font-lock-mode
    (font-lock-mode)))

(provide 'rainbow-identifiers)
;;; rainbow-identifiers.el ends here
