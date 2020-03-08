;;; space-theming.el --- Easilly override theme faces -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: faces
;; URL: https://github.com/p3r7/space-theming
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;  -----------
;;
;; Easilly override theme faces by configuration.
;; This is a port of the 'theming' layer from Spacemacs to regular Emacs.
;;
;; For detailed instructions, please look at the README.md at https://github.com/p3r7/space-theming/blob/master/README.md

;;; Code:


;; VARS

(defvar space-theming-modifications '()
  "An alist of theme modifications. Each element should
be on the form (THEME . SPEC), where THEME is a symbol
representing a theme, and SPEC is an alist mapping faces
to face specs (see `defface').")

(defvar space-theming-headings-inherit-from-default '()
  "A list of themes where all headings should inherit
from the default face, or the symbol `all'.")

(defvar space-theming-headings-same-size '()
  "A list of themes where all headings should have the
same size, or the symbol `all'.")

(defvar space-theming-headings-bold '()
  "A list of themes where all headings should be bold,
or the symbol `all'.")

(defvar space-theming--header-faces
  '(font-latex-sectioning-0-face
    font-latex-sectioning-1-face
    font-latex-sectioning-2-face
    font-latex-sectioning-3-face
    font-latex-sectioning-4-face
    font-latex-sectioning-5-face
    font-latex-slide-title-face
    info-title-1
    info-title-2
    info-title-3
    info-title-4
    markdown-header-face
    markdown-header-face-1
    markdown-header-face-2
    markdown-header-face-3
    markdown-header-face-4
    markdown-header-face-5
    markdown-header-face-6
    org-document-title
    org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8)
  "List of header faces.")



;; THEME TRACKING

(defvar space-theming--current-theme nil
  "Currently loaded theme.")

(defun space-theming--track-theme (theme &optional no-confirm)
  "Track the current theme."
  (setq space-theming--current-theme theme))

(advice-add 'load-theme :after #'space-theming--track-theme)



;; MAIN ADVICE

(defun space-theming--theming (theme &optional no-confirm)
  "Removes existing user theming and applies customizations for the given
theme."
  ;; Headings
  (let ((mods nil))
    (when (space-theming--in-or-all theme space-theming-headings-inherit-from-default)
      (setq mods (plist-put mods :inherit 'default)))
    (when (space-theming--in-or-all theme space-theming-headings-same-size)
      (setq mods (plist-put mods :height 1.0)))
    (when (space-theming--in-or-all theme space-theming-headings-bold)
      (setq mods (plist-put mods :weight 'bold)))
    (when mods
      (dolist (face space-theming--header-faces)
        (custom-set-faces `(,face ((t ,mods))))))

    ;; Add new modifications
    (dolist (spec (append (cdr (assq theme space-theming-modifications))
                          (cdr (assq t space-theming-modifications))))
      (custom-theme-set-faces theme `(,(car spec) ((t ,(cdr spec))))))))



;; COMMANDS

(defun space-theming-update-current-theme ()
  "Update face overrides for current theme."
  (interactive)
  (unless space-theming--current-theme
    (error "Current theme is unknown (`space-theming--current-theme' not set)."))
  (space-theming--theming space-theming--current-theme))



;; INIT

(defun space-theming-init-theming ()
  "Function to drop in your init.el after loading `space-theming'."
  ;; Apply theme customizations after any call to load-theme
  (advice-add 'load-theme :after #'space-theming--theming)
  ;; Apply the initial customizations now, because load-theme has propably already been called
  (when space-theming--current-theme
    (space-theming-update-current-theme)))



;; PRIVATE HELPERS

(defun space-theming--in-or-all (key seq)
  "Returns t if KEY is found in SEQ or if SEQ equals 'all."
  (or (eq 'all seq) (memq key seq)))




(provide 'space-theming)

;;; space-theming.el ends here
