;;; zettel.el --- Eclectic Zettelkasten Citar Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (zettel "0.8") (citar "1.0"))
;; Keywords: deft zettelkasten org
;; URL: https://github.com/boyechko/eclectic-zettelkasten

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Citar integration for zettel.el

(require 'zettel)
(require 'citar)

(defvar zettel-bibliographic-note-directory (in-zettel-dir "reticulum")
  "Directory where `citar-open-notes' can find my notes about the source.")

(defun zettel-symlink-bibliographic-note (arg file citekey)
  "Creates a symbolic link from the given Zettel file to the specified
citekey. With prefix argument, replace the existing link."
  (interactive
   (list current-prefix-arg
         buffer-file-name
         (let ((citekey (alist-get :citekey
                          (zettel-file-metadata buffer-file-name))))
           (if (string-match "^@*\\([[:alnum:]-]+\\)" citekey)
               (match-string 1 citekey)
             (read-string "Citekey (without @): ")))))
  (let ((note-name (concat citekey "." zettel-file-extension)))
    (when (y-or-n-p (format "Create the symlink '%s' to this note? " note-name))
      (make-symbolic-link
       (file-relative-name file zettel-bibliographic-note-directory)
       (expand-file-name note-name zettel-bibliographic-note-directory)
       arg))))

(provide 'zettel-citar)
