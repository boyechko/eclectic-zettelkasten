;;; ezeka-zk-hacks.el --- Ezk & Zk Integration Hacks -*- lexical-binding: t -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ezeka "0.8") (zk "0.4") (zk-index "0.4"))
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

;; Hacks (advice, defalises, etc.) for Ezk & Zk integration

(require 'ezeka)
(require 'zk)
(require 'zk-index)
(require 'ezeka-zk)

;;; Code:

(defun adv--zk-group-function (file transform)
  "Replace `zk--group-function' to better TRANSFORM the given FILE.
See `zk--group-function' for details."
  (let ((case-fold-search t)
        (base (file-name-base file)))
    (cond ((not transform)
           "ezk")
          ((string-match (zk-file-name-regexp) file)
           (let ((id (match-string 1 file))
                 (title (match-string 2 file)))
             (ezeka-zk-format-function "%i {%l} %c" id title)))
          (t
           base))))

(defun ezeka-zk--file-id (file)
  "Replace `zk--file-id' for the given FILE."
  (when (ezeka-file-p file)
    (ezeka-file-name-id file)))

(defun ezeka-zk-backlinks ()
  "Select from list of all notes that link to the current note.
Unlike `zk-backlinks', search in the entire `ezeka-directory' as well
as list the note's children."
  (interactive)
  (let* ((id (zk--file-id buffer-file-name))
         (zk-directory ezeka-directory)
         (files (zk--grep-file-list
                 (concat "\\(parent: " id
                         "\\|" (regexp-quote (zk--format zk-link-format id))
                         "\\)"))))
    (if files
        (find-file (funcall zk-select-file-function "Backlinks: " files))
      (user-error "No backlinks found"))))

(defun ezeka-zk-file-name-regexp ()
  "Return the correct regexp matching Ezeka file names.
The regexp captures these groups:

Group 1 is the ezk ID.
Group 2 is the title."
  (concat "\\(?1:"
          (ezeka--id-regexp)
          "\\)"
          "\\(?2: .+\\)*"
          "\\."
          zk-file-extension
          "$"))

(defvar ezeka-zk-hacks--zfnr-func nil)
(defvar ezeka-zk-hacks--zb-func nil)
(defvar ezeka-zk-hacks-mode nil)

(define-minor-mode ezeka-zk-hacks-mode
  "More radical customization of `zk' than with just `ezeka-zk'."
  :global nil
  :init-value nil
  :group 'ezeka
  :lighter " EzzH"
  (cond (ezeka-zk-hacks-mode            ; enable the mode
         (advice-add 'zk--group-function :override 'adv--zk-group-function)
         (advice-add 'zk--file-id :override 'ezeka-zk--file-id)
         (setq ezeka-zk-hacks--zfnr-func (symbol-function 'zk-file-name-regexp)
               ezeka-zk-hacks--zb-func (symbol-function 'zk-backlinks)
               ezeka-zk-hacks-mode t)
         (defalias 'zk-file-name-regexp 'ezeka-zk-file-name-regexp)
         (defalias 'zk-backlinks 'ezeka-zk-backlinks))
        (t                              ; disable the mode
         (advice-remove 'zk--group-function 'adv--zk-group-function)
         (advice-remove 'zk--file-id 'ezeka-zk--file-id)
         (fset 'zk-file-name-regexp ezeka-zk-hacks--zfnr-func)
         (fset 'zk-backlinks ezeka-zk-hacks--zb-func)
         (setq ezeka-zk-hacks--zfnr-func nil
               ezeka-zk-hacks--zb-func nil
               ezeka-zk-hacks-mode nil))))

(define-globalized-minor-mode global-ezeka-zk-hacks-mode
  ezeka-zk-hacks-mode ezeka-zk-hacks-mode)

(provide 'ezeka-zk-hacks)
;;; ezeka-zk-hacks.el ends here
