;;; ezeka-virtual.el --- Support for virtual notes -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-07-19
;; Version: 0.1
;; Package-Requires: ((emacs 30.1))
;; Keywords: none
;; URL: https://github.com/boyechko/

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

;; Virtual notes are like placeholders and aliases that do not exist as
;; full-fledged notes (yet), but are useful for creating links.

;;; Code:

(defun ezeka-insert-symlinks-table (symlinks)
  "Insert the SYMLINKS as a CSV table."
  (interactive (list (ezeka-scan-current-symlinks
                      (read-string "Symlinks in which subdirectory? "))))
  (let (missing)
    (insert "virtual_note,target_note,last_modified,status" "\n")
    (dolist (symlink (sort symlinks))
      (let-alist symlink
        (when (eq .status 'missing)
          (push symlink missing))
        (insert (format "%s\"%s\",\"%s\",\"%s\",\"%s\"\n"
                        (if (eq .status 'exists)
                            ""
                          (format "[[%s]] --- " (ezeka-file-name-id .virtual-rubric)))
                        .virtual-rubric
                        .target-rubric
                        (format-time-string "%FT%R" .modified)
                        .status))))
    (message "%d symlinks total, %d missing"
             (length symlinks) (length missing))))

(defun ezeka-scan-current-symlinks (subdir)
  "Scan SUBDIR for all current symbolic links."
  (let ((symlinks '())
        (dir (expand-file-name subdir ezeka-directory)))
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively
                     dir
                     (format "^\\w.*\\.%s$" ezeka-file-extension)))
        (when (file-symlink-p file)
          (let* ((virtual-rubric (file-name-base file))
                 (target-path (expand-file-name
                               (file-symlink-p file)
                               (file-name-directory file)))
                 (target-rubric (file-name-base target-path))
                 (modified (file-attribute-modification-time (file-attributes file)))
                 (status (if (file-exists-p target-path)
                             'exists
                           'missing)))
            (push `((virtual-rubric . ,virtual-rubric)
                    (target-rubric . ,target-rubric)
                    (modified . ,modified)
                    (status . ,status))
                  symlinks)))))
    symlinks))

(provide 'ezeka-virtual)
;;; ezeka-virtual.el ends here
