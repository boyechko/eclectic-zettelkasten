;;; ezeka-zk.el --- Eclectic Zettelkasten & Zk Integration -*- lexical-binding: t -*-

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

;; Zk and Zk-index integration for ezeka.el

(require 'ezeka)
(require 'zk)
(require 'zk-index)

;; For these avariables to be treated as dynamic in `ezeka-zk-index-choose-kasten',
;; need to declare them first here.
(defvar zk-directory)
(defvar zk-id-regexp)
(defvar zk-id-format)

(defmacro ezeka-zk-with-kasten (kasten &rest body)
  (declare (indent 1))
  `(let ((zk-directory (ezeka-kasten-directory ,kasten)))
     (cl-progv '(zk-id-regexp
                 zk-id-time-string-format
                 zk-directory-subdir-function)
         (cl-case (ezeka-kasten-id-type ,kasten)
           (:numerus
            '("[a-z]-[0-9]\\{4\\}"
              ,(concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                       "-%H%M")))
           (:bolus
            '("[0-9]\\{3\\}-[a-z]\\{3\\}"
              ,(concat (downcase (format-time-string "%a")) "-%j")))
           (t
            '("[0-9]\\{8\\}T[0-9]\\{4\\}"
              "%Y%m%dT%H%M")))
       ,@body)))

(defun ezeka-zk-initialize-kasten (kasten)
  "Set necessary variables for long-term work in KASTEN."
  (custom-set-variables
   `(zk-directory ,(ezeka-kasten-directory kasten))
   `(global-mode-string ,(propertize (concat "Kasten:" (upcase kasten))
                                     'face 'bold-italic)))
  (cl-case (ezeka-kasten-id-type kasten)
    (:numerus
     (setq zk-id-regexp "\\([a-z]-[0-9]\\{4\\}\\)"
           zk-id-time-string-format
           (concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                   "-%H%M")))
    (:bolus
     (setq zk-id-regexp "\\([0-9]\\{3\\}-[a-z]\\{3\\}\\)"
           zk-id-time-string-format
           (concat (downcase (format-time-string "%a-%j")))))
    (t
     (setq zk-id-regexp "\\([0-9T]\\{13\\}\\)"
           zk-id-time-string-format "%Y%m%dT%H%M"))))

;;;###autoload
(defun ezeka-zk-index-choose-kasten (arg new-kasten)
  "If there is an existing `zk-index-buffer-name', switches to it, otherwise
 interactively selects the deft directory from among `ezeka-kaesten'. With a
prefix argument, selects new Zk directory regardless of Zk-Index buffer
status."
  (interactive
   (if (or (null (get-buffer zk-index-buffer-name))
           (equal current-prefix-arg '(4)))
       (list current-prefix-arg
             (completing-read "Zettel kasten: "
                              (mapcar #'car ezeka-kaesten)))
     (list current-prefix-arg nil)))
  (if (not new-kasten)
      (pop-to-buffer zk-index-buffer-name)
    (if-let ((buffer (get-buffer zk-index-buffer-name)))
        (kill-buffer buffer))
    (custom-set-variables
     '(zk-directory-subdir-function #'ezeka-subdirectory)
     `(zk-index-mode-name ,(format "Zk:%s" (capitalize new-kasten)))
     `(zk-header-title-line-regexp
       "^\\(?9:title: §\\)*[^ ]+\\.* \\(?1:{\\(?2:[^ ]+\\)} \\(?3:.*\\)\\)$"
       "Regexp of the line in a zk file's header that contains the title.
Group 1 is the entire title.
Group 2 is the category.
Group 3 is the title without category."))
    (ezeka-zk-initialize-kasten new-kasten)
    (zk-index)
    (zk-index-refresh)))

(defun ezeka-zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (ezeka-insert-header-template new-id nil title orig-id))

(defun ezeka-zk-format-function (files)
  "See `zk-new-note-header-function'."
  (let* (output)
    (dolist (file files output)
      (when (ezeka-note-p file)
        (let* ((metadata (ezeka-file-metadata file)))
          (push (format-spec zk-index-format
                             `((?i . ,(ezeka-file-id file))
                               (?t . ,(alist-get :title metadata))
                               (?c . ,(alist-get :category metadata))
                               (?k . ,(or (alist-get :citekey metadata) ""))))
                output))))))

(defun ezeka-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((format-string
         (replace-regexp-in-string "%\\([^[:alpha:]]*\\)[[:alpha:]]"
                                   "%\\1s"
                                   zk-index-format)))
    (insert
     (format (concat format-string "\n\n")
             "ID" "Category" "Citekey" "Title"))))

(defun ezeka-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((kasten (upcase (f-base zk-directory))))
    (insert (concat (propertize kasten
                                'face 'warning
                                'justification 'center)))
    (insert "\n\n")))

(defun ezeka-zk-parse-file (target files)
  "See `zk-parse-file-function'."
  (let* ((files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
             (if (equal target 'id)
                 (ezeka-file-id file)
               (alist-get :title (ezeka-file-metadata file))))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))

(defvar ezeka-zk-metadata-alist nil
  "An alist containing file metadata and mtime, cached by ID. Each item
has the form
(ID TITLE FILENAME MTIME METADATA).")

(defun ezeka-zk-cache-update-all ()
  "Update file list and update cached information for each file. Returns
`ezeka-zk-metadata-alist'."
  (setq ezeka-zk-metadata-alist
    (mapcar
     (lambda (file)
       (when (ezeka-note-p file)
         (let ((metadata (ezeka-file-metadata file)))
           (list (ezeka-file-id file)
                 (alist-get :title metadata)
                 file
                 (file-attribute-modification-time (file-attributes file))
                 metadata))))
     (zk--directory-files t))))

(defun ezeka-zk-alist ()
  "See `zk-alist-function'."
  (or ezeka-zk-metadata-alist
      (ezeka-zk-cache-update-all)))

(defun ezeka-zk-zmove-all-in-desktop (kasten arg)
  "Move all files listed in the active region of deft-browser to KASTEN. With
prefix argument, confirm each move and ask about destination kasten."
  (interactive (list (completing-read "Which kasten to move to? " ezeka-kaesten)
                     current-prefix-arg))
  (let ((lines (count-lines (point-min) (point-max)))
        (moved 1)
        (zk-alist (zk--alist (zk--directory-files))))
    (goto-char (point-min))
    (while (re-search-forward zk-id-regexp nil t)
      (let* ((id (match-string-no-properties 1))
             (title (buffer-substring-no-properties
                     (point-at-bol) (match-beginning 0)))
             (file (zk--parse-id 'file-path id zk-alist)))
        (when (and file
                   (or arg
                       (y-or-n-p
                        (format "[%d/%d] Move %s [%s] to %s? "
                                moved lines id title kasten))))
          (ezeka-zmove-to-another-kasten file kasten nil t)
          (cl-incf moved))))))

;; TODO: Rewrite without ivy
(defun ezeka-zk-set-parent ()
  "Sets the parent metadata of the current Zettel to the Zettel chosen by the
user from cached and visiting Zettel."
  (interactive)
  (let ((metadata (ezeka-file-metadata buffer-file-name)))
    (ezeka-ivy-read-reverse-alist-action
     "Set parent to: "
     (delete-dups
      (append (mapcar (lambda (path)
                        (cons (deft-file-title path) path))
                      (ezeka-visiting-buffer-list t))
              (ezeka-completion-table __FILES__)))
     (lambda (path)
       (setf (alist-get :parent metadata) (ezeka-file-link path))
       (ezeka-normalize-header buffer-file-name metadata)))))

(defun ezeka-zk-insert-link-to-kasten (&optional kasten)
  "Call `zk-insert-link' after temporarily setting zk variables to be
appropriate for the particular Zettelkasten."
  (interactive (list (if current-prefix-arg
                         (completing-read "Kasten: " ezeka-kaesten)
                       "rumen")))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'zk-insert-link)))

(defun ezeka-zk-find-note-in-kasten (arg &optional kasten)
  "Call `zk-find-file' after temporarily setting zk variables to be appropriate
for the particular Zettelkasten. Defaults to the Kasten set in
`zk-directory', if any. With prefix arg, ask to select Kasten."
  (interactive (list current-prefix-arg
                     (if-let ((kasten
                               (and (not current-prefix-arg)
                                    (ezeka-directory-kasten zk-directory))))
                         kasten
                       (completing-read "Kasten: " ezeka-kaesten))))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'zk-find-file)))

(defun ezeka-rgrep-link-at-point (link)
  "Executes recursive grep for the ezeka link at point."
  (interactive
   (list (when (ezeka-link-at-point-p t)
           (ezeka-link-at-point))))
  (consult-grep ezeka-directory link))

(provide 'ezeka-zk)
