;;; ezeka-ivy.el --- Eclectic Zettelkasten Ivy Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.13.4") (ezeka "0.8"))
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

;; Ivy integration for ezeka.el

(require 'ezeka)
(require 'ivy)

(defun ezeka-ivy-titles-reverse-alist (&optional sort-by)
  "Returns a reverse alist of choices consisting of cached Zettel titles and
their paths. For use with `ezeka-ivy-read-reverse-alist-action'. SORT-BY is
either 'MTIME [default] or 'TITLE."
  (let (titles-alist)
    (cond (deft-hash-titles
            (maphash (lambda (key val)
                       (push (cons val key) titles-alist))
                     deft-hash-titles)
            (cl-sort titles-alist
                     (if (equal sort-by 'title)
                         #'deft-file-title-lessp
                       #'deft-file-newer-p)
                     :key #'cdr))
          (t
           (error "No Deft titles cached")))))

(defun ezeka-ivy-read-reverse-alist-action (prompt choices func &optional require-match)
  "Uses `ivy-read' to select from list of CHOICES alist composed of value/key
pairs. Upon selection, call the given FUNC, a function accepting one
argument, on the key. Returns a cons cell consisting of the match from
`ivy-read' and the result of FUNC."
  (let (result)
    (ivy-read prompt
              choices
              :action (lambda (choice)
                        (setq result
                          (if (consp choice)
                              (cons (car choice) (funcall func (cdr choice)))
                            (cons choice nil))))
              :re-builder 'ivy--regex-ignore-order
              :require-match require-match)
    result))

(defun ezeka-ivy-metadata-reverse-alist (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `ezeka-ivy-read-reverse-alist-action' as collection.
Relies on Zettel metadata, so slower than `ezeka-ivy-titles-reverse-alist'."
  (let ((fmt (concat "%s%-12s %-10s %-53s %s")))
    (mapcar (lambda (file)
              (let ((metadata (ezeka-file-metadata file))
                    (buf (get-file-buffer file)))
                (cons (format fmt
                              (if (and buf (buffer-modified-p buf)) "*" " ")
                              (alist-get :slug metadata)
                              (alist-get :category metadata)
                              (cl-subseq (alist-get :title metadata) 0
                                         (min (length (alist-get :title metadata))
                                              53))
                              (or (alist-get :keywords metadata) ""))
                      file)))
            files)))

(defun ezeka-ivy-select-link (&optional prompt require-match)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. PROMPT is the prompt to pass to `ivy-read'; if
REQUIRE-MATCH is non-nil, do not allow entering link manually."
  (let ((choice
         (ezeka-ivy-read-reverse-alist-action (or prompt "Select link to: ")
                                              (ezeka-ivy-titles-reverse-alist)
                                              #'identity
                                              require-match)))
    (cond ((cdr choice)                 ; link selected from candidates
           (ezeka-file-link (cdr choice)))
          ((ezeka-link-p (car choice)) ; valid link typed in
           (car choice))
          (t
           (signal 'wrong-type-argument '("That is not a valid link"))))))

(defun ezeka-ivy-insert-link (arg)
  "Inserts a link to another Zettel being currently visited or to those in
the Deft cache. With prefix argument, offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in the list
of cached or visiting Zettel, just insert the link to what was selected. If
the cursor in already inside a link, replace it instead."
  (interactive "P")
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (ezeka-deft-parsed-title path) path))
                                (ezeka-visiting-buffer-list t))
                        (ezeka-ivy-titles-reverse-alist 'mtime)))))
    (if choices
        (let* ((choice (ezeka-ivy-read-reverse-alist-action
                        "Insert link to: " choices 'ezeka-file-link nil))
               (link (or (cdr choice)
                         ;; Create a new child if there is no match
                         (let ((new-child (ezeka-insert-new-child nil)))
                           (kill-new (car choice)) ; save the entered text
                           (save-excursion
                             (with-current-buffer
                                 (ezeka-link-file
                                  (ezeka-find-link new-child))
                               (ezeka-insert-metadata-template
                                nil (car choice))))
                           new-child))))
          (if (not (ezeka-link-at-point-p))
              (if arg
                  (funcall-interactively #'ezeka-insert-link-with-metadata link)
                (ezeka-insert-link-with-metadata link :title :before t))
            ;; When replacing, don't including anything
            (delete-region (match-beginning 0) (match-end 0))
            (insert (ezeka-org-format-link link))))
      (user-error "No Deft cache or visited Zettel"))))

;; TODO: Also relies on Deft
(defun ezeka-ivy-set-parent ()
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
              (ezeka-ivy-titles-reverse-alist)))
     (lambda (path)
       (setf (alist-get :parent metadata) (ezeka-file-link path))
       (ezeka-normalize-metadata buffer-file-name metadata)))))

;; TODO: Also relies on Deft
(defun ezeka-ivy-switch-to-buffer (arg)
  "Quickly switch to other open Zettel buffers. With prefix argument, do so
in another window."
  (interactive "P")
  (let ((choices
         (mapcar (lambda (path)
                   (if (not deft-hash-titles)
                       (error "Deft hash table is not initialized")
                     (when (null (deft-file-title path))
                       (deft-cache-file path))
                     (cons (format "%s%s"
                                   (if (buffer-modified-p (get-file-buffer path))
                                       "✒︎"
                                     "")
                                   (deft-file-title path))
                           path)))
                 (ezeka-visiting-buffer-list t))))
    (ezeka-ivy-read-reverse-alist-action
     (if choices "Visit live buffer: " "Visit cached: ")
     (or choices (ezeka-ivy-titles-reverse-alist))
     (if (not arg) 'find-file 'find-file-other-window))))

(provide 'ezeka-ivy)
