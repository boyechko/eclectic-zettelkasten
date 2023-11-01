;;; ezeka-breadcrumbs.el --- Create breadcrumb trails -*- lexical-binding: t -*-

;; Copyright (C) 2023 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2023-07-24
;; Version: 0.1
;; Package-Requires: ((emacs 28.2))
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

;; Automatically create breadcrumb trails when visiting Zettel notes.

;;; Code:

(require 'ezeka)
(require 'org)

(defvar ezeka-breadcrumb-trail-buffer nil
  "Buffer where to record the breadcrumb trail.")

(defvar ezeka-breadcrumb-trail-id nil
  "Org ID of the current breadcrumb head.")

(defcustom ezeka-leave-breadcrumb-trail t
  "When non-nil, leave a trail of visited Zettel notes."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumb-trail-headline "Breadcrumbs"
  "Exact name of the breadcrumb trail headline."
  :type 'string
  :group 'ezeka)

(defun ezeka--goto-breadcrumb-head ()
  "Go to the head of the current breadcrumb.
Return NIL if the breadcrumb head could not be found."
  (let ((id (org-id-find ezeka-breadcrumb-trail-id 'marker)))
    (when id
      (goto-char id)
      (move-marker id nil)
      id)))

(defun ezeka--find-breadcrumb-trail (target source)
  "Find the place in the current buffer where to drop breadcrumbs.
TARGET and SOURCE should be strings or symbols. Return
'primary or 'secondary if the trail was found (i.e. drop
breadcrumbs here), or nil if can't locate trail (i.e. don't
drop breadcrumbs)."
  (let ((target (cond ((null target) (error "Target is null"))
                      ((ezeka-file-p target) (ezeka-file-link target))
                      ((ezeka-link-p target) target)
                      ((symbolp target) (symbol-name target))
                      (t target)))
        (source (cond ((ezeka-file-p source) (ezeka-file-link source))
                      ((ezeka-link-p source) source)
                      ((symbolp source) (symbol-name source))
                      (t source))))
    (save-restriction
      (ezeka--goto-breadcrumb-head)
      (org-narrow-to-subtree)
      (let ((org-blank-before-new-entry '((heading . nil)))
            (head-level (org-current-level)))
        (cond ((and (stringp source)
                    (search-forward source nil t)
                    (eq 'headline (car (org-element-at-point))))
               ;; Breadcrumb for SOURCE found, so add one for TARGET
               (let ((src-level (car (org-heading-components)))
                     (src-head (point)))
                 (org-narrow-to-subtree)
                 (if (and (search-forward target nil t)
                          (= (1+ src-level)
                             (car (org-heading-components))))
                     (progn
                       (message "Breadcrumb for %s already exists under %s"
                                target
                                source)
                       nil)
                   (goto-char src-head)
                   (end-of-line)
                   (org-insert-heading-after-current)
                   (while (< (org-current-level) head-level)
                     (org-demote-subtree))
                   'secondary)))
              ((and (goto-char (point-min))
                    (search-forward target nil t))
               ;; Breadcrumbs already dropped for TARGET
               (message "Breadcrumbs already exist for %s" target)
               nil)
              (t
               (org-end-of-subtree)
               (org-insert-heading-after-current)
               (while (< (org-current-level) head-level)
                 (org-demote-subtree))
               'primary))))))

(defun ezeka--find-linear-trail (target source)
  "Find the place in the current buffer where to drop breadcrumbs.
TARGET and SOURCE are file names. Return either 'primary to
mark trail being found or nil if can't locate trail."
  (save-restriction
    (let ((org-blank-before-new-entry '((heading . nil))))
      ;; 1) Get positioned in the ezeka-breadcrumb-trail-headline subtree
      (ezeka--goto-breadcrumb-head)
      (org-narrow-to-subtree)
      ;; 2) Try to find an existing SOURCE headline
      (cond ((and (search-forward (ezeka--format-link target) nil t)
                  (eq 'headline (car (org-element-at-point))))
             (goto-char (org-element-property :begin (org-element-at-point)))
             (skip-chars-forward "* " (point-at-eol))
             (ezeka-update-link-prefix-title))
            ((and source
                  (search-forward (ezeka--format-link source) nil t)
                  (eq 'headline (car (org-element-at-point))))
             (org-narrow-to-subtree)
             (unless (search-forward (ezeka--format-link target) nil t)
               (end-of-line)
               (org-insert-heading-after-current)
               (org-demote-subtree)
               'secondary))
            (t
             (org-end-of-subtree)
             (org-insert-heading-after-current)
             (org-demote-subtree)
             'primary)))))

(defun ezeka--breadcrumb-string (target source)
  "Return a breadcrumb string for TARGET from SOURCE."
  (let* ((t-file (cond ((ezeka-file-p target) target)
                       ((ezeka-link-p target) (ezeka-link-file target))))
         (s-file (cond ((ezeka-file-p source) source)
                       ((ezeka-link-p source) (ezeka-link-file source))))
         (timestamp (format-time-string (cdr org-time-stamp-formats))))
    (concat (if t-file
                (or (alist-get :title (ezeka-file-metadata t-file 'noerror))
                    (ezeka-file-name-caption t-file))
              (format "%s" target))
            (when source
              (format " (%s)"
                      (if s-file
                          (ezeka-file-name-id s-file)
                        source))))))

;;;###autoload
(defun ezeka-breadcrumbs-drop (&optional target source)
  "Add the Zettel TARGET to `ezeka-breadcrumb-trail-buffer'.
TARGET should be a Zettel filename; SOURCE can be one too,
or a symbol describing where the function is being called
from."
  (interactive (list buffer-file-name 'interactive))
  (when ezeka-leave-breadcrumb-trail
    (let* ((problem nil)
           (t-file (cond ((ezeka-file-p target) target)
                         ((ezeka-link-p target) (ezeka-link-file target))
                         ((and (null target) (ezeka-file-p buffer-file-name))
                          buffer-file-name)
                         (t
                          (setq problem "target is not a Zettel file"))))
           (s-file (cond ((ezeka-file-p source) source)
                         ((ezeka-link-p source) (ezeka-link-file source))
                         (t                     nil))))
      (when (and (not (buffer-live-p ezeka-breadcrumb-trail-buffer))
                 (y-or-n-p "There is no breadcrumb trail. Start one? "))
        (call-interactively #'ezeka-start-breadcrumb-trail))
      (cond ((or (null ezeka-breadcrumb-trail-id)
                 (not (buffer-live-p ezeka-breadcrumb-trail-buffer)))
             (setq problem "no active breadcrumb trail"))
            ((ezeka-same-file-p
              t-file (buffer-file-name ezeka-breadcrumb-trail-buffer))
             (setq problem "same Zettel"))
            ((ezeka-same-file-p
              s-file (buffer-file-name ezeka-breadcrumb-trail-buffer))
             ;; FIXME: There has to be a better way to do this
             (setq s-file nil)))
      (if problem
          (message "Could not drop breadcrumbs for `%s' (from %s): %s"
                   (ezeka-file-link t-file)
                   (if s-file (ezeka-file-link s-file) source)
                   problem)
        (with-current-buffer ezeka-breadcrumb-trail-buffer
          (save-excursion
            (when-let ((status (ezeka--find-breadcrumb-trail t-file s-file)))
              (insert (ezeka--breadcrumb-string
                       t-file
                       (when (and source (symbolp source)) source)))
              (message "Dropped breadcrumbs for `%s' as %s"
                       (ezeka-file-name-id t-file)
                       status))))))))

;;; TODO: Since this is needed to actually drop breadcrumbs, the breadcrumb
;;; dropping should perhaps be a minor mode?
(add-hook 'ezeka-find-file-functions #'ezeka-breadcrumbs-drop)

;;;###autoload
(defun ezeka-breadcrumbs-drop-elisp (source)
  "Drop breadcrumbs for the current Emacs function.
SOURCE should be a string or symbol."
  (interactive
   (list (buffer-file-name
          (get-buffer (read-buffer "How did you get here? " nil t)))))
  (unless (or (null ezeka-breadcrumb-trail-id)
              (not (buffer-live-p ezeka-breadcrumb-trail-buffer)))
    (let* ((defname (rb-kill-ring-save-def-name))
           (target (format "[[elisp:(find-function '%s)][%s]]"
                           defname defname)))
      (with-current-buffer ezeka-breadcrumb-trail-buffer
        (save-excursion
          (when-let ((status (ezeka--find-breadcrumb-trail target source)))
            (insert (ezeka--breadcrumb-string target source))
            (message "Dropped breadcrumbs for `%s' as %s"
                     (ezeka-file-name-id target)
                     status)))))))

;;;###autoload
(defun ezeka-start-breadcrumb-trail (file)
  "Start a new breadcrumb trail in FILE at the current heading."
  (interactive
   (list (let ((ezeka-leave-breadcrumb-trail nil))
           (if ezeka-mode
               buffer-file-name
             (ezeka-zk-select-file
              "tempus"
              "Select Zettel for breadcrumb trail: ")))))
  (setq ezeka-breadcrumb-trail-buffer (find-file-noselect file)
        ezeka-breadcrumb-trail-id     nil)
  (set-buffer ezeka-breadcrumb-trail-buffer)
  (if (not (org-at-heading-p))
      (user-error "Move to desired heading first")
    (setq ezeka-breadcrumb-trail-id (org-id-get nil 'create))
    (message "Breadcrumbs will be dropped under heading `%s'"
             (nth 4 (org-heading-components)))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
