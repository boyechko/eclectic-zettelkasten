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

(defvar ezeka--breadcrumbs-trail nil
  "Overlay object pointing to the current breadcrumb trail.")

(defcustom ezeka-breadcrumbs-leave-trail t
  "When non-nil, leave a trail of visited Zettel notes."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumbs-trail-headline "Breadcrumbs"
  "Exact name of the breadcrumb trail headline."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-breadcrumbs-record-source nil
  "If non-nil, include the breadcrumb source in parentheses."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumbs-find-trail-function #'ezeka-breadcrumbs-find-linear-trail
  "Function called with TARGET and SROUCE to find the trail.
TARGET and SOURCE should be strings or symbols. Return
'primary or 'secondary if the trail was found (i.e. drop
breadcrumbs here), or nil if can't locate trail (i.e. don't
drop breadcrumbs).")

(defun ezeka-breadcrumbs-find-trail (target source &rest args)
  "Call `ezeka-breadcrumbs-find-trail-function' for TARGET from SOURCE.
If non-nil, ARGS are also passed along."
  (apply ezeka-breadcrumbs-find-trail-function target source args))

(defun ezeka--goto-breadcrumbs-trailhead ()
  "Go to the head of the current breadcrumb trail.
Return the position on success, otherwise NIL."
  (let ((pos (overlay-end ezeka--breadcrumbs-trail)))
    (when pos
      (goto-char pos))))

(defun ezeka-breadcrumbs-visit-trailhead (&optional buffer-only)
  "Visit the current breadcrumbs trailhead.
If BUFFER-ONLY is non-nil, just `pop-to-buffer' without
going to the exact location. Return the position on success,
otherwise NIL."
  (interactive "P")
  (when ezeka--breadcrumbs-trail
    (pop-to-buffer (overlay-buffer ezeka--breadcrumbs-trail))
    (unless buffer-only
      (goto-char (overlay-end ezeka--breadcrumbs-trail)))))

(defun ezeka--insert-heading-after-current (level)
  "Insert `org-mode' heading of LEVEL after the current one."
  (end-of-line)
  (newline)
  (insert (make-string level ?*) " "))

(defun ezeka--update-breadcrumbs-heading (target source)
  "Update the breadcrumb heading for TARGET (from SOURCE)."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\(?1:\\*+\\) \\(.*\\<nil\\>.*\\)$" (point-at-eol) 'noerror)
      (replace-match
       (format "%s %s"
               (match-string-no-properties 1)
               (ezeka--breadcrumbs-string target source))))))

(defun ezeka-breadcrumbs-find-arboreal-trail (target source &optional allow-duplicates)
  "Find where to drop breadcrumbs in an arboreal trail.
See `ezeka-breadcrumbs-find-trail-function' for details
about TARGET and SOURCE. If ALLOW-DUPLICATES is non-nil, add
breadcrumbs even there are already some there."
  (let ((target (cond ((null target) (error "Target is null"))
                      ((ezeka-file-p target) (ezeka-file-link target))
                      ((ezeka-link-p target) target)
                      ((symbolp target) (symbol-name target))
                      (t target)))
        (source (cond ((ezeka-file-p source) (ezeka-file-link source))
                      ((ezeka-link-p source) source)
                      ((symbolp source) (symbol-name source))
                      (t source))))
    (with-current-buffer (overlay-buffer ezeka--breadcrumbs-trail)
      (save-restriction
        (goto-char (overlay-end ezeka--breadcrumbs-trail))
        (org-narrow-to-subtree)
        (let ((org-blank-before-new-entry '((heading . nil)))
              (head-level (org-current-level)))
          (cond ((and (stringp source)
                      (search-forward source nil t)
                      (org-at-heading-p))
                 ;; Breadcrumb for SOURCE found, so add one for TARGET
                 (let ((src-level (car (org-heading-components)))
                       (src-head (point)))
                   (cond ((and (search-forward target nil t)
                               (org-at-heading-p)
                               (not allow-duplicates))
                          (ezeka--update-breadcrumbs-heading target source)
                          (message "Breadcrumb for %s already exists under %s"
                                   target
                                   source)
                          nil)
                         (t
                          (goto-char src-head)
                          (ezeka--insert-heading-after-current (1+ src-level))
                          'secondary))))
                ((and (goto-char (point-min))
                      (search-forward target nil 'noerror)
                      (org-at-heading-p)
                      (not allow-duplicates))
                 ;; Breadcrumbs already dropped for TARGET
                 (ezeka--update-breadcrumbs-heading target source)
                 (when (and nil
                            (y-or-n-p (format "Breadcrumbs already exist for %s. Visit it?" target)))
                   (point-marker)))
                (t
                 ;; No breadcrumbs dropped for TARGET
                 (org-end-of-subtree)
                 (ezeka--insert-heading-after-current (1+ head-level))
                 'primary)))))))

(defun ezeka-breadcrumbs-find-linear-trail (target source)
  "Find where to drop breadcrumbs on a linear trail.
See `ezeka-breadcrumbs-find-trail-function' for details about
TARGET and SOURCE."
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
      (ezeka--goto-breadcrumbs-trailhead)
      (org-narrow-to-subtree)
      (let ((org-blank-before-new-entry '((heading . nil)))
            (head-level (org-current-level)))
        (cond ((search-forward target nil t)
               ;; Breadcrumbs already dropped for TARGET
               (message "Breadcrumbs already exist for %s" target)
               nil)
              (t
               ;; No breadcrumbs dropped for TARGET
               (org-end-of-subtree)
               (ezeka--insert-heading-after-current (1+ head-level))
               'primary))))))

(defun ezeka--breadcrumbs-string (target source &optional comment)
  "Return a breadcrumb string for TARGET from SOURCE.
Optionally, add COMMENT after TARGET."
  (save-match-data
    (let* ((t-file (cond ((ezeka-file-p target) target)
                         ((ezeka-link-p target) (ezeka-link-file target))))
           (s-file (cond ((ezeka-file-p source) source)
                         ((ezeka-link-p source) (ezeka-link-file source))))
           (timestamp (format-time-string (cdr org-time-stamp-formats))))
      (ezeka--concat-strings " "
        (if t-file
            (or (ezeka-format-file-name "{%l} %c [[%i]]" t-file)
                (format "%s" (file-name-nondirectory target)))
          target)
        comment
        ;; TODO Implement `format-spec' for breadcrumb strings?
        ;; Perhaps having different entries for ezeka and non-ezeka targets?
        (when (and source ezeka-breadcrumbs-record-source)
          (format "(%s)"
                  (cond (s-file (ezeka-file-name-id s-file))
                        ((stringp source) (file-name-nondirectory source))
                        (t source))))))))

;;;###autoload
(defun ezeka-breadcrumbs-drop (&optional target source)
  "Add the Zettel TARGET to the current breadcrumbs trail.
TARGET should be a Zettel filename; SOURCE can be one too,
or a symbol describing where the function is being called
from."
  (interactive (list buffer-file-name 'interactive))
  (when ezeka-breadcrumbs-leave-trail
    (save-excursion
      (if-let ((trail ezeka--breadcrumbs-trail)
               (trail-buf (overlay-buffer trail)))
          (let* ((inhibit-read-only t)
                 (problem nil)
                 (t-file (cond ((ezeka-file-p target) target)
                               ((ezeka-link-p target) (ezeka-link-file target))
                               ((and (null target) (ezeka-file-p buffer-file-name))
                                buffer-file-name)
                               (t nil)))
                 (s-file (cond ((ezeka-file-p source) source)
                               ((ezeka-link-p source) (ezeka-link-file source))
                               (t nil))))
            (cond ((ezeka-same-file-p t-file (buffer-file-name trail-buf))
                   (setq problem "same Zettel"))
                  ((ezeka-same-file-p s-file (buffer-file-name trail-buf))
                   ;; FIXME: There has to be a better way to do this
                   (setq s-file nil)))
            (if problem
                (message "Could not drop breadcrumbs for `%s' (from %s): %s"
                         (if t-file (ezeka-file-link t-file) target)
                         (if s-file (ezeka-file-link s-file) source)
                         problem)
              (let ((status (ezeka-breadcrumbs-find-trail target source)))
                (pcase status
                  ((pred null)
                   nil)
                  ((pred symbolp)
                   (with-current-buffer (overlay-buffer ezeka--breadcrumbs-trail)
                     (insert (ezeka--breadcrumbs-string target source))
                     (message "Dropped breadcrumbs for `%s' as %s"
                              (ezeka-file-name-id t-file)
                              status)))
                  ((pred markerp)
                   (pop-to-buffer (overlay-buffer ezeka--breadcrumbs-trail))
                   (goto-char (marker-position status)))))))
        (setq ezeka--breadcrumbs-trail nil)
        (unless (y-or-n-p "There is no active breadcrumb trail. Continue anyway? ")
          (user-error "There is no active breadcrumb trail"))))))

;;; TODO: Since this is needed to actually drop breadcrumbs, the breadcrumb
;;; dropping should perhaps be a minor mode?
(add-hook 'ezeka-find-file-functions #'ezeka-breadcrumbs-drop)

(defvar ezeka--breadcrumbs-xref-format "Find Regexp: %s"
  "Format-like string for dropping bookmarks from *xref* buffer.
The control sequence %s is replaced with the xref search string.")

(defun ezeka--breadcrumbs-xref-advice (&rest _)
  "Before advice for `xref-goto-xref' to drop breadcrumbs from xref buffer."
  (when-let* ((xref (xref--item-at-point))
              (file (xref-file-location-file (xref-item-location xref)))
              (_ (ezeka-file-p file)))
    (ezeka-breadcrumbs-drop file
                            (format ezeka--breadcrumbs-xref-format
                                    (ezeka--breadcrumbs-xref-search-string)))))

(defun ezeka--breadcrumbs-xref-search-string ()
  "Return xref search string."
  (save-excursion
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward 'face 'xref-match t)))
      (downcase (buffer-substring-no-properties
                 (prop-match-beginning match) (prop-match-end match))))))

(defun ezeka--breadcrumbs-elisp-target ()
  "Return a breadcrumbs target for the current Emacs Lisp function."
  (when-let ((defname (rb-kill-ring-save-def-name)))
    (format "[[elisp:(find-function '%s)][%s]]" defname defname)))

(defun ezeka--breadcrumbs-buffer-target ()
  "Return a breadcrumbs target for the current buffer."
  (if buffer-file-name
      (format "[[file:%s::%s][%s]]"
              buffer-file-name
              (string-trim
               (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (file-name-nondirectory buffer-file-name))
    (format "\"%s\"" (buffer-name))))

;;;###autoload
(defun ezeka-breadcrumbs-drop-external (source &optional comment)
  "Drop breadcrumbs for the current external location.
SOURCE should be a string or symbol; COMMENT can be a short string."
  (interactive
   (list (buffer-file-name
          (get-buffer (read-buffer "How did you get here? " nil t)))))
  (unless (or (null ezeka--breadcrumbs-trail)
              (not (overlay-buffer ezeka--breadcrumbs-trail)))
    (let ((inhibit-read-only t)
          (target (or (ezeka--breadcrumbs-elisp-target)
                      (ezeka--breadcrumbs-buffer-target))))
      (with-current-buffer (overlay-buffer ezeka--breadcrumbs-trail)
        (save-excursion
          (when-let ((status (ezeka-breadcrumbs-find-trail target source)))
            (insert (ezeka--breadcrumbs-string target source comment))
            (message "Dropped breadcrumbs from `%s' as %s"
                     (file-name-nondirectory source)
                     status)))))))

(defun ezeka--find-function-drop-breadcrumbs (&rest _)
  "After advice for `find-function' to drop breadcrumbs."
  (ezeka-breadcrumbs-drop-external (buffer-name (current-buffer))))
;; (advice-add 'find-function :after 'ezeka--find-function-drop-breadcrumbs)
;; (advice-remove 'find-function 'ezeka--find-function-drop-breadcrumbs)

;;;###autoload
(defun ezeka-breadcrumbs-start-trail (file)
  "Start a new breadcrumb trail in FILE at the current heading."
  (interactive (list buffer-file-name))
  (save-excursion
    (with-current-buffer (find-file-noselect file)
     (let* ((trailhead ezeka-breadcrumbs-trail-headline)
            (existing (org-find-exact-headline-in-buffer trailhead)))
       (cond ((and (org-at-heading-p)
                   (or (string= (nth 5 (org-heading-components))
                                ezeka-breadcrumbs-trail-headline)
                       (y-or-n-p "Drop breadcrumbs here? ")))
              ;; Start trail on current heading
              )
             ((and existing
                   (y-or-n-p
                    (format "There is a `%s' headline in this buffer. Use that? "
                            trailhead)))
              (goto-char (marker-position existing)))
             ((and (not (org-at-heading-p))
                   (y-or-n-p (format "Insert `%s' heading here? " trailhead)))
              (ezeka--insert-heading-after-current (1+ (or (org-current-level) 0)))
              (insert ezeka-breadcrumbs-trail-headline))
             (t
              (user-error "Well, what do you want, then?!")))
       (ezeka-breadcrumbs-stop-trail)
       (setq ezeka--breadcrumbs-trail (make-overlay (point-at-eol) (point-at-eol)))
       (overlay-put ezeka--breadcrumbs-trail 'type 'ezeka--breadcrumbs-trail)
       (overlay-put ezeka--breadcrumbs-trail 'after-string " (🍞)")
       (add-hook 'kill-buffer-hook #'ezeka-breadcrumbs-stop-trail nil t)
       (setq-local buffer-save-without-query t)
       (message "Breadcrumbs will be dropped in `%s'" (file-name-base file))))))

;; TODO: Any way to mark the breadcrumb heading and buffer?
(defun ezeka-breadcrumbs-stop-trail ()
  "Stop dropping breadcrumbs on this trail."
  (interactive)
  (when (overlayp ezeka--breadcrumbs-trail)
    (message "Breadcrumbs will no longer be dropped in `%s'"
             (overlay-buffer ezeka--breadcrumbs-trail))
    (delete-overlay ezeka--breadcrumbs-trail)
    (setq-local buffer-save-without-query
                (default-value 'buffer-save-without-query)))
  (setq ezeka--breadcrumbs-trail nil))

(defun ezeka-breadcrumbs-trail-dispatch (arg)
  "Start, switch to, or reset breadcrumb trail based on ARG."
  (interactive "p")
  (pcase arg
    (1 (if (and (org-at-heading-p)
                (string= (nth 4 (org-heading-components))
                         ezeka-breadcrumbs-trail-headline))
           (call-interactively 'ezeka-breadcrumbs-start-trail)
         (or (ezeka-breadcrumbs-visit-trailhead)
             (call-interactively 'ezeka-breadcrumbs-start-trail))))
    (4 (ezeka-breadcrumbs-stop-trail))
    (_ (user-error "Not a valid option"))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
