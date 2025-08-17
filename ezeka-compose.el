;;; ezeka-compose.el --- Interface for composing longer texts -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2022-08-01
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

;;

;;; Code:
(require 'ezeka-base)
(require 'ezeka-file)
(require 'ezeka-meta)

(defcustom ezeka-snippet-heading
  '("Content" "Snippet" "Write-Up")
  "The title of the snippet heading."
  :type '(or string list))

(defcustom ezeka-snippet-modified-property "MODIFIED"
  "Name of the snippet heading's last-modified property."
  :type 'string)

(defcustom ezeka-insert-snippet-summary nil
  "Non-nil means insert the snippet summary."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-insert-snippet-footnotes t
  "Non-nil means insert footnotes."
  :type 'boolean
  :group 'ezeka)

(defun ezeka--org-nth-link-on-line (&optional n)
  "Return the Nth link on the current line.
With a negative N, count from the end. If there is no such link,
return NIL."
  (let ((n (or n 1)))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (cond ((> n 0)
               (beginning-of-line)
               (dotimes (i n)
                 (org-next-link)))
              ((< n 0)
               (end-of-line)
               (dotimes (i (abs n))
                 (org-previous-link))))
        (when (ezeka-link-at-point-p)
          (ezeka-link-at-point))))))

(defun ezeka--org-move-after-drawers ()
  "Move point after the properties and logbook drawers, if any.
Return the resulting point."
  (when (org-at-heading-p)
    (forward-line))
  (when (looking-at org-property-drawer-re)
    (goto-char (match-end 0))           ; move right after :END:
    (unless (zerop (forward-line))
      (insert "\n")))
  (when (looking-at org-logbook-drawer-re)
    (goto-char (match-end 0))           ; move right after :END:
    (unless (zerop (forward-line))
      (insert "\n"))))

(defun ezeka--find-snippet-heading ()
  "Go to the first snippet heading in the current buffer.
Return the new position; otherwise, nil."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      ;; HARDCODED Match the entire org-mode heading line
      (when (re-search-forward (ezeka--snippet-heading-regexp))
        (setq pos (match-end 0))))
    (when pos
      (goto-char pos))))

(defun ezeka--snippet-heading-regexp ()
  "Return the regexp to match `ezeka-snippet-heading'."
  (concat "^\\*+ +"
          (if (stringp ezeka-snippet-heading)
              ezeka-snippet-heading
            (concat "\\("
                    (apply #'ezeka--concat-strings "\\|" ezeka-snippet-heading)
                    "\\)"))
          ".*$"))

(defun ezeka-set-modified-property (&optional time)
  "Add :MODIFIED: property to the current heading.
If given, set it to TIME, otherwise the latest recorded
modification date."
  (cond ((not (org-at-heading-p))
         (user-error "Move to the org heading first"))
        (t
         (if-let ((mdata (ezeka-file-metadata buffer-file-name)))
             (org-set-property ezeka-snippet-modified-property
                               (alist-get 'created mdata))))))

(defun ezeka--compose-extract-content (file summary org-id)
  "Extract snippet subtree from FILE.
If SUMMARY is non-nil, also extract summary. ORG-ID is the ID of
where the content is to be inserted."
  (let (content)
    (with-current-buffer (get-file-buffer file)
      ;; Include Summary section if present
      (when (and (or summary ezeka-insert-snippet-summary)
                 (org-find-exact-headline-in-buffer "Summary"))
        (goto-char (org-find-exact-headline-in-buffer "Summary"))
        (forward-line)
        (let ((summary-start (point)))
          (org-end-of-subtree)
          (push "\n#+begin_comment" content)
          (push (buffer-substring-no-properties summary-start (point))
                content)
          (push "\n#+end_comment\n" content)))
      (or (ezeka--find-snippet-heading)
          (signal 'ezeka-error (list "Can't find the Snippet or Content section")))
      (if (not org-id)
          (warn "No org-id added to file %s" file)
        (org-entry-add-to-multivalued-property (point)
                                               "USED_IN+"
                                               (format "id:%s" org-id)))
      (basic-save-buffer)
      (ezeka--org-move-after-drawers)
      (let ((content-start (point)))
        (org-end-of-subtree)
        (push (buffer-substring-no-properties content-start (point))
              content)))
    content))

;;; TODO:
;;; - if region is active, narrow to it rather than to subtree (allows # lines!)
;;; - don't copy subtrees marked with COMMENT
;;; - quickly scan through all the headings and see if any need updating?
(defun ezeka-insert-snippet-text (link &optional force summary)
  "Insert snippet text from the given LINK into the current buffer.
By default, only update the text if the modification time is
different. With \\[universal-argument] FORCE, forces update. With SUMMARY
\(or \\[universal-argument] \\[universal-argument]),
insert the summary before the content."
  (interactive
   (list (or (org-entry-get (point) "SNIP_SOURCE")
             (save-excursion
               (org-back-to-heading)
               (ezeka--org-nth-link-on-line -1))
             (user-error "Insert a link to the snippet source first"))
         (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (save-excursion
    (save-restriction
      (org-back-to-heading)
      (let* ((snip-file (ezeka-link-file link))
             ;; Get the metadata and most recent modification
             (snip-mdata (ezeka-file-metadata snip-file))
             (their-modified (or (alist-get 'modified snip-mdata)
                                 (alist-get 'created snip-mdata)))
             (our-modified
              (when-let ((snip-modified (org-entry-get (point) "SNIP_MODIFIED")))
                (ezeka--parse-time-string snip-modified)))
             (current? (time-equal-p their-modified our-modified))
             (our-tags (org-get-tags))
             (local? (cl-find "local" our-tags :test #'string=))
             (org-id (org-id-get-create)))
        (org-narrow-to-subtree)
        (when local?
          (user-error "There are local changes (or at least :local: tag)"))
        ;; Update the snippet title
        (when (and nil
                   (looking-at org-outline-regexp))
          (replace-regexp (regexp-quote (elt (org-heading-components) 4))
                          (ezeka-format-metadata "%t [[%i]]" snip-mdata)))
        (unless (string= link (org-entry-get (point) "SNIP_SOURCE"))
          (org-entry-put (point) "SNIP_SOURCE" link))
        ;; Remove CHANGED tag, if present
        (org-set-tags (cl-remove "CHANGED" (org-get-tags) :test #'string=))
        (if (and current? (not force))
            (message "Snippet is up to date; leaving alone")
          (org-entry-put (point)
                         "SNIP_MODIFIED"
                         (ezeka-timestamp their-modified 'full 'brackets))
          (when (or t (y-or-n-p "Update the text? "))
            ;; If current line is a comment, create a heading after it
            (when (org-at-comment-p)
              (org-insert-subheading nil))
            (ezeka--org-move-after-drawers)
            (let ((start (point))
                  (removed-elements '(comments 0 footnotes 0))
                  (snip-buf (find-file-noselect snip-file))
                  (content '()))
              (delete-region start (point-max))
              ;; Get the Summary and Snippet subtrees from snippet snip-file
              (setq content (ezeka--compose-extract-content snip-file summary org-id))

              ;; Insert the copied subtrees and remove extraneous stuff
              (apply #'insert (nreverse content))
              (goto-char start)
              (setq removed-elements
                (ezeka--clean-inserted-content start our-tags))

              (org-indent-region (point-min) (point-max))
              (goto-char (point-max))
              (insert "\n")
              (rb-collapse-blank-lines)
              (message "Removed %d comments and %d footnotes"
                       (plist-get removed-elements 'comments)
                       (plist-get removed-elements 'footnotes))
              t)))))))

(defun ezeka--clean-inserted-content (start our-tags)
  "Clean up inserted content in `ezeka-insert-snippet-text'.
START is buffer position where to start. OUR-TAGS are the
tags specifying what to include or not. Return a plist of
removed elements."
  (let ((comments-removed 0)
        (footnotes-removed 0))
    ;; Transform headings
    (while (re-search-forward "^[*]+ " nil t)
      (goto-char (match-beginning 0))
      (replace-match (concat "*" (match-string 0)))
      ;; Remove headings if desired
      (when (cl-find "noheadings" our-tags :test #'string=)
        (ezeka--org-move-after-drawers)
        (kill-region (match-beginning 0) (point))))
    ;; Remove <<tags>>
    (goto-char start)
    (while (re-search-forward "<<[^>]+>>\n*" nil t)
      (replace-match ""))
    ;; Remove Zettel links
    (goto-char start)
    (while (re-search-forward " ?\\[\\[[[:alnum:]~-]+]]" nil t)
      (replace-match ""))
    ;; Remove inline @@...@@ and <...> comments, but not {...}
    (goto-char start)
    (while (re-search-forward " ?\\(@@\\|<\\).+?\\(@@\\|>\\)\n*" nil t)
      (cl-incf comments-removed)
      (replace-match ""))
    ;; Remove footnotes if need be
    (unless ezeka-insert-snippet-footnotes
      (goto-char start)
      (while (re-search-forward "^\\[fn:.+?\\].*?$" nil t)
        (goto-char (match-beginning 0))
        (kill-paragraph 1)
        (cl-incf footnotes-removed)))
    (list 'comments comments-removed
          'footnotes footnotes-removed)))

(defun ezeka--update-inserted-snippet ()
  "Update the snippet in the current note wherever it is used."
  (let ((current (current-buffer)))
    (save-excursion
      (when-let ((label (string=
                         "ν" (ezeka-file-name-label (buffer-file-name current))))
                 (pos (or (org-find-exact-headline-in-buffer "Snippet")
                          (org-find-exact-headline-in-buffer "Content"))))
        (when (y-or-n-p "Did you modify the snippet text? ")
          (goto-char pos)
          (org-entry-put (point) "MODIFIED"
                         (ezeka-timestamp nil 'full 'brackets))
          (when-let* ((used-in (org-entry-get (point) "USED_IN+"))
                      (used-list
                       (split-string
                        (replace-regexp-in-string "\\(id:\\|\\[id:\\)" "" used-in)
                        nil t " \n"))
                      (not-tagged
                       (save-excursion
                         (cl-remove-if (lambda (org-id)
                                         (if (not (org-id-find org-id))
                                             (warn "Cannot find Org ID `%s' that uses snippet from `%s'"
                                                   org-id
                                                   (ezeka-file-name-id
                                                    (buffer-file-name current)))
                                           (org-id-goto org-id)
                                           (member "CHANGED" (org-get-tags))))
                                       used-list))))
            (when (y-or-n-p (format "%s\nAdd CHANGED tags in these files? "
                                    (mapconcat (lambda (id)
                                                 (ezeka-file-name-caption
                                                  (car (org-id-find id))))
                                               not-tagged
                                               "\n")))
              (dolist (org-id not-tagged)
                (org-id-goto org-id)
                (org-back-to-heading t)
                (org-set-tags (cl-union '("CHANGED") (org-get-tags) :test #'string=)))))))
      (switch-to-buffer current))))
(add-hook 'ezeka-modified-updated-hook #'ezeka--update-inserted-snippet)

(defun ezeka-find-inserted-snippet ()
  "Find source of snippet inserted with `ezeka-insert-snippet-text'.
The point should be within the org entry. If called from the heading
with :USED_IN: property, perform the reverse action."
  (interactive)
  (let ((line-at-point (thing-at-point 'line t)))
    (if-let ((used-in (org-entry-get (point) "USED_IN+")))
        (progn
          (org-id-goto (string-trim used-in "\\(id:\\|\\[id:\\)" "]"))
          (org-back-to-heading t))
      (if-let ((source-link (org-entry-get (point) "SNIP_SOURCE")))
          (ezeka-find-link source-link)
        (org-back-to-heading)
        (org-next-link)
        (org-open-at-point)
        (goto-char (point-min))))
    (when (search-forward (string-trim line-at-point) nil t)
      (goto-char (match-beginning 0)))))

(defun ezeka-transclude-snippet (link)
  "Insert `#+transclude' statement from LINK."
  (interactive
   ;; Assume the file is the last link on the current line
   (list (save-excursion
           (org-back-to-heading)
           (end-of-line)
           (org-previous-link)
           (when (ezeka-link-at-point-p)
             (ezeka-link-at-point)))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((file (ezeka-link-file link))
             (metadata (ezeka-file-metadata file)))
        (org-narrow-to-subtree)
        ;; Update the heading title
        (org-edit-headline
         (format "%s [[%s]]" (alist-get 'title metadata) link))
        ;; Delete existing text
        (org-back-to-heading t)
        (delete-region (point-at-bol 2) (org-end-of-subtree t))
        ;; Insert the transclusion line
        (insert (format "\n#+transclude: [[%s::begin]] :lines 2- :end \"end\""
                        (file-relative-name file)))))))

(defun ezeka-org-footnote-action-maybe-local (&optional arg)
  "Place footnotes locally in snippets, ignoring `org-footnote-section'.
This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets. Footnotes are placed locally if the current
heading matches `ezeka-snippet-heading' or if the command was called
with \\[universal-argument] ARG. With double \\[universal-argument],
offer additional options."
  (interactive "P")
  (let ((snippet? (string= ezeka-snippet-heading
                          (save-excursion
                            (org-back-to-heading-or-point-min t)
                            (and (org-context)
                                 (nth 4 (org-heading-components)))))))
    (if (or snippet? arg)
        (let ((org-footnote-section nil)
              (org-footnote-auto-label nil)
              (org-footnote-define-inline nil)
              (org-footnote-auto-adjust 'sort)
              (context (org-context)))
          (org-element-cache-reset)
          (when-let ((it (cadr
                          (split-string (ezeka-file-name-id buffer-file-name)
                                        "-"))))
            (kill-new it))
          (org-footnote-new))
      (org-footnote-action (equal arg '(16))))))

(provide 'ezeka-compose)
;;; ezeka-compose.el ends here
