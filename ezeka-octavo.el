;;; ezeka-octavo.el --- Eclectic Zettelkasten & Octavo Integration -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.2
;; Keywords: zettelkasten org
;; URL: https://github.com/boyechko/eclectic-zettelkasten
;; Package-Requires: ((emacs "28.1") (ezeka "0.8") (octavo "0.1") (octavo-index "0.1"))

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

;; Octavo and Octavo-index integration for ezeka.el

(require 'ezeka)
(require 'octavo)
(require 'octavo-index)

;; For these avariables to be treated as dynamic, need to declare them first
;; here.

;;; Code:

(defvar octavo-directory)
(defvar octavo-id-regexp)
(defvar octavo-id-format)

(defmacro cmd-named (name &rest body)
  "Wrap the BODY inside an interactive defun form for NAME."
  (declare (indent 1))
  `(defun ,name ()
     (interactive)
     ,@body))
(put 'cmd-named 'lisp-indent-function 1)

(defun ezeka-octavo-initialize-kasten (name)
  "Set necessary variables for long-term work in Kasten with given NAME."
  (let ((kasten (ezeka-kasten name)))
    (setq octavo-directory (ezeka-kasten-directory kasten)
          octavo-id-regexp (ezeka--id-regexp))
    (if (eq (ezeka-kasten-id-type kasten) :numerus)
        (setq octavo-index-format "%i {%l} %c"
              octavo-id-time-string-format
              (concat (cl-subseq (downcase (format-time-string "%a")) 0 1) "-%H%M"))
      (setq octavo-id-time-string-format "%Y%m%dT%H%M"
            octavo-index-format "{%-12l} %c [[%i]]"))))

(defcustom ezeka-octavo-index-buffer-format "*Octavo-Index: %k*"
  "Format string to use when creating Octavo index buffers.
%k means capitalized kasten.
%K means upcased kasten."
  :type 'string)

(defun ezeka-octavo--index-buffer-name (kasten)
  "Return a name for an octavo index buffer for KASTEN.
The format is customizable via `ezeka-octavo-index-buffer-format'."
  (format-spec ezeka-octavo-index-buffer-format
               `((?k . ,(capitalize kasten))
                 (?k . ,(upcase kasten)))))

;;;###autoload
(defun ezeka-octavo-index-switch-to-kasten (kasten &optional choose noselect)
  "Create or switch to Octavo-Index buffer for given KASTEN.
If KASTEN is not specified, switch to the most recently
viewed index, if there are active ones. With CHOOSE (or
\\[universal-argument]), offer a choice of Kasten regardless. If NOSELECT
is non-nil (or \\[universal-argument] \\[universal-argument]), don't actually switch."
  (interactive
   (let ((active (cl-remove-if-not #'ezeka-kasten
                                   (ezeka-octavo--current-active-indexes)
                                   :key #'car))
         (prefix (prefix-numeric-value current-prefix-arg)))
     (list
      (if (or (not active) (= prefix 4))
          (completing-read "Switch to Kasten: "
                           (mapcar (lambda (k)
                                     (let ((name (ezeka-kasten-name k)))
                                       (if (assoc-string name active)
                                           (propertize name 'face 'bold-italic)
                                         name)))
                                   (ezeka-kaesten))
                           nil
                           t)           ; = user cannot exit without selecting
        (caar active))
      (= prefix 4)
      (= prefix 16))))
  (custom-set-variables
   '(octavo-subdirectory-function #'ezeka-id-subdirectory)
   `(octavo-index-buffer-name ,(ezeka-octavo--index-buffer-name kasten)))
  (ezeka-octavo-initialize-kasten kasten)
  (unless noselect
    (octavo-index)))

(defun ezeka-octavo--current-active-indexes ()
  "Return alist of currently active Kasten and their Octavo index buffers."
  (let ((regexp
         (string-replace "%k" "\\(.*\\)"
                         (regexp-quote ezeka-octavo-index-buffer-format))))
    (delq nil
          (mapcar (lambda (buf)
                    (when (string-match regexp (buffer-name buf))
                      (cons (downcase (match-string 1 (buffer-name buf)))
                            buf)))
                  (buffer-list)))))

(defun ezeka-octavo-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (ezeka-insert-header-template new-id nil title orig-id))

(defun ezeka-octavo-index-button-display-action (file buffer &optional same-window)
  "Function to display FILE or BUFFER on button press in Index and Desktop.
If \\[universal-argument] SAME-WINDOW is non-nil, use same window.
See `octavo-index-button-display-action'."
  (let ((buffer (or buffer (find-file-noselect file))))
    (with-current-buffer buffer
      (ezeka-breadcrumbs-drop buffer-file-name 'index))
    (let ((same-window (or same-window current-prefix-arg)))
      (cond ((one-window-p)
             (pop-to-buffer buffer
                            (display-buffer-in-direction
                             buffer
                             '((direction . top)
                               (window-height . 0.6)))))
            (same-window
             (find-file file))
            (t (find-file-other-window file))))))

(defun ezeka-octavo-format-function (format id title &optional noerror)
  "Format given ID and TITLE according to FORMAT.
Control sequences %i (ID), %c (caption), and %l (label) are
parsed from the filename. For everything else, call
`ezeka-format-metadata' instead. If NOERROR is non-nil, just
return nil if FORMAT cannot be rendered from ID and TITLE."
  (let* ((title (string-replace "%" "%%" (string-trim (or title ""))))
         (case-fold-search nil)
         (basic (format-spec format
                             `((?i . ,id)
                               ,@(if (string-match "^ *{\\(.*\\)} \\(.*\\)" title)
                                     `((?c . ,(match-string 2 title))
                                       (?l . ,(match-string 1 title)))
                                   `((?c . ,title)
                                     (?t . ,title))))
                             'delete))) ;; or 'ignore to possibly fetch mdata
    (if (and (not (string-match-p "%[0-9-]*[[:alpha:]]" basic))
             (not (member id ezeka--marked-for-rename)))
        basic
      (if-let* ((debug t)
                (file (ezeka-link-file id))
                (mdata (ezeka-file-metadata file)))
          ;; FIXME: Hackish way to catch hand-edited TITLE
          (progn
            (unless (string-match "^ *{\\(.*\\)} \\(.*\\)" title)
              (setf (alist-get 'title mdata) title))
            (ezeka-format-metadata format mdata))
        (unless noerror
          (error "Cannot retrieve metadata for `%s'" id))))))

;;;=============================================================================
;;; Mapping Across Octavo-Index Buttons
;;;=============================================================================

(defun ezeka-octavo-map-buttons (func &optional buffer start end)
  "Map FUNC across octavo-index buttons in BUFFER between START and END.
Like `widget-map-buttons' but for octavo-index buttons."
  (mapc func (octavo-index--current-button-list buffer start end)))

(defun ezeka-octavo-map-button-files (func &optional buffer start end)
  "Map FUNC across octavo-index buttons in BUFFER between START and END.
FUNC should be a function accepting arguments FILE, COUNTER,
TOTAL-FILES. Return list of files mapped across."
  (let* ((buttons (octavo-index--current-button-list buffer start end))
         (total (length buttons))
         (n 0)
         results)
    (dolist (button buttons (nreverse results))
      (let ((file (octavo--triplet-file (button-get button 'octavo-triplet))))
        (funcall func file n total)
        (push file results)
        (cl-incf n)))))

(defmacro define-octavo-index-mapper (name func &optional docstring &rest body)
  "Define interactive function to map FUNC across Octavo-Index region.
FUNC should take filename as the only argument. BODY is the body of
the index mapper command.

\fn(NAME (ARG) &OPTIONAL DOCSTRING &BODY BODY)"
  (declare (indent 2))
  (let ((arg (gensym "arg"))
        (beg (gensym "beg"))
        (end (gensym "end"))
        (file (gensym "file"))
        (files (gensym "files")))
    `(defun ,name (,arg ,beg ,end)
       ,docstring
       (interactive (if (region-active-p)
                        (list current-prefix-arg
                              (region-beginning) (region-end))
                      (list current-prefix-arg
                            (point-at-bol) (point-at-eol))))
       (let ((ezeka-save-after-metadata-updates t)
             (ezeka-update-modification-date 'never)
             (inhibit-read-only t))
         ,@body
         (let ((,files (nreverse (ezeka-octavo-map-button-files ,func nil ,beg ,end))))
           (delete-region ,beg ,end)
           (dolist (,file ,files ,files)
             (octavo-index--insert-button ,file)
             ;;(insert "\n")
             ))))))

(defvar ezeka-octavo--last-genus nil
  "Used by `ezeka-octavo-index-set-genus' to hold the last set genus.")

(defun ezeka-octavo-index-set-genus (file)
  "Set genus for the Zettel FILE at point in the Octavo-Index buffer.
Afteward, save the file without asking."
  (interactive (list (ezeka--grab-dwim-file-target)))
  (let ((ezeka-save-after-metadata-updates t)
        (ezeka-update-modification-date 'never)
        (inhibit-read-only t)
        (genus (ezeka-read-genus nil nil ezeka-octavo--last-genus)))
    (ezeka-set-genus file genus)
    (setq ezeka-octavo--last-genus genus)
    (when octavo-index-view-mode
      (with-current-buffer octavo-index-buffer-name
        (when (file-equal-p (ezeka--grab-dwim-file-target) file)
          (delete-region (point-at-bol) (point-at-eol))
          (octavo-index--insert-button file))))
    (octavo-index-next-line)))

(define-octavo-index-mapper ezeka-octavo-index-mass-set-genus
    (lambda (file &rest args) (ezeka-set-genus file ezeka-octavo--last-genus))
  "Set the genus for all notes in the active region and save the
files without asking."
  (setq ezeka-octavo--last-genus
    (ezeka-read-genus nil "Mass-set what genus? " ezeka-octavo--last-genus)))

(define-octavo-index-mapper ezeka-octavo-index-set-citekey
    (lambda (file &rest args) (ezeka-set-citekey file nil current-prefix-arg))
  "Set the citekey for all notes in the active region and save the
files without asking.")

(define-octavo-index-mapper ezeka-octavo-index-set-title
    (lambda (file &rest args) (ezeka-set-title file))
  "Set the title for all notes in the active region and save the files without
asking.")

(define-octavo-index-mapper ezeka-octavo-index-set-rubric
    (lambda (file &rest args)
      (let* ((metadata (ezeka-file-metadata file))
             (updated (ezeka-decode-rubric
                       (read-string "Change rubric to what? "
                                    (ezeka-encode-rubric metadata)))))
        (while updated
          (setf (alist-get (pop updated) metadata) (pop updated)))
        (ezeka--update-metadata-values file metadata)))
  "Set the entire rubric line for all notes in the active region and save the
files without asking.")

(define-octavo-index-mapper ezeka-octavo-index-entitle
    (lambda (file n out-of)
      (ezeka-entitle-file-name file
                               current-prefix-arg
                               (format "[%d/%d] New file name: " n out-of)))
  "Entitle all files in the currently active region of Octavo-Index
buffer. With \\[universal-argument], don't bother editing each new
name."
  (when (ezeka-file-p (current-buffer))
    (ezeka-entitle-file-name (buffer-file-name) current-prefix-arg)
    (save-buffer)
    (user-error "Use `ezeka-entitle-file-name' for singe files")))

(defun ezeka-octavo-move-all-in-region (start end kasten arg)
  "Move files between START and END of Octavo Index to KASTEN.
With \\[universal-argument] ARG, confirm each move and ask about
destination kasten."
  (interactive
   (append (if (region-active-p)
               (list (region-beginning) (region-end))
             (list (point-min) (point-max)))
           (list (completing-read "Which kasten to move to? "
                                  (mapcar #'ezeka-kasten-name (ezeka-kaesten)))
                 current-prefix-arg)))
  (let ((lines (count-lines start end))
        (moved 1)
        (octavo-alist (octavo--alist (ezeka--directory-files))))
    (goto-char start)
    (while (re-search-forward octavo-id-regexp end t)
      (let* ((id (match-string-no-properties 1))
             (title (buffer-substring-no-properties
                     (point-at-bol) (match-beginning 0)))
             (file (octavo--parse-id 'file-path id octavo-alist)))
        (when (and file
                   (or arg
                       (y-or-n-p
                        (format "[%d/%d] Move %s [%s] to %s? "
                                moved lines id title kasten))))
          (ezeka-move-to-another-kasten file kasten nil t)
          (cl-incf moved))))))

;;;=============================================================================
;;; Selecting notes
;;;=============================================================================

(eval-after-load 'vertico
  (defun ezeka-octavo--setup-vertico ()
    "Define `vertico-sort-history-alpha' sort if not already defined."
    (put 'vertico-sort--define 'lisp-indent-function 1)
    (unless (fboundp 'vertico-sort-reverse-alpha)
      (vertico-sort--define (reverse-alpha)
                            32 (if (eq % "") 0 (/ (aref % 0) 4)) string> string>))))

(defun ezeka-octavo-insert-link-to-kasten (kasten &optional link-only sort)
  "Insert a link to KASTEN.
If LINK-ONLY (or \\[universal-argument]) is non-nil, insert plain link.
If `vertico' exists and SORT is non-nil, set `vertico-sort-function' to it."
  (interactive (list (ezeka--read-kasten) nil current-prefix-arg))
  (when (featurep 'vertico) (ezeka-octavo--setup-vertico))
  (let ((vertico-sort-function (or sort 'vertico-sort-history-alpha))
        (target (ezeka--select-file
                 (ezeka--directory-files (ezeka-kasten kasten))
                 "Insert link to: ")))
    (funcall-interactively 'ezeka-insert-link-with-metadata
                           target
                           (unless link-only
                             (list (ezeka--read-metadata-field))))))

(defun ezeka-octavo--link-context (&optional n)
  "Return a string of N words before point."
  (string-trim
   (buffer-substring-no-properties
    (save-excursion
      (backward-word-strictly (or n 1))
      (point))
    (point))))

(defun ezeka-octavo-insert-contextual-link (&optional n kasten sort)
  "Insert a link to KASTEN based on the previous N words.
If KASTEN is not given, assume one with highest order or,
with \\[universal-argument], select Kasten interactively. If SORT is
non-nil, set `vertico-sort-function' to it."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (if (and current-prefix-arg
                  (not (integerp current-prefix-arg)))
             (ezeka--read-kasten)
           "numerus")))     ; HARDCODED
  (let ((pos (point))
        (vertico-sort-function (or sort 'vertico-sort-history-alpha))
        (octavo-link-and-title nil)
        (context (downcase (ezeka-octavo--link-context n))))
    (when (save-excursion
            (re-search-backward "[[:space:].,;:?!]"
                                (or (re-search-backward "\\sw" (point-at-bol) t)
                                    (point-at-bol))
                                'noerror
                                n))
      (goto-char (match-beginning 0)))
    (ezeka-insert-link-with-metadata
     (ezeka--select-file (ezeka--directory-files (ezeka-kasten kasten))
                         "Insert link to: "
                         'require-match
                         (string-trim context "[[:punct:]]" "[[:punct:]]")))))

(cmd-named ezeka-octavo-insert-link-to-numerus
  (ezeka-octavo-insert-link-to-kasten "numerus"))
(cmd-named ezeka-octavo-insert-link-to-tempus
  (ezeka-octavo-insert-link-to-kasten "tempus"
                                      current-prefix-arg
                                      'vertico-sort-reverse-alpha))
(cmd-named ezeka-octavo-insert-link-to-scriptum
  (ezeka-octavo-insert-link-to-kasten "scriptum"))

(defun ezeka-octavo-find-note-in-kasten (kasten &optional other-window sort)
  "Temporarily set octavo variables for KASTEN and call `octavo-find-file'.
With \\[universal-argument] OTHER-WINDOW, open in other window.
If `vertico' exists and SORT is non-nil, set `vertico-sort-function' to it."
  (interactive
   (list (if-let ((kasten
                   (and octavo-directory
                        (not current-prefix-arg)
                        (ezeka-directory-kasten octavo-directory))))
             kasten
           (ezeka--read-kasten))
         current-prefix-arg))
  (when (featurep 'vertico) (ezeka-octavo--setup-vertico))
  (let* ((vertico-sort-function (or sort 'vertico-sort-history-alpha))
         (file (ezeka--select-file (ezeka--directory-files kasten)
                                   (if other-window
                                       "Find note in other window: "
                                     "Find note: "))))
    (unless (ezeka-handle-symlink file (not other-window))
      (ezeka-find-file file (not other-window)))))

(defmacro ezeka-octavo--define-kasten-finders (kasten &optional sort)
  "Define a set of new commands for finding notes in KASTEN.
SORT is the function that vertico uses to sort the results."
  `(progn
     (defun ,(intern (concat "ezeka-octavo-find-in-" kasten)) ()
       ,(format "Find a note in %s kasten." kasten)
       (interactive)
       (ezeka-octavo-find-note-in-kasten ,kasten nil ,sort))
     (defun ,(intern (concat "ezeka-octavo-find-in-" kasten "-other-window")) ()
       ,(format "Find a note in %s kasten in other window." kasten)
       (interactive)
       (ezeka-octavo-find-note-in-kasten ,kasten 'other-window ,sort))))

(ezeka-octavo--define-kasten-finders "numerus")
(ezeka-octavo--define-kasten-finders "tempus" 'vertico-sort-reverse-alpha)
(ezeka-octavo--define-kasten-finders "scriptum")

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defvar ezeka--octavo-replace-links-before-history nil
  "History variable used in `ezeka-octavo-replace-links'.")
(defvar ezeka--octavo-replace-links-after-history nil
  "History variable used in `ezeka-octavo-replace-links'.")

(defun ezeka-octavo-replace-links (before after &optional directory confirm)
  "Replace BEFORE links to AFTER links in DIRECTORY.
DIRECTORY defaults to `ezeka-directory' if not given. If
AFTER is nil, replace the link with +BEFORE+. Return a tuple
of number of links replaced in number of files. If
CONFIRM (or \\[universal-argument]) is non-nil, check with
user before replacing."
  (interactive
   (if (save-excursion
         (beginning-of-line)
         (thing-at-point-looking-at "^(.*)$"))
       (when-let* ((_ (y-or-n-p
                       (format "%s\nThis looks like a move.log record. Use it? "
                               (match-string-no-properties 0))))
                   (rec (read (match-string-no-properties 0))))
         (list (car rec) (cadr rec) nil nil))
     (let* ((before (read-string "Replace link: "
                                 nil
                                 'ezeka--octavo-replace-links-before-history))
            (after (read-string (format "Replace `%s' with link: " before)
                                nil
                                'ezeka--octavo-replace-links-after-history)))
       (list before
             (if (string-empty-p after) nil after)
             nil
             (y-or-n-p "Confirm before replacing? ")))))
  (let* ((ezeka-header-update-modified nil)
         (ezeka-breadcrumbs-leave-trail nil)
         (before (ezeka-link-id before)) ; remove any explicit Kasten part
         (with-links
          (let ((octavo-directory (or directory ezeka-directory)))
            ;; NOTE: Requires `octavo--grep-file-list' after PR #68 that translates
            ;; Emacs regexp into POSIX form and defaults to extended regexps
            (octavo--grep-file-list
             (format "\\(parent: [a-z:]*%s$\\|firstborn: %s$\\|%s][][]\\)"
                     before before before))))
         (link-regexp (format "\\[\\[[a-z:]*%s]]" before))
         (replacement (if after
                          (ezeka--format-link after)
                        (format "+%s+" before)))
         (count 0)
         message)
    (if (not with-links)
        (message "No links to %s found" before)
      (dolist (file with-links count)
        (let ((open-buffer (get-file-buffer file))
              (file-mdata (ezeka-file-metadata file))
              (inhibit-read-only t))
          (save-excursion
            (with-current-buffer (or open-buffer (find-file-noselect file))
              (when confirm (switch-to-buffer (current-buffer)))
              ;; Replace parent
              (when (and (ezeka--child-p (ezeka-file-name-id file) before file-mdata)
                         (or (not confirm)
                             (y-or-n-p (format "Replace parent in %s (%s)? "
                                               (alist-get 'title file-mdata)
                                               (alist-get 'id file-mdata)))))
                ;; FIXME: Worth extending to preserve multiple parents?
                (setf (alist-get 'parent file-mdata) after)
                (ezeka--update-file-header file file-mdata)
                (cl-incf count))
              ;; Replace firstborn
              (when (and (string= before (alist-get 'firstborn file-mdata))
                         (or (not confirm)
                             (y-or-n-p (format "Replace firstborn in %s (%s)? "
                                               (alist-get 'title file-mdata)
                                               (alist-get 'id file-mdata)))))
                (setf (alist-get 'firstborn file-mdata) after)
                (ezeka--update-file-header file file-mdata)
                (cl-incf count))
              (goto-char (point-min))
              (if confirm
                  (query-replace-regexp link-regexp replacement)
                (while (re-search-forward link-regexp nil t)
                  (replace-match replacement t)
                  (cl-incf count)))
              (save-buffer)
              (ezeka--add-to-system-log 'replace-links nil
                'note (ezeka-encode-rubric file-mdata)
                'original before
                'new replacement)
              (unless open-buffer
                (kill-buffer (current-buffer)))))))
      (cons count (length with-links)))))

(defun ezeka--child-p (child parent &optional child-metadata)
  "Return non-nil if CHILD is a child of PARENT.
Both CHILD and PARENT are note IDs. If CHILD-METADATA is not
provided, read it from CHILD."
  (let* ((mdata (or child-metadata (ezeka-file-metadata child)))
         (parents (alist-get 'parent mdata)))
    (cl-typecase parents
      (null   nil)
      (string (string= parents parent))
      (cons   (cl-find parent parents :test #'string=))
      (t (error "Don't know how to handle " (type-of parents))))))

(defcustom ezeka-octavo-delete-note-method 'archive
  "How to actually delete Octavo notes.
When set to 'archive the notes are renamed with modified
`ezeka-file-extension', replacing the last character by
`_', e.g. \"tx_\" instead of \"txt\"."
  :type '(choice (const :tag "Delete" delete)
                 (const :tag "Archive" archive)))

(defun ezeka-octavo-delete-note (link-or-file &optional change-to method)
  "Delete the Zettel at LINK-OR-FILE, updating existing links.
Links are replaced with CHANGE-TO. If CHANGE-TO is not
given, suggest the note's successor, if set. METHOD overrides
`ezeka-octavo-delete-note-method'."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         nil
         (when (or (null ezeka-octavo-delete-note-method)
                   current-prefix-arg)
           (intern-soft
            (completing-read "Deletion method: "
                             '(delete archive)
                             nil
                             t)))))
  (let* ((method (or method ezeka-octavo-delete-note-method))
         (file (if (ezeka-link-p link-or-file)
                   (ezeka-link-file link-or-file)
                 link-or-file))
         (link (if (ezeka-link-p link-or-file)
                   link-or-file
                 (ezeka-file-link link-or-file)))
         (mdata (ezeka-file-metadata file))
         (successor (or (alist-get 'successor mdata)
                        (alist-get 'parent mdata)))
         (with-links (let ((octavo-directory ezeka-directory))
                       (octavo--grep-file-list
                        (format "(firstborn: %s|parent: %s|%s\\]\\])"
                                link link link))))
         (change-to
          (or change-to
              (and (not (zerop (length with-links)))
                   (read-string (format "Replace %d link(s) to %s with what? "
                                        (length with-links) link)
                                (if (and successor
                                         (file-exists-p (ezeka-link-file successor)))
                                    successor
                                  (concat "+" link "+")))))))
    (when with-links                    ; FIXME: Pass with-links!
      (ezeka-octavo-replace-links link change-to))
    (ezeka--add-to-move-log link
                            (concat (symbol-name method) "d")
                            (alist-get 'caption mdata)
                            (if with-links
                                (format "Replace %d links with %s"
                                        (length with-links) change-to)
                              "No links"))
    (when (y-or-n-p (format "Really %s note `%s'? "
                            (symbol-name method)
                            (alist-get 'rubric mdata)))
      (ezeka--add-to-system-log method nil
        'note link)
      (if (eq method 'delete)
          (delete-file file)
        (let ((archived
               (file-name-with-extension (file-name-base file)
                                         (replace-regexp-in-string
                                          ".$" "_"
                                          ezeka-file-extension))))
          (ezeka--rename-file file archived)
          (ezeka--git-stage-file archived)))
      (kill-buffer-ask (get-file-buffer file)))))

(defun ezeka-octavo-insert-link-to-index ()
  "Insert link to the ID at point in `octavo-index-buffer-name'."
  (interactive)
  (let ((id (with-current-buffer octavo-index-buffer-name
              (octavo-index--button-at-point))))
    (ezeka-insert-link-with-metadata id '(title) :before t)))

(provide 'ezeka-octavo)
;;; ezeka-octavo.el ends here
