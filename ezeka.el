;;; ezeka.el --- Eclectic Zettelkasten -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.5"))
;; Keywords: zettelkasten org
;; URL: https://github.com/boyechko/ezeka

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

;; This package provides a very personalized implementation of Zettelkasten
;; relying on Org, Deft (now moved to ezeka-deft.el) that began on 2015-06-31.

(require 'org)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defun in-ezeka-dir (&optional relative-path)
  "Returns absolute pathname of the given pathspec relative to
the Zettel directory."
  (expand-file-name (or relative-path "") ezeka-directory))

;; FIXME: temporary
(defvar ezeka-regexp-bolus-currens
  "\\([0-9]\\{3\\}\\)-\\([a-z]\\{3\\}\\)"
  "The regular expression that matches bolus currens like abc-123.")

(defvar ezeka-regexp-numerus-currens
  "\\([a-z]\\)-\\([0-9]\\{4\\}\\)"
  "The regular expression that matches numerus currens like d-0503.")

(defvar ezeka-regexp-tempus-currens
  "\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)T\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "The regular expression that matches the basic (but not extended) ISO 8601
timestamp.
Groups 1-3 are year, month, day.
Groups 4-5 are hour, minute.")

;; FIXME: Is this or the individually-named variables redundant?
(defvar ezeka-type-regexp-alist
  `((:bolus   . ,ezeka-regexp-bolus-currens) ; FIXME: temporary
    (:numerus . ,ezeka-regexp-numerus-currens)
    (:tempus  . ,ezeka-regexp-tempus-currens))
  "An alist of type and its regular expressions for the various slug types.")

(defvar ezeka-type-example-alist
  '((:bolus   . "123-abc") ; FIXME: temporary
    (:numerus . "a-1234")
    (:tempus  . "20210123T1234"))
  "An alist of type and an example of what it looks like for the various slug
types.")

(defvar ezeka-regexp-slug
  ;; Strip the groups in the component regexps
  (concat "\\("
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-numerus-currens)
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-tempus-currens)
          ;; FIXME: temporary
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-bolus-currens)
          "\\)")
  "A generalized regexp that matches any slug, whatever its slug type.")

(defvar ezeka-regexp-link
  (concat "\\(?9:\\(?1:[[:alpha:]]+\\):\\)*\\(?2:" ezeka-regexp-slug "\\)")
  "The regular expression that matches Zettel links.
Group 1 is the kasten, if specified.
Group 2 is the slug.")

(defvar ezeka-regexp-iso8601-date
  "\\<\\([0-9]\\{4\\}\\)-*\\([0-9]\\{2\\}\\)-*\\([0-9]\\{2\\}\\)"
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar ezeka-regexp-iso8601-time
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar ezeka-regexp-iso8601-datetime
  (concat ezeka-regexp-iso8601-date ezeka-regexp-iso8601-time)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defvar ezeka-pregenerated-numeri-file "auto/unused-numeri.dat"
  "List of unused numri curentes to use for creating new numerus currens
Zettel in rumen when Emacs cannot check the list of existing files.")

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom ezeka-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-extension "txt"
  "Default extension for Zettel files."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-kaesten
  ;; name | directory | slug type | list-order
  `(("os"         :tempus  1)
    ("rumen"      :numerus 2)
    ("esophagus"  :numerus 3)
    ("omasum"     :tempus  4)
    ("abomasum"   :tempus  5)
    ("rectum"     :tempus  6)
    ("fabula"     :tempus  7)
    ("machina"    :tempus  8))
  "An alist containing the names and slug types of kaesten."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-kaesten-aliases nil
  "An alist of any other aliases for the `ezeka-kaesten'. This is an alist of
the actual name followed by the alias."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-default-kasten
  ;; slug type | kasten name
  `((:numerus . "rumen")
    (:bolus . "esophagus")              ; FIXME: temporary
    (:tempus . "omasum"))
  "An alist of default Kasten (i.e. not requiring fully qualified link) for
each slug type."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-categories nil
  "A list of categories used for Zettel."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-number-of-frames nil
  "Try to use only this many frames. Nil means single frame."
  :type 'symbol
  :options '(one two many)
  :group 'ezeka)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun space-or-punct-p (char)
  "Returns T if the character is a space or punctuation."
  (when char
    (string-match-p "[[:space:][:punct:]]" (char-to-string char))))

;; TODO: More extensible way to do this without invoking other modes?
(defun ezeka--grab-dwim-file-target (&optional link-at-point)
  "Returns the do-what-I-mean Zettel file from a variety of modes. If
LINK-AT-POINT is non-nil, prioritize such a link if exists."
  (cond ((ezeka-link-at-point-p)
         (ezeka-link-file (ezeka-link-at-point) t))
        ((and ezeka-mode (ezeka-note-p buffer-file-name t))
         buffer-file-name)
        ((eq major-mode 'magit-status-mode)
         (magit-file-at-point))
        ((eq major-mode 'deft-mode)     ; TODO Factor out
         (--if-let (button-at (point))
             (button-get it 'tag)
           (ezeka-ivy-select-link)))    ; TODO Factor out
        (t
         (ezeka-ivy-select-link))))     ; TODO Factor out

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun ezeka-note-p (file-or-buffer &optional strict)
  "Returns non-NIL if the file or buffer is a Zettel. It is a Zettel if all
of these conditions are met:
1) the file exists;
2) its extension is `ezeka-file-extension';
3) its filename matches `ezeka-regexp-slug'; and, if STRICT is non-NIL,
4) the file is inside `ezeka-directory'."
  (interactive "f")
  (when file-or-buffer
    (let ((file (cl-typecase file-or-buffer
                  (buffer (buffer-file-name file-or-buffer))
                  (string (expand-file-name file-or-buffer))
                  (t
                   (signal 'type-error
                           '("FILE-OR-BUFFER can only be file or buffer"))))))
      (when file
        (and (string-equal (file-name-extension file) ezeka-file-extension)
             (string-match ezeka-regexp-slug (file-name-base file))
             (if strict
                 (string-prefix-p ezeka-directory file)
               t))))))

(defun ezeka-kasten-directory (kasten)
  "Returns the directory of the given KASTEN."
  (if (assoc kasten ezeka-kaesten)
      (file-name-as-directory (in-ezeka-dir (ezeka-kasten-truename kasten)))
    (error "Unknown Kasten: %s" kasten)))

(defun ezeka-directory-kasten (directory)
  "Returns the kasten name of the given Zettel directory."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun ezeka-kasten-truename (kasten)
  "Returns the true name of the given KASTEN."
  (or (cdr (assoc kasten ezeka-kaesten-aliases))
      (car (assoc kasten ezeka-kaesten))))

(defun ezeka-file-slug (file)
  "Returns the slug part of the given Zettel file."
  (file-name-base file))

;; FIXME: Relies on the fact that the Kasten directory is 2nd from the last.
(defun ezeka-file-kasten (file)
  "Returns the kasten of the given Zettel file."
  (let ((dirs (reverse (split-string (file-name-directory file) "/" t "/"))))
    (cond ((assoc (car dirs) ezeka-kaesten)
           (ezeka-kasten-truename (car dirs)))
          ((assoc (cadr dirs) ezeka-kaesten)
           (ezeka-kasten-truename (cadr dirs)))
          (t
           (error "Can't figure out kasten for %s" file)))))

(defun ezeka-file-link (file)
  "Given the path to a Zettel FILE, returns a fully qualified link to it."
  (let ((kasten (ezeka-file-kasten file)))
    (if (string= kasten
                 (alist-get (ezeka-type file) ezeka-default-kasten))
        (ezeka-file-slug file)
      (concat kasten ":" (ezeka-file-slug file)))))

(defun ezeka-link-p (string)
  "Returns non-NIL if the string could be a link to a Zettel."
  (and (string-match (concat "^" ezeka-regexp-link "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if (match-string-no-properties 1 string)
           (or (assoc (match-string-no-properties 1 string) ezeka-kaesten)
               (assoc (match-string-no-properties 1 string) ezeka-kaesten-aliases))
         t)))

(defun ezeka-link-kasten (link)
  "Returns the kasten part of the given LINK. If no kasten is explicitly
specified, asks the user to resolve the ambiguity."
  (when (string-match ezeka-regexp-link link)
    (let* ((kasten (match-string 1 link))
           (slug (match-string 2 link))
           (type (ezeka-type slug)))
      (or kasten
          (if-let ((default (alist-get type ezeka-default-kasten)))
              default
            (call-interactively #'ezeka-set-default-kasten)
            (ezeka-link-kasten link))))))

(defun ezeka-set-default-kasten (type kasten)
  "Interactively set the default kasten for the given slug type. See
`ezeka-default-kasten' for valid types."
  (interactive
   (list (intern (completing-read "Set default for which type of Zettel? "
                           (mapcar #'first ezeka-default-kasten)))
         (completing-read "Set the default to what Kasten? "
                   (if (listp ezeka-kaesten)
                       (mapcar #'first ezeka-kaesten)
                     (error "No Zettelkästen defined")))))
  (setf (alist-get type ezeka-default-kasten) kasten))

;; FIXME: Rename `ezeka-type' to `ezeka-slug-type'?
(defun ezeka-kasten-slug-type (kasten)
  "Returns the Zettel slug naming type for the given kasten based on
`ezeka-kaesten'."
  (cadr (assoc kasten ezeka-kaesten #'string=)))

(defun ezeka-link-slug (link)
  "Returns the slug part of the given LINK."
  (when (string-match ezeka-regexp-link link)
    (match-string 2 link)))

(defun ezeka-make-link (kasten slug)
  "Make a new proper link to SLUG in KASTEN."
  (let ((slug-type (ezeka-kasten-slug-type kasten)))
    (cond ((not slug-type)
           (error "Unknown kasten: %s" kasten))
          ((not
            (string-match-p (alist-get slug-type ezeka-type-regexp-alist) slug))
           (error "Slug doesn't match the slug type for %s kasten" kasten))
          ((rassoc kasten ezeka-default-kasten)
           slug)
          (t
           (concat kasten ":" slug)))))

(defun ezeka-numerus-subdirectory (slug)
  "Returns the right subdirectory for the given numerus currens slug."
  (when (string-match ezeka-regexp-numerus-currens slug)
    (file-name-as-directory (cl-subseq slug 0 1))))

(defun ezeka-tempus-subdirectory (slug)
  "Returns the right subdirectory for the given tempus currens slug."
  (when (string-match ezeka-regexp-tempus-currens slug)
    (file-name-as-directory (match-string 1 slug))))

(defun ezeka-bolus-subdirectory (slug)
  "Finds the right directory for the given bolus currens slug."
  (when (stringp slug)
    (let ((result
           (cl-case (elt slug 0)
             (?0 "000-099")
             (?1 "100-199")
             (?2 "200-299")
             (?3 "300-399")
             (?4 "400-499")
             (?5 "500-599")
             (?6 "600-699")
             (?7 "700-799")
             (?8 "800-899")
             (?9 "900-999"))))
      (when result
        (file-name-as-directory result)))))

(defun ezeka-link-file (link &optional noerror)
  "Return a full file path to the Zettel LINK. If NOERROR is non-NIL,
don't signal an error if the link is invalid."
  (if (ezeka-link-p link)
      (let ((kasten (ezeka-link-kasten link))
            (slug (ezeka-link-slug link)))
        (expand-file-name
         (concat slug "." ezeka-file-extension)
         (expand-file-name
          (cl-case (ezeka-type slug)
            (:numerus (ezeka-numerus-subdirectory slug))
            (:tempus (ezeka-tempus-subdirectory slug))
            (:bolus (ezeka-bolus-subdirectory slug)) ; FIXME: temporary
            (t (unless noerror
                 (error "This is not a proper Zettel link: %s" link))))
          (ezeka-kasten-directory kasten))))
    (unless noerror
      (error "This is not a proper Zettel link: %s" link))))

;; FIXME: Rename `ezeka-type' to `ezeka-slug-type'?
(defun ezeka-type (slug-or-file)
  "Returns the type of the given slug or file: :NUMERUS or :TEMPUS."
  (let ((slug (file-name-base slug-or-file)))
    (cond ((string-match-p ezeka-regexp-tempus-currens slug)
           :tempus)
          ((string-match-p ezeka-regexp-numerus-currens slug)
           :numerus)
          ;; FIXME: Temporary
          ((string-match-p ezeka-regexp-bolus-currens slug)
           :bolus)
          (t
           ;; Anything else is not a Zettel
           nil))))

(defun ezeka-encode-iso8601-datetime (string)
  "Returns the internal encoded time given the ISO8601 date/time
expression, with or without time."
  (let ((cadr 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" ezeka-regexp-iso8601-date) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match ezeka-regexp-iso8601-time string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun ezeka-file-content (file &optional metadata-only noerror)
  "Returns the content of the FILE, getting it either from an opened buffer
or the file itself. If NOERROR is non-NIL, don't signal an error if cannot
get the content. If METADATA-ONLY is non-nil, only get the metadata."
  (cl-flet ((retrieve-content ()
                              "Get the content from `current-buffer'."
                              (widen)
                              (buffer-substring-no-properties
                               (point-min)
                               (if (not metadata-only)
                                   (point-max)
                                 (goto-char (point-min))
                                 (if (re-search-forward "\n\n" nil t)
                                     (match-beginning 0)
                                   (unless noerror
                                     (error "Cannot separate metadata")))))))
    (cond ((get-file-buffer file)
           (save-excursion
             (save-restriction
               (with-current-buffer (get-file-buffer file)
                 (retrieve-content)))))
          ((file-exists-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (retrieve-content)))
          (t
           (unless noerror
             (error "Cannot get content for %s" file))))))

;;;=============================================================================
;;; Metadata
;;;=============================================================================

(defvar ezeka-regexp-metadata-line
  "\\(?1:\\w+\\):\\s-+\\(?2:.*\\)"
  "The regular expression that matches a YAML metadata line.
Group 1 is the key.
Group 2 is the value.")

(defvar ezeka-combined-title-format "title: §%s {%s} %s"
  "The format string, suitable for use in FORMAT with argments of link,
category, and title in that order.")

(defvar ezeka-regexp-combined-title
  (concat "§"
          ezeka-regexp-link             ; \1 and \2
          "\\(?9:\\.\\)* \\(?8:{\\(?3:[^}]+\\)}\\)*\\(?4:[^#@]+\\)*\\(?5:@\\S-+\\)*\\(?6:#.+\\)*")
  "Regular expression for a combined title string, used in `ezeka-file-metadata'.
Group 1 is the kasten.
Group 2 is the slug.
Group 3 is the category.
Group 4 is the title itself.
Group 5 is the citation key.
Group 6 is the keyword block.")

(defun ezeka-decode-combined-title (combined)
  "Returns an alist of metadata from a combined title. If cannot decode,
returns NIL."
  (when (and combined (string-match ezeka-regexp-combined-title combined))
    (let ((slug     (match-string 2 combined))
          (category (match-string 3 combined))
          (title    (match-string 4 combined))
          (citekey  (match-string 5 combined))
          (keywords (match-string 6 combined)))
      (list (cons :slug slug)
            (cons :type (ezeka-type slug))
            (cons :category category)
            (cons :title (if title (string-trim title) ""))
            (when citekey (cons :citekey (string-trim citekey)))
            (when keywords (cons :keywords (list (string-trim keywords))))))))

(defun ezeka-encode-combined-title (metadata)
  "Returns a list of two elements: 1) string that encodes into the title line
the given METADATA, and 2) leftover metadata."
  (list (format "§%s {%s} %s%s"
                (alist-get :link metadata)
                (or (alist-get :category metadata) "Unset")
                (alist-get :title metadata)
                (if (alist-get :citekey metadata)
                    (concat " " (alist-get :citekey metadata))
                  ""))
        (cl-set-difference metadata
                           '((:link) (:category) (:title) (:type) (:citekey))
                           :key #'car)))

(defun ezeka-metadata-yaml-key (keyword)
  "Returns a YAML-formatted string that is the name of the KEY, a keyword
symbol."
  (cl-subseq (symbol-name keyword) 1))

(defun ezeka-metadata-yaml-value (value)
  "Returns a YAML-formatted string for the given metadata VALUE."
  (cl-typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun ezeka-normalize-metadata (file &optional metadata)
  "Replaces the FILE's metadata section with either the given METADATA or
by parsing the FILE's metadata."
  (let ((metadata (or metadata (ezeka-file-metadata file)))
        (old-point (point)))
    (save-mark-and-excursion
      (with-current-buffer (get-file-buffer file)
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t 1)
            (narrow-to-region (point-min) (point)))
          (delete-region (point-min) (point-max))
          (cl-multiple-value-bind (title remaining-metadata)
              (ezeka-encode-combined-title metadata)
            (insert "title: " title "\n")
            (mapc (lambda (cons)
                    (insert (format "%s: %s\n"
                                    (ezeka-metadata-yaml-key (car cons))
                                    (ezeka-metadata-yaml-value (cdr cons)))))
                  (let (ordered-metadata)
                    (dolist (key '(:subtitle :author
                                   :created :modified
                                   :parent :firstborn :oldnames
                                   :readings :keywords)
                                 (nreverse ordered-metadata))
                      (when (alist-get key remaining-metadata)
                        (push (cons key (alist-get key remaining-metadata))
                              ordered-metadata)))))))))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

(defun ezeka-decode-metadata-section (yaml-section file)
  "Returns an alist of metadata decoded from the given yaml metadata section
of FILE. They keys are converted to keywords."
  (let* ((metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match ezeka-regexp-metadata-line line)
                   (let ((key (intern (concat ":" (match-string 1 line))))
                         (value (string-trim (match-string 2 line) " " " ")))
                     (cons key
                           ;; Handle lists properly
                           (if (string-match "^\\[\\(.*\\)\\]$" value)
                               (split-string (match-string 1 value)
                                             "," t "[[:space:]]+")
                             value)))
                 (error "Malformed metadata line: '%s'" line))))
           (split-string yaml-section "\n")))
         (title (alist-get :title metadata))
         (decoded (ezeka-decode-combined-title title)))
    ;; When successfully decoded combined title, replace the original title with
    ;; the decoded metadata.
    (when decoded
      (setq metadata
        (append decoded (cl-remove :title metadata :key #'car))))
    (push (cons :kasten (ezeka-file-kasten file)) metadata)
    (push (cons :link (ezeka-file-link file)) metadata)))

(defun ezeka-file-metadata (file)
  "Returns an alist of metadata for the given FILE based on the most current
content of the FILE. They keys are converted to keywords."
  (ezeka-decode-metadata-section (ezeka-file-content file t t) file))

(defcustom ezeka-update-modification-date t
  "Determines whether `ezeka-update-metadata-date' updates the modification
date. Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (or T)."
  :type 'symbol)

(defun ezeka-update-metadata-date ()
  "Updates the modification time in the metadata section of the Zettel in the
current buffer according to the value of `ezeka-update-modifaction-date'."
  (interactive)
  (when (ezeka-note-p buffer-file-name)
    (let* ((today (format-time-string "%Y-%m-%d"))
           (now (format-time-string "%Y-%m-%d %a %H:%M"))
           (metadata (ezeka-file-metadata buffer-file-name))
           (last-modified (or (alist-get :modified metadata)
                              (alist-get :created metadata))))
      (unless (string-equal (or last-modified "") now)
        ;; FIXME: Probably better to convert modification times to Emacs's encoded
        ;; time rather than doing it with strings.
        (when (or (equal ezeka-update-modification-date 'always)
                  (and (equal ezeka-update-modification-date 'sameday)
                       (string= (cl-subseq last-modified 0 (length today)) today))
                  ;; Automatic updating conditions not met; need to confirm
                  (and (member ezeka-update-modification-date '(sameday confirm t))
                       (y-or-n-p
                        (format "%s last modified at %s. Update to now? "
                                (file-name-base buffer-file-name)
                                last-modified))))
          (setf (alist-get :modified metadata) now)))
      (ezeka-normalize-metadata buffer-file-name metadata))))

(defun ezeka-update-title ()
  "Interactively asks for a different title and updates the Zettel's metadata."
  (interactive)
  (when (ezeka-note-p buffer-file-name)
    (let ((metadata (ezeka-file-metadata buffer-file-name)))
      (setf (alist-get :title metadata)
            (read-string "Change title to what? "
                         (alist-get :title metadata)))
      (ezeka-normalize-metadata buffer-file-name metadata))))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun ezeka-make-numerus (number letters)
  "Returns a new numerus currens slug composed of the NUMBER and LETTERS,
both of which are strings."
  (concat number "-" letters))

(defun ezeka-numerus-number (slug)
  "Returns the number part of the SLUG as a string."
  (when (string-match ezeka-regexp-numerus-currens slug)
    (match-string 1 slug)))

(defun ezeka-numerus-letters (slug)
  "Returns the letters part of the SLUG as a string."
  (when (string-match ezeka-regexp-numerus-currens slug)
    (match-string 3 slug)))

(defun ezeka-numerus-parts (slug)
  "Returns NIL if the slug is not a numerus currens slug, and otherwise
returns a list of two elements: the number and letters parts of the slug."
  (when (and (stringp slug)
             (string-match ezeka-regexp-numerus-currens slug))
    (list (match-string 1 slug) (match-string 3 slug))))

(defun abase26-letter-to-decimal (letter)
  "Returns the decimal number corresponding to the given character-as-string.
Case-insensitive."
  (if (string-match "[a-zA-Z]" letter)
      (- (string-to-char (downcase letter)) ?a)
    (error "LETTER must be a string of one letter")))

(defun abase26-decimal-to-letter (n)
  "Returns a string of the number in abase26 corresponding to the given
decimal."
  (if (< -1 n 26)
      (char-to-string (+ n ?a))
    (error "N must be an integer between 0 and 25")))

(defun abase26-encode (n &optional width)
  "Returns a string representation of the integer in the 'alphabetic' base
26. If WIDTH is given, returns the string at least WIDTH wide, padded with
abase26 equivalent of 0, namely 'a'."
  (let (digits)
    (while (> n 25)
      (push (% n 26) digits)
      (setq n (/ n 26)))
    (push n digits)
    (when width
      (while (> width (length digits))
        (push 0 digits)))
    (apply #'concat (mapcar #'abase26-decimal-to-letter digits))))

(defun abase26-decode (string)
  "Returns the integer for the given string representation in the
'alphabetic' base 26."
  (let ((n (1- (length string)))
        (total 0))
    (dolist (d (split-string string "" t))
      (setq total (+ total (* (abase26-letter-to-decimal d) (expt 26 n))))
      (setq n (1- n)))
    total))

(defun ezeka-generate-new-slug (type)
  "Generates a random new slug of the given type."
  (cl-case type
    (:tempus (format-time-string "%Y%m%dT%H%M"))
    (:bolus  (format "%03d-%s"
                     (random 1000)
                     (abase26-encode (random (expt 26 3)) 3)))
    (:numerus (format "%s-%04d"
                      (abase26-encode (random 26))
                      (random 10000)))
    (:index   (format "%02d-%s"
                      (random 100)
                      (abase26-encode (random 26))))
    (t        (error "Unknown Zettel type"))))

(defun ezeka-next-unused-slug (kasten)
  "Returns the next unused slug for the given KASTEN from `ezeka-kaesten'."
  (let ((type (ezeka-kasten-slug-type kasten))
        slug)
    (cl-flet ((exists? ()
                "Checks if SLUG is either NIL or exists."
                (or (null slug)
                    (file-exists-p
                     (ezeka-link-file (ezeka-make-link kasten slug))))))
      (cond ((eq type :tempus)
             (while (exists?)
               (setq slug (ezeka-generate-new-slug type))))
            ((and (eq type :numerus)
                  (file-exists-p (in-ezeka-dir ezeka-pregenerated-numeri-file)))
             (unwind-protect
                 (with-current-buffer
                     (find-file-noselect
                      (in-ezeka-dir ezeka-pregenerated-numeri-file))
                   (let ((left (count-lines (point-min) (point-max))))
                     (unwind-protect
                         (while (and (> left 0) (exists?))
                           (setq slug
                             (string-trim
                              (delete-and-extract-region
                               (point-min)
                               (search-forward-regexp "[[:space:]]" nil t))))
                           (cl-decf left))
                       (let ((inhibit-message t))
                         (basic-save-buffer)))
                     (message "%d pregenerated numer%s left"
                              left
                              (if (= left 1) "us" "i"))))))
            (t
             (while (exists?)
               (setq slug (ezeka-generate-new-slug type))))))
    slug))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun ezeka-decode-time-into-tempus-currens (time)
  "Returns a tempus currens slug based on the given Emacs time object."
  (format-time-string "%Y%m%dT%H%M" time))

(defun ezeka-tempus-currens-slug-for (link)
  "Returns a suitable tempus currens slug for the given Zettel link."
  (if (eq (ezeka-kasten-slug-type (ezeka-link-kasten link)) :tempus)
      ;; If already tempus currens, just return that slug
      (ezeka-link-slug link)
    ;; Otherwise come up with an appropriate slug based on the metadata
    (let ((metadata (ezeka-file-metadata (ezeka-link-file link)))
          oldname)
      (cond ((setq oldname
               (cl-find-if
                (lambda (l)
                  (eq (ezeka-kasten-slug-type (ezeka-link-kasten l))
                      :tempus))
                (alist-get :oldnames metadata)))
             ;; One of the old names was a tempus currens; just use that
             (ezeka-link-slug oldname))
            ((alist-get :created metadata)
             ;; Use the created metadata and make up the time of creation
             ;; FIXME: Any more elegant way to do this?
             (ezeka-decode-time-into-tempus-currens
              ;; TODO: This needs to handle org-mode timestamps in metadata
              (ezeka-encode-iso8601-datetime
               (concat (alist-get :created metadata)
                       "T"
                       (format-time-string "%H:%M")))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (ezeka-next-unused-slug (ezeka-link-kasten link))))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun ezeka-link-at-point-p (&optional freeform)
  "Returns non-nil if the thing at point is a wiki link (i.e. [[XXX]]). The
first group is the link target. If FREEFORM is non-nil, also consider Zettel
links that are not enclosed in square brackets."
  (thing-at-point-looking-at
   (replace-regexp-in-string
    "(\\?[0-9]:" "("
    (if freeform
        (concat "\\(?1:" ezeka-regexp-link "\\)")
      (concat "\\[\\[\\(?1:" ezeka-regexp-link "\\)\\]\\(\\[[^]]+\\]\\)*\\]")))))

(defun ezeka-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`ezeka-link-at-point-p'."
  (match-string-no-properties 1))

(defun ezeka-find-file (file &optional same-window)
  "Edit the given file based on the value of `ezeka-number-of-frames'. If
SAME-WINDOW is non-NIL, opens the buffer visiting the file in the same
window."
  (if same-window
      (find-file file)
    (cl-case ezeka-number-of-frames
      (two (if (< (length (frame-list)) 2)
               (find-file-other-frame file)
             (select-window (ace-select-window))
             (find-file file)))
      (one (let ((pop-up-windows t))
             (select-window (ace-select-window))
             (find-file file)))
      (nil (find-file file))
      (t (find-file-other-frame file)))))

(defun ezeka-find-link (link &optional same-window)
  "Attempts to find the given Zettel link based on the value of
`ezeka-number-of-frames'. If SAME-WINDOW is non-NIL, opens the link in the
same window. Returns T if the link is a Zettel link."
  (when (ezeka-link-p link)
    (ezeka-find-file (ezeka-link-file link) same-window)
    (when (zerop (buffer-size))
      (call-interactively #'ezeka-insert-metadata-template))
    ;; make sure to return T for `org-open-link-functions'
    t))

(defun ezeka-kill-link-or-sexp-at-point (&optional arg)
  "If there is a Zettel link at point, kill it, including the square
brackets. Otherwise, call `kill-sex'."
  (interactive "p")
  (if (ezeka-link-at-point-p)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (kill-new (buffer-substring-no-properties start end))
        (delete-region start end)
        (let ((around (string (preceding-char) (following-char))))
          (cond ((string-match-p "\\s \\s " around)
                 (just-one-space 1))
                ((string-match-p "\\s " around)
                 (just-one-space 0)))))
    (kill-sexp arg)))

(defun ezeka-org-format-link (target &optional description)
  "Returns a formatted org-link to TARGET, which can be either a link or a filepath."
  (let* ((file (or (if (file-name-absolute-p target)
                       target
                     (ezeka-link-file target))
                   (error "Link target doesn't exist; make sure it's saved")))
         (link (ezeka-file-link file)))
    (format "[[%s]%s]"
            link
            (if description
                (format "[%s]" description) ""))))

(defun ezeka-insert-link-with-metadata (link &optional field where confirm)
  "Inserts the Zettel link, optionally adding a metadata FIELD put
WHERE (:BEFORE, :AFTER, or in :DESCRIPTION). If CONFIRM is non-NIL, ask for
confirmation before inserting metadata."
  (let* ((file (ezeka-link-file link))
         (metadata (ezeka-file-metadata file))
         (field (or field
                    (when (called-interactively-p 'any)
                      (intern-soft
                       (completing-read
                        "Which metadata field? "
                        '(":none" ":title" ":citekey" ":category"))))))
         (value (alist-get field metadata))
         (where (or where
                    (when field
                      (intern-soft
                       (completing-read "Where? "
                                 '(":before" ":after" ":description")))))))
    (insert (if (or (bolp) (space-or-punct-p (char-before))) "" " ")
            (if (or (null value)
                    (not confirm)
                    (progn
                      ;; Pressing return just defaults to NO rather than quit
                      (define-key query-replace-map [return] 'act)
                      (y-or-n-p (format (if (eq where :description)
                                            "Insert %s in the link %s? "
                                          "Insert %s %s the link? ")
                                        field where))))
                (concat (if (eq where :before)
                            (concat value " ")
                          "")
                        (ezeka-org-format-link
                         link
                         (when (eq where :description)
                           value))
                        (if (eq where :after)
                            (concat " " value)
                          ""))
              (ezeka-org-format-link link))
            (if (or (eolp) (space-or-punct-p (char-after))) "" " "))))

(defun ezeka-insert-link-from-clipboard (arg)
  "Link `ezeka-insert-link' but attempts to get the link slug from OS
clipboard, inserting it with metadata. With prefix argument, insert just the
link itself."
  (interactive "P")
  (let ((link (gui-get-selection 'CLIPBOARD))
        (backlink (when buffer-file-name
                    (ezeka-file-link buffer-file-name))))
    (when (ezeka-link-p link)
      (if arg
          (ezeka-insert-link-with-metadata link)
        (ezeka-insert-link-with-metadata link :title :before t))
      (when backlink
        (gui-set-selection 'CLIPBOARD backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun ezeka-kill-ring-save-link-title (arg)
  "Save the title to the kill ring and system clipboard of either the Zettel
link at point or, if there is none, the current buffer. With prefix argument,
saves the 'combinted title'."
  (interactive "P")
  (let ((file (ezeka--grab-dwim-file-target t)))
    (when file
      (let* ((metadata (ezeka-file-metadata file))
             (title (if arg
                        (car (ezeka-encode-combined-title metadata))
                      (alist-get :title metadata))))
        (kill-new title)
        (unless select-enable-clipboard
          (gui-set-selection 'CLIPBOARD title))
        (message "Saved [%s] in the kill ring" title)))))

(defun ezeka-kill-ring-save-link (arg)
  "Save the Zettel link at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `ezeka-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (ezeka--grab-dwim-file-target t)))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name file ezeka-directory)
                    (ezeka-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (gui-set-selection 'CLIPBOARD link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R %s &" file)))))))

(defun ezeka-kill-ring-save-next-link ()
  "Save the first link at or after point (but before EOL)."
  (interactive)
  (save-excursion
    (let ((link (if (ezeka-link-at-point-p)
                    (ezeka-link-at-point)
                  (let ((eol (save-excursion (end-of-visual-line) (point))))
                    (when (re-search-forward ezeka-regexp-link eol t)
                      (match-string-no-properties 0))))))
      (when link
        (kill-new link)
        (message "Saved [%s] to kill ring" link)))))

(defun ezeka-links-to (link)
  "Runs a recursive grep (`rgrep') to find references to the link at point or
to current Zettel."
  (interactive (list (ezeka-file-link (ezeka--grab-dwim-file-target t))))
  (grep-compute-defaults)
  (rgrep link "*.txt" (f-slash ezeka-directory) nil))

(defun ezeka-rgrep (string)
  "Runs a recursive grep (`rgrep') for the given STRING across all Zettel."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (rgrep string "*.txt" (f-slash ezeka-directory) nil))

;; Show the beginning of Zettel title in mode-line
(defun ezeka-show-title-in-mode-line ()
  (interactive)
  (when (and (ezeka-note-p buffer-file-name)
             (not (zerop (buffer-size))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((metadata
               (ezeka-decode-combined-title
                (buffer-substring-no-properties
                 (or (re-search-forward "title: " nil t) (point-min))
                 (point-at-eol)))))
          (when metadata
            (let ((words (split-string (alist-get :title metadata))))
              (setq-local mode-line-misc-info
                          (replace-regexp-in-string
                           "/" "" (mapconcat #'identity
                                    (cl-subseq words 0 (min 5 (length words)))
                                    " "))))))))))
(add-hook 'ezeka-mode-hook 'ezeka-show-title-in-mode-line)

(defun ezeka-update-link-prefix-title ()
  "Kills text from point to the next Zettel link, replacing it with that
Zettel's title."
  (interactive)
  (save-excursion
    (let ((start (point)))
      (org-next-link)
      (when (ezeka-link-at-point-p)
        (let* ((file (ezeka-link-file (ezeka-link-at-point)))
               (title (alist-get :title (ezeka-file-metadata file))))
          (delete-region start (1- (point)))
          (backward-char)
          (insert title))))))

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun ezeka-trace-genealogy (file-or-link &optional degree)
  "Returns the FILE-OR-LINK's next genealogical link, or NIL if could not
figure out. With the optional DEGREE, try to find the Nth link (i.e.
grandparent if DEGREE is 2, child if DEGREE is -1, an so on), returning the
most remote link that could be found."
  (let ((degree (or degree 1)))
    (if (= (abs degree) 0)
        file-or-link
      (ezeka-trace-genealogy (alist-get (if (> degree 0)
                                            :parent
                                          :firstborn)
                                        (ezeka-file-metadata
                                         (if (ezeka-link-p file-or-link)
                                             (ezeka-link-file file-or-link)
                                           file-or-link)))
                             (if (> degree 0)
                                 (1- degree)
                               (1+ degree))))))

(defun ezeka-find-ancestor (n)
  "Opens the current Zettel's immediate ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-note-p buffer-file-name)
    (let ((ancestor (ezeka-trace-genealogy buffer-file-name n)))
      (if ancestor
          (ezeka-find-link ancestor)
        (message "No ancestor found")))))

(defun ezeka-find-descendant (n)
  "Opens the current Zettel's immediate descendant. With a prefix argument,
try to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-note-p buffer-file-name)
    (let ((descendant (ezeka-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (ezeka-find-link descendant)
        (message "No descendant found")))))

(defun ezeka-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current Zettel, adding its title (if
available) before the link. With a numerical prefix argument, try to find Nth
ancestor. With a universal argument, ask for confirmation before inserting."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (ezeka-trace-genealogy buffer-file-name degree)))
    (if link
        (ezeka-insert-link-with-metadata link :title :before (not arg))
      (message "Could not find such ancestor"))))

(defvar ezeka-note-parent-of-new-child nil
  "An alist of new children and their respective parents.")

(defun ezeka-generate-new-child (parent kasten)
  "Generates a new child of the given PARENT in the KASTEN."
  (let ((child-link (ezeka-make-link
                     kasten
                     (ezeka-next-unused-slug kasten))))
    (add-to-list 'ezeka-note-parent-of-new-child (cons child-link parent))
    child-link))

(defun ezeka-insert-new-child (&optional arg)
  "Inserts a link to a new Zettel in the same Kasten as the current Zettel,
saves the current Zettel as its parent, and sets the `ezeka-link-backlink'
to current Zettel. With prefix argument, allows the user to select a
different Kasten. With double prefix argument, asks for the full link.
Returns link the new child."
  (interactive "P")
  (when (ezeka-note-p buffer-file-name t)
    (let ((parent-link (ezeka-file-link buffer-file-name))
          child-link)
      (if (equal arg '(16))
          (while (not child-link)
            (setq child-link (read-string "Enter link for new child: "))
            (when (file-exists-p (ezeka-link-file child-link))
              (message "This Zettel already exists; try again")))
        (let ((kasten (if (equal arg '(4))
                          (completing-read
                           "Kasten: "
                           (if (listp ezeka-kaesten)
                               (mapcar #'car ezeka-kaesten)
                             (error "No `ezeka-kaesten' defined")))
                        (ezeka-link-kasten parent-link))))
          (setq child-link (ezeka-generate-new-child parent-link kasten))))
      (insert (ezeka-org-format-link child-link))
      child-link)))

(defun ezeka-insert-new-child-from-org-item (&optional arg)
  "Wrapper around `ezeka-insert-new-child' that, if the point is in a list
item, first saves the text of the item in the kill ring before inserting the
new child link. Passes the prefix argument to `ezeka-insert-new-child'."
  (interactive "P")
  (let* ((start (or (org-in-item-p) (point)))
         (text (cond ((region-active-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     ((org-in-item-p)
                      (buffer-substring-no-properties (1+ start) (point)))
                     (t (save-excursion
                          (beginning-of-line)
                          ;; FIXME: Might be good to have some limit to prevent
                          ;; killing whole paragraphs worth of text with soft
                          ;; newlines.
                          (buffer-substring-no-properties
                           (point) (1- start))))))
         (citekey (or (alist-get :citekey
                        (ezeka-file-metadata buffer-file-name))
                      "")))
    (when text
      (kill-new (concat (org-trim text)
                        (when citekey " @")
                        citekey)))
    (ezeka-insert-new-child arg)))

;;;=============================================================================
;;; Buffers and Frames
;;;=============================================================================

(defun ezeka-visiting-buffer-list (&optional skip-current)
  "Returns a list of Zettel files that are currently being visited. If
SKIP-CURRENT is T, remove the current buffer."
  (mapcar #'buffer-file-name
          (cl-remove-if-not (lambda (buf)
                              (ezeka-note-p (buffer-file-name buf)))
                            (remove (when skip-current
                                      (current-buffer))
                                    (buffer-list)))))

(defun ezeka-kill-visiting-buffers ()
  "Kills all Zettel that are being currently visited."
  (interactive)
  (mapc (lambda (file)
          (kill-buffer (get-file-buffer file)))
        (ezeka-visiting-buffer-list t)))

(defun ezeka-formatted-frame-title ()
  "Returns a string suitable for `frame-title-format' as a way to
consistently format the frame title with useful information for
Zettelkasten work."
  (interactive)
  (concat (if (ezeka-note-p buffer-file-name)
              (let ((metadata (ezeka-file-metadata buffer-file-name)))
                (format "%s §%s@%s"
                        (alist-get :title metadata)
                        (alist-get :slug metadata)
                        (alist-get :kasten metadata)))
            "%b")))

(defun ezeka-completion-table (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `completing-read' as collection."
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

(defun ezeka-switch-to-buffer (arg)
  "Quickly switch to other open Zettel buffers. With prefix argument, do so
in another window."
  (interactive "P")
  (let ((table (ezeka-completion-table (ezeka-visiting-buffer-list t))))
    (funcall (if arg 'find-file-other-window 'find-file)
             (cdr (assoc-string
                   (completing-read "Visit buffer: " table nil t) table)))))

;;;=============================================================================
;;; Categories
;;;=============================================================================

(defun ezeka-read-category (&optional arg prompt sort-fn)
  "Uses `completing-read' to select a category from `ezeka-categories'. With
prefix argument, asks the user to type in the category directly. If SORT-FN
is given, use that to sort the list first."
  (let ((prompt (or prompt "Category: "))
        (categories (if (not (functionp sort-fn))
                        ezeka-categories
                      (let ((cats-copy (cl-copy-list ezeka-categories)))
                        (cl-sort cats-copy sort-fn :key #'cdr)))))
    (if current-prefix-arg
        (read-string prompt)
      (completing-read prompt categories))))

(defun ezeka-set-category (file category)
  "Sets the category to the Zettel title based on `ezeka-categories'. With
prefix argument, allows the user to type in a custom category."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (ezeka-read-category nil nil #'>)))
  (let ((orig-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (cond ((not (re-search-forward ezeka-regexp-combined-title nil t))
               (message "Not sure how to set the category here"))
              ((match-string 3)
               (replace-match category nil nil nil 3))
              (t
               (goto-char (match-beginning 4))
               (insert "{" category "} ")))
        ;; only save buffer if was not initially visiting it
        (unless (eq orig-buffer (current-buffer))
          (save-buffer))))))

;;;=============================================================================
;;; Populating Files
;;;=============================================================================

(defun ezeka-insert-metadata-template (&optional link category title parent)
  "Inserts the metadata template into the current buffer, populating the
metadata with the LINK, CATEGORY, TITLE, and PARENT."
  (interactive (list (if buffer-file-name
                         (ezeka-file-link buffer-file-name)
                       ;; FIXME: Rewrite with completing-read
                       (read-string "Link to this note: "))
                     (ezeka-read-category)
                     (read-string "Title: ")
                     (read-string "Link to parent? "
                                  (cdr (assoc link ezeka-note-parent-of-new-child)))))
  (let ((file (ezeka-link-file link)))
    (when (zerop (buffer-size))
      (insert (format ezeka-combined-title-format
                      link
                      (or category "Unset")
                      (or title "Untitled")))
      (insert "\ncreated: "
              ;; Insert creation time, making it match a tempus currens filename
              (format-time-string
               "%Y-%m-%d %a %H:%M"
               (let ((today (format-time-string "%Y%m%d")))
                 (if (and (eq :tempus (ezeka-type buffer-file-name))
                          (not (string-match-p (regexp-quote today) file))
                          (not
                           (when (called-interactively-p 'any)
                             (y-or-n-p "Past tempus currens; set created time to now? "))))
                     (ezeka-encode-iso8601-datetime file)
                   nil)))
              "\n")                     ; i.e. current time
      (when parent
        (insert "parent: " parent "\n"))
      (insert "\n"))))

(defun ezeka-incorporate-file (file kasten &optional arg)
  "Moves the file in the current buffer to the appropriate Zettelkasten. With
prefix argument, asks for a different name."
  (interactive (list (buffer-file-name)
                     (completing-read "Zettel kasten: " ezeka-kaesten)
                     current-prefix-arg))
  (rename-file-and-buffer
   (if (not arg)
       (ezeka-link-file
        (ezeka-make-link kasten (file-name-base file)))
     (call-interactively #'rename-file-and-buffer))))

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun ezeka-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current heading to
facilitate refiling."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM" (ezeka-insert-link-with-metadata
                            buffer-file-name :title :after))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (cl-subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun ezeka-org-interactive-tempus ()
  "Inserts a tempus currens link after having the user select the date using
org-mode's interactive `org-time-stamp' command."
  (interactive)
  (let ((datetime
         (with-temp-buffer
           (let ((start (point)))
             (org-time-stamp '(4) t)
             (org-timestamp-format (org-timestamp-from-string
                                    (delete-and-extract-region start (point)))
                                   "%Y%m%dT%H%M")))))
    (insert (ezeka-org-format-link datetime))))

(defun ezeka-dwim-with-this-timestring (beg end)
  "Do What I Mean with the timestring in the region. If the timestring is
IS8601, make it into an org-time-stamp, and vice-versa. If it's something
else, try to make it into org-time-stamp."
  (interactive
   (cond ((org-at-timestamp-p t)
          (list (match-beginning 0) (match-end 0)))
         ((or (thing-at-point-looking-at ezeka-regexp-iso8601-datetime)
              (thing-at-point-looking-at ezeka-regexp-iso8601-date))
          (list (match-beginning 0) (match-end 0)))
         ((region-active-p)
          (list (region-beginning) (region-end)))
         ((user-error "Region not active"))))
  (let ((text (buffer-substring-no-properties beg end))
        timestamp)
    (cond ((iso8601-valid-p text)       ; ISO-8601 -> Org timestamp
           (let ((parsed (iso8601-parse text)))
             (delete-region beg end)
             (org-insert-time-stamp (iso8601--encode-time parsed)
                                    (integerp (car parsed)) t)))
          ((setq timestamp              ; org timestamp -> ISO-8601
             (org-timestamp-from-string (if (string-match-p "[[<].*[]>]" text)
                                            text
                                          (format "[%s]" text))))
           (delete-region beg end)
           (insert
            (org-timestamp-format timestamp "[[%Y%m%dT%H%M]]")))
          ((integerp (car (parse-time-string text))) ; otherwise -> org timestamp
           (delete-region beg end)
           (org-insert-time-stamp (encode-time (parse-time-string text)) t t))
          (t
           (signal 'wrong-type-argument (list text))))))

(defun ezeka-org-export-as-new-ezeka ()
  "Creates a new Zettel file in the current Zettelkasten based on the current
org subtree."
  (interactive)
  (let ((parent-file buffer-file-name)
        (parent-link (ezeka-file-link buffer-file-name))
        tempus-currens
        new-title
        new-file)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (let ((title (nth 4 (org-heading-components)))
              timestamp)
          (cond ((string-match "\\(.*\\) \\([[<].*[]>]\\)" title)
                 (list (match-string 1 title) (match-string 2 title))
                 (setq timestamp (save-match-data (org-timestamp-from-string
                                                   (match-string 2 title)))
                       new-title (match-string 1 title)))
                ((org-get-scheduled-time nil)
                 (setq timestamp
                   (org-timestamp-from-time (org-get-scheduled-time nil) t)))
                (t
                 (error "Could not get the timestamp for new Zettel")))
          (setq tempus-currens (org-timestamp-format timestamp "%Y%m%dT%H%M")
                new-file (ezeka-link-file tempus-currens))
          (let* ((content (org-get-entry)))
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              ;; New file buffer
              (with-current-buffer (get-buffer-create new-file)
                (ezeka-insert-metadata-template new-link
                                                (completing-read "Category: "
                                                                 ezeka-categories
                                                                 nil
                                                                 nil
                                                                 "Memo")
                                                new-title parent-link)
                (insert "\n" content)
                (save-buffer)))))))
    ;; Back in original buffer
    (with-current-buffer (get-file-buffer (file-truename parent-file))
      (org-cut-subtree)
      (insert (ezeka-org-format-link (ezeka-file-link new-file))
              " "
              (alist-get :title (ezeka-file-metadata new-file))))))

(defun ezeka-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link. With a
prefix argument, ignore `ezeka-number-of-frames' and open the link in the
same window."
  (interactive "P")
  (when (ezeka-link-at-point-p t)
    ;; FIXME: Is it okay to check like this for prefix arg "upstream"?
    (ezeka-find-link (ezeka-link-at-point) (or arg current-prefix-arg))
    ;; This function is later added to `org-open-at-point-functions', so "must
    ;; return t if they identify and follow a link at point. If they don’t find
    ;; anything interesting at point, they must return nil."
    t))

(defun ezeka-open-link-at-mouse (ev)
  "Open a Zettel link at mouse point."
  (interactive "e")
  (mouse-set-point ev)
  (ezeka-open-link-at-point t))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'ezeka-find-link org-open-link-functions)

     ;; Do the same for Zettel links that lack even the link markup. This is
     ;; useful for following parents/children.
     (push #'ezeka-open-link-at-point org-open-at-point-functions)

     ;; This allows following links as part of #+INCLUDE statements.
     ;; TODO: Add a function to follow #+INCLUDE links
     ))

;;;-----------------------------------------------------------------------------
;;; Org-Mode: Inserting Snippets
;;;-----------------------------------------------------------------------------

(defcustom ezeka-insert-snippet-summary nil
  "Non-nil means insert the snippet summary."
  :type 'boolean)

(defcustom ezeka-insert-snippet-footnotes nil
  "Non-nil means insert footnotes."
  :type 'boolean)

;;; TODO:
;;; - implement some kind of checksum check for keeping draft up to date
;;; - if region is active, narrow to it rather than to subtree (allows # lines!)
;;; - don't copy subtrees marked with COMMENT
;;; - update the snippet title in the heading while I'm at it
;;; - command to edit the current heading in situ and locate same text point
;;; - quickly scan through all the headings and see if any need updating?
;;; - add marker that I'm including text from the Zettel; define a new org block?
(defun ezeka-insert-snippet-text (arg file)
  "Inserts the combination of Summary and Snippet sections from the given
snippet FILE into the current buffer. With prefix argument, forces update."
  (interactive
   ;; Assume the file is the last link on the current line
   (list current-prefix-arg
         (save-excursion
           (end-of-line)
           (org-previous-link)
           (when (ezeka-link-at-point-p)
             (ezeka-link-file (ezeka-link-at-point))))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((metadata (ezeka-file-metadata file))
             (modified (format "[%s]" (or (alist-get :modified metadata)
                                          (alist-get :created metadata))))
             current?)
        ;; Update the timestamp if modification time is more recent
        (end-of-line)
        (if (org-at-timestamp-p 'inactive)
            (if (string= (org-element-property :raw-value (org-element-context))
                         modified)
                (setq current? t)       ; we still have most recent text
              ;; Need to repeat `org-at-timestamp-p' for match data
              (when (org-at-timestamp-p 'inactive)
                (replace-match modified)))
          (just-one-space)
          (insert modified))
        (if (and current? (null arg))
            (message "Snippet is up to date; leaving alone")
          (when (y-or-n-p "Update the text? ")
            ;; If current line is a comment, create a heading after it
            (when (org-at-comment-p)
              (org-insert-subheading nil))
            ;; Delete existing text
            (org-narrow-to-subtree)
            (forward-line 1)
            (let ((start (point))
                  (comments-removed 0)
                  (footnotes-removed 0)
                  (content '()))
              (delete-region start (point-max))
              ;; Get the Summary and Snippet subtrees from snippet file
              (with-current-buffer (find-file-noselect file)
                ;; Include Summary section if present
                (when (and ezeka-insert-snippet-summary
                           (org-find-exact-headline-in-buffer "Summary"))
                  (goto-char (org-find-exact-headline-in-buffer "Summary"))
                  (forward-line)
                  (let ((copy-from (point)))
                    (org-end-of-subtree)
                    (mapcar (lambda (line)
                              (push (concat "# " line "\n") content))
                            (split-string
                             (buffer-substring-no-properties copy-from (point))
                             "\n" t))))
                (goto-char (or (org-find-exact-headline-in-buffer "Snippet")
                               (org-find-exact-headline-in-buffer "Content")
                               (error "Can't find the Snippet or Content section")))
                (let ((copy-from (point)))
                  (org-end-of-subtree)
                  (push (buffer-substring copy-from (point)) content)))
              ;; Insert the copied subtrees and remove its headings and comments
              (insert "\n")
              (apply #'insert (nreverse content))
              (goto-char start)
              (while (re-search-forward "^[*]+ " nil t) ; remove headings
                (goto-char (match-beginning 0))
                (kill-line 1))
              ;; Remove my notes in {...} and Zettel links
              (goto-char start)
              (while (re-search-forward (rx (optional blank)
                                            (group
                                             (or (and "[[" (+? (not space)) "]]")
                                                 (and "{" (+? anything) "}"))))
                                        nil t)
                (when (eq (elt (match-string 1) 0) ?{)
                  (incf comments-removed))
                (replace-match ""))
              ;; Remove footnotes if need be
              (unless ezeka-insert-snippet-footnotes
                (goto-char start)
                (while (re-search-forward "^\\[fn:.+?\\].*?$" nil t)
                  (goto-char (match-beginning 0))
                  (kill-paragraph 1)
                  (incf footnotes-removed)))
              (org-indent-region (point-min) (point-max))
              (goto-char (point-max))
              (insert "\n")
              (rb-collapse-blank-lines)
              (message "Removed %d comments and %d footnotes"
                       comments-removed footnotes-removed)
              t)))))))

(defun ezeka-find-inserted-snippet ()
  "While the point is within the org entry, find the source of the snippet
inserted through `ezeka-insert-snippet-text'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-next-link)
    (org-open-at-point)))

(defun ezeka-transclude-snippet (link)
  "Inserts the transclusion statement from given snippet LINKE into the
current buffer."
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
         (format "%s [[%s]]" (alist-get :title metadata) link))
        ;; Delete existing text
        (org-back-to-heading t)
        (delete-region (point-at-bol 2) (org-end-of-subtree t))
        ;; Insert the transclusion line
        (insert (format "\n#+transclude: [[%s::begin]] :lines 2- :end \"end\""
                        (file-relative-name file)))))))

(defcustom ezeka-snippet-heading "Snippet"
  "The text of the snippet heading."
  :type 'string)

(defun ezeka-org-footnote-action-maybe-local (&optional arg)
  "This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets, making sure that the footnotes are placed locally
rather in whatever `org-footnote-section' is set to."
  (interactive "P")
  (let (snippet?)
    (save-excursion
      (org-back-to-heading-or-point-min t)
      (setq snippet?
        (and (org-context)
             (string-equal (nth 4 (org-heading-components)) ezeka-snippet-heading))))
    (if (not snippet?)
        (org-footnote-action arg)
      (let ((org-footnote-section nil)
            (context (org-context)))
        (org-element-cache-reset)
        ;; taken from `org-footnote-action'
        (if (not (and context (> (point)
	                             (save-excursion
		                           (goto-char (org-element-property :end context))
		                           (skip-chars-backward " \t")
		                           (point)))))
            (org-footnote-action arg)
          (kill-new (format-time-string "%H%M"))
          (org-footnote-new))))))

;;;=============================================================================
;;; Bookmark Integration
;;;=============================================================================

(defun bookmark-make-record-ezeka ()
  "Bookmark record function for Zettel bookmarks, setting the
bookmark's filename property to the Zettel link."
  (list (cons 'filename (ezeka-file-link buffer-file-name))
        (cons 'handler 'bookmark-ezeka-handler)))

(defun bookmark-ezeka-handler (bmk-record)
  "Bookmark record handler for Zettel bookmarks."
  (find-file (ezeka-link-file (cdr (assoc 'filename bmk-record)))))

;; Use the special ezeka bookmark handler in Zettel buffers
(add-hook 'ezeka-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-ezeka)))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defun ezeka-zmove-to-another-kasten (source-file &optional kasten target-link)
  "Generates a zmove shell command to move the current Zettel to another
kasten. With prefix argument, asks for a target link instead. Opens the
target link and returns it."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (completing-read "Which kasten to move to? " ezeka-kaesten)))
  (let ((source-link (ezeka-file-link source-file)))
    (if (not target-link)
        (if (equal current-prefix-arg '(4))
            (read-string "Enter target link: ")
          (setq target-link
            (ezeka-make-link
             kasten
             (cl-case (cadr (assoc kasten ezeka-kaesten #'string=))
               (:numerus (ezeka-next-unused-slug kasten))
               (:tempus (ezeka-tempus-currens-slug-for source-link))
               (t
                (error "Don't know how to handle this"))))))
      (error "Don't know where to move %s" source-link))
    ;; Offer to save buffers, since zmove tries to update links
    (save-some-buffers nil (lambda () (ezeka-note-p buffer-file-name t)))
    (shell-command (format "zmove %s %s" source-link target-link))
    (cond ((string= source-file buffer-file-name)
           (kill-this-buffer)
           (unless (eq (length (frame-list)) 1)
             (delete-frame)))
          ((eq major-mode 'magit-status-mode)
           (magit-refresh)))
    (ezeka-find-link target-link t)))

(defun ezeka-generate-n-new-slugs (how-many type)
  "Generates a bunch of new slugs, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (intern (completing-read "Which type? "
                           (mapcar #'first ezeka-default-kasten)))))
  (goto-char (point-max))
  (let (slugs)
    (dotimes (n how-many)
      (push (ezeka-generate-new-slug type) slugs))
    (mapc (lambda (s)
            (insert s "\n"))
          (delete-dups slugs))
    (delete-duplicate-lines (point-min) (point-max))))

;; Based on https://stackoverflow.com/a/30328255
(defun magit-show-ezeka-title-in-minibuffer ()
  "Displays Zettel title of the file under cursor in minibuffer."
  (while-no-input
    (redisplay)
    (let (file line)
      (when (and (eq major-mode 'magit-status-mode)
                 (setq file (magit-file-at-point))
                 (ezeka-note-p file)
                 (setq line (magit-file-line file)))
        (let ((metadata (ezeka-decode-combined-title
                         (car (split-string line "title: " t)))))
          (message "%s | %s"
                   (alist-get :slug metadata) (alist-get :title metadata)))))))
(add-hook 'post-command-hook 'magit-show-ezeka-title-in-minibuffer)

;;;=============================================================================
;;; Mode
;;;=============================================================================

;; Define a minor mode for working with Zettel files
(define-minor-mode ezeka-mode
  "Make the keymap ezeka-mode-map active."
  :lighter " Zettel"
  :keymap
  (mapcar (lambda (tuple)
            (cons (if (stringp (car tuple)) (kbd (car tuple)) (car tuple))
                  (cdr tuple)))
          ;;------------------------------------------------------------------
          ;; According to key binding conventions, the only bindings reserved
          ;; for minor modes are "Sequences consisting of C-c followed by any
          ;; other punctuation character" than {, }, <, >, : or ; that are
          ;; reserved for major modes, leaving the following:
          ;;
          ;; ` ~ ! @ # $ % ^ & * ( ) - _ = + [ ] | \ ' " , . / ?
          ;;------------------------------------------------------------------
          '(("C-c ^" . ezeka-find-ancestor)
            ("C-c _" . ezeka-find-descendant)
            ("C-c @" . ezeka-insert-ancestor-link)
            ("C-c ," . ezeka-insert-new-child)
            ("C-c /" . ezeka-kill-ring-save-link-title)
            ("C-c #" . ezeka-kill-ring-save-link)
            ("C-c $" . ezeka-kill-ring-save-link-at-point)
            ("C-c +" . ezeka-dwim-with-this-timestring)
            ;; Shadows `org-agenda-file-to-front'
            ("C-c [" . ezeka-update-title)
            ;; Shadows prefix map for `orgtbl-ascii-plot' and `org-plit/gnuplot'
            ("C-c \"" . ezeka-avy-link-search)
            ;; Shadows `org-open-at-mouse', but allows opening in same window with C-u
            ([C-down-mouse-1] . ezeka-open-link-at-mouse)
            ;;
            ;; Unsafe: reserved for major modes
            ;;
            ("C-c C-'" . ezeka-set-category)
            ;; Shadows `org-schedule'
            ("C-c C-s" . ezeka-select-and-find-link)
            ;; Shadows `org-deadline'
            ("C-c C-d" . ezeka-kill-ring-save-link-title)
            ;; Shadows `kill-sexp' in global-map
            ("C-M-k" . ezeka-kill-link-or-sexp-at-point)
            ;; Shadows `org-insert-link'
            ("C-c C-M-l" . ezeka-insert-link-from-clipboard)
            ;; Shadows `org-ctrl-c-tab'
            ;;("C-c C-i" . 'ezeka-org-include-cached-file)
            ;; Shadows `org-set-property-and-value'
            ("C-c C-x F" . ezeka-org-set-todo-properties)
            ("C-c C-x z" . ezeka-zmove-to-another-kasten)
            ;; Shadows org-mode's `org-toggle-ordered-property'
            ("C-c C-x l" . ezeka-links-to)
            )))

;; Enable ezeka-mode for files that match the pattern
(add-hook 'org-mode-hook
  (lambda ()
    (when (ezeka-note-p buffer-file-name)
      (setq org-export-with-broken-links t)
      (ezeka-mode 1))))

;; Treat : (colon) as part of the word, allowing forward/backward-word over full
;; Zettel links.
(add-hook 'ezeka-mode-hook
  (lambda ()
    (modify-syntax-entry ?: "w")))

;; On save, update modificate date
(add-hook 'ezeka-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'ezeka-update-metadata-date nil t)))

(provide 'ezeka)
