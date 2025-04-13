;;; ezeka-meta.el --- Metadata and parsing logic for Ezeka -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-04-12
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

;;;=============================================================================
;;; Metadata: Internal
;;;=============================================================================

(defconst ezeka-metadata-fields
  '((id        :format ?i :hidden t :predicate ezeka-id-valid-p)
    (link      :format ?L :hidden t :predicate ezeka-link-p)
    (path      :format ?P :hidden t)
    (filename  :format ?F :hidden t)
    (rubric    :format ?R)
    (category  :format ?l :hidden t)
    (label     :format ?l :hidden t)
    (caption   :format ?c :hidden t)
    (citekey   :format ?k :hidden t)
    (successor :format ?s :predicate ezeka-id-valid-p)
    (title     :format ?t)
    (subtitle  :format ?T)
    (author    :format ?a)
    (date      :format ?d :hidden t)
    (created   :format ?C :predicate ezeka--timep)
    (modified  :format ?M :predicate ezeka--timep)
    (parent    :format ?p :predicate ezeka-id-valid-p)
    (firstborn :format ?f :predicate ezeka-id-valid-p)
    (oldnames  :format ?o :predicate (ezeka-id-valid-p))
    (readings  :format ?r :predicate listp)
    (keywords  :format ?k :predicate listp))
  "An alist of valid metadata fields and their properties.
The format of each item should be as follows:
    (FIELD [:FORMAT ?<character>] [:HIDDEN T] [:PREDICATE <type>]).

If HIDDEN is T, the field is not added to the note header.
YAML collections are returned as Emacs lists and strings
that look like ISO 8601 or `org-mode' timestamps are
converted to Emacs encoded time values. Everything else is
assumed to be scalars, which result in Emacs strings. If
given, PREDICATE is a function that must be satisfied by the
de-YAMLified value.

The order of items will affect how the metadata is written
into the file header.")

(defun ezeka--metadata-field-format (field)
  "Return the format string (e.g. '%t') for FIELD."
  (string ?% (plist-get (cdr (assq field ezeka-metadata-fields)) :format)))

(defun ezeka--metadata-field-hidden-p (field)
  "Return non-nil if FIELD should is hidden."
  (plist-get (cdr (assq field ezeka-metadata-fields)) :hidden))

(defun ezeka--metadata-field-predicate (field)
  "Return predicate for FIELD."
  (plist-get (cdr (assq field ezeka-metadata-fields)) :predicate))

(defun ezeka--read-metadata-field (&optional prompt default)
  "Read a metadata field from the user after PROMPT.
The result is returned as a symbol. If there is no input,
return DEFAULT."
  (if-let ((field (completing-read (or prompt "Which field? ")
                                   ezeka-metadata-fields
                                   nil
                                   t
                                   nil
                                   nil
                                   default))
           (_ (not (string-empty-p field))))
      (intern-soft field)
    default))

(defun ezeka--parse-citation-key (key)
  "Parse the citation KEY, returning a plist of values."
  (let ((case-fold-search nil))
    (when (string-match (concat "^[@&]*\\(?1:[A-Z][a-z]+\\)"
                                "\\(?:\\(?2:[A-Z][a-z]+\\)"
                                "\\(?3:EtAl\\)*\\)*\\(?4:[0-9-]+\\)*$") key)
      (let ((author1 (match-string 1 key))
            (author2 (match-string 2 key))
            (etal (match-string 3 key))
            (date (match-string 4 key)))
        (list 'author1 author1
              'author2 author2
              'date date
              'authors (cond (etal
                              (format "%s, %s, et al." author1 author2))
                             (author2
                              (format "%s and %s" author1 author2))
                             (t
                              author1)))))))

(ert-deftest ezeka--parse-citation-key ()
  (should (equal '(author1 "Horkheimer" author2 "Adorno" date "1989" authors "Horkheimer and Adorno")
                 (ezeka--parse-citation-key "HorkheimerAdorno1989")))
  (should (equal '(author1 "Chiang" author2 nil date "1998" authors "Chiang")
                 (ezeka--parse-citation-key "&Chiang1998"))))

(defun ezeka--citaton-key-authors (key)
  "Return a human-readable list of authors in citation KEY."
  (plist-get 'authors (ezeka--parse-citation-key key)))

(defun ezeka--citaton-key-date (key)
  "Return the date in citation KEY."
  (plist-get 'date (ezeka--parse-citation-key key)))

(defun ezeka-format-metadata (format-string metadata &optional time-format)
  "Format a string out of FORMAT-STRING and METADATA.
The format control string may contain the following %-sequences:

%a means list of cited authors.
%c means caption (i.e. short title).
%C means creation timestamp.
%F means filename.
%i means ID (e.g. a-1234).
%k means citation key.
%K means kasten.
%l means label (genus or category).
%L means formatted link (using `ezeka--format-link').
%M means modification timestamp.
%p means parent.
%R means rubric.
%s means stable mark (see `ezeka-header-rubric-stable-mark').
%t means title.
%T means subtitle.

See `ezeka-metadata-fields' for the full list of available
metadata.

For time values, use TIME-FORMAT if specified; otherwise,
use `ezeka-long-timestamp-format'."
  (save-match-data
    (let-alist metadata
      (let ((_format-time
             (lambda (time)
               (cond ((ezeka--timep time)
                      (format-time-string
                       (or time-format ezeka-long-timestamp-format)
                       time))
                     ((and (stringp time)
                           (string-match-p ezeka-iso8601-date-regexp time))
                      time)
                     ((null time)
                      "")
                     (t
                      (warn "Unknown time value `%s' in note %s" time .id)
                      "<unknown>")))))
        (string-trim
         (format-spec format-string
                      `((?a . ,(if-let ((ck \.citekey))
                                   (format "%s's " (ezeka--citaton-key-authors ck))
                                 ""))
                        (?c . ,(or \.caption (ezeka--pasteurize-file-name \.title)))
                        (?C . ,(funcall _format-time \.created))
                        (?d . ,(if-let ((ck \.citekey))
                                   (or \.date (ezeka--citaton-key-date ck))
                                 ""))
                        (?F . ,\.filename)
                        (?i . ,\.id)
                        (?k . ,(cond ((or (not \.citekey)
                                          (string-empty-p \.citekey))
                                      "")
                                     ((string-match-p "^[@&]" \.citekey)
                                      \.citekey)
                                     (t
                                      (concat "@" \.citekey))))
                        (?K . ,\.kasten)
                        (?l . ,\.label)
                        (?L . ,(ezeka--format-link \.id))
                        (?M . ,(funcall _format-time \.modified))
                        (?p . ,\.parent)
                        (?R . ,\.rubric)
                        (?s . ,(if \.caption-stable
                                   ezeka-header-rubric-stable-mark
                                 ""))
                        (?t . ,(or \.title \.caption))
                        (?T . ,(or \.subtitle \.title))))))))) ; FIXME HACK

(ert-deftest ezeka-format-metadata ()
  (let* ((file (ezeka-link-file "a-0000"))
         (mdata (ezeka-file-metadata file)))
    (should (string= "a-0000" (ezeka-format-metadata "%i" mdata)))
    (should (string= "2022-08-08 Mon 18:25" (ezeka-format-metadata "%C" mdata)))
    (should (string= (ezeka-format-metadata "%t" mdata)
                     (ezeka-format-metadata "%T" mdata)))
    (should (string= (file-name-base file) (ezeka-format-metadata "%R" mdata)))))

(defun ezeka-metadata (file-or-link &rest values)
  "Create a metadata object from pairs of VALUES.
FILE-OR-LINK identifies the Zettel these metadata belong to.
VALUES should be a list of keyword and value properties
corresponding to metadata fields."
  (declare (indent 1))
  (if (and values (zerop (mod (length values) 2)))
      (let* ((link (if (ezeka-link-p file-or-link)
                       file-or-link
                     (ezeka-file-link file-or-link)))
             (mdata `((link . ,link)
                      (id . ,(ezeka-link-id link))
                      (file . ,(unless (ezeka-link-p file-or-link)
                                 file-or-link))
                      (_metadata . created))))
        (while values
          (push (cons (car values) (cadr values)) mdata)
          (setq values (cddr values)))
        mdata)
    (signal 'wrong-type-argument (list 'key-value-pairs-p values))))

(defun ezeka-file-metadata (file)
  "Return an alist of metadata for FILE."
  (save-match-data
    (if-let* ((file (expand-file-name file ezeka-directory))
              (header (ezeka-file-content file 'just-header))
              (mdata (ezeka--decode-header header file))
              (rubric (ezeka-decode-rubric (file-name-base file))))
        (let-alist mdata
          (setf (alist-get '_metadata mdata)
                'from-file)
          (setf (alist-get 'id mdata)
                (or \.id
                    (ezeka-file-name-id file)
                    (file-name-base file))) ; HACK
          (setf (alist-get 'type mdata)
                (or \.type
                    (ezeka-id-type file)))
          (setf (alist-get 'kasten mdata)
                (or \.kasten
                    (ezeka-file-kasten file)))
          (setf (alist-get 'filename mdata)
                (file-name-base file))
          (setf (alist-get 'path mdata)
                (file-relative-name file ezeka-directory))
          (setf (alist-get 'link mdata)
                (or (ignore-errors (ezeka-file-link file))
                    (ignore-errors (ezeka-make-link kasten id))
                    (ezeka--file-name-part file 'id)))
          (setf (alist-get 'caption mdata)
                (or \.caption
                    (ezeka-file-name-caption file)
                    \.title))
          (setf (alist-get 'title mdata)
                (or \.title
                    \.caption))
          (setf (alist-get 'created mdata)
                (or (ezeka--parse-time-string (alist-get 'id rubric))
                    \.created))
          (setf (alist-get 'rubric mdata)
                (ezeka-encode-rubric mdata))
          mdata)
      (signal 'file-error file))))

(defun ezeka--symlink-metadata (file)
  "Return metadata alist for symlinked FILE."
  (save-match-data
    (when-let ((truename (file-symlink-p file))
               (rubric (ezeka-decode-rubric (file-name-base file))))
      (setf (alist-get 'created rubric)
            (file-attribute-modification-time (file-attributes file)))
      rubric)))

(defmacro ezeka-encode-rubric (metadata &optional stable-mark)
  "Return a string that encodes the given METADATA into the rubric.
The rubric consist of `ezeka-file-name-format', preceded by
`ezeka-header-rubric-stable-mark' if STABLE-MARK is given."
  `(concat (when ,stable-mark ezeka-header-rubric-stable-mark)
           (ezeka-format-metadata ezeka-file-name-format ,metadata)))

;; TODO It would be useful to generate the regexp based on
;; `ezeka-encode-rubric' output, perhaps by passing METADATA consisting
;; of %-sequences from `ezeka-format-metadata'. The fields in
;; `ezeka-metadata-fields' could, then, provide their own particular regexp.
(defmacro ezeka-header-rubric-regexp ()
  "Regular expression for the rubric string as found in the header.

Groups 1-5 see `ezeka-file-name-regexp'.
Group 6 is the stable rubric mark."
  `(concat "\\(?6:" ,ezeka-header-rubric-stable-mark "\\)*"
           ,(ezeka-file-name-regexp)))

(defun ezeka-decode-rubric (rubric)
  "Return alist of metadata from the RUBRIC line.
If cannot decode, return NIL."
  (when (and rubric (string-match (ezeka-header-rubric-regexp) rubric))
    (let ((id           (match-string 1 rubric))
          (kasten       (match-string 2 rubric))
          (label        (match-string 3 rubric))
          (caption      (match-string 4 rubric))
          (stable       (when (match-string 6 rubric) t))
          (citekey      (match-string 5 rubric)))
      `((id . ,id)
        (kasten . ,(when kasten (string-trim kasten)))
        (type . ,(ezeka-id-type id))
        (label . ,(unless (string= "nil" label) ; HACK for new notes
                    (ezeka--validate-label label)))
        (caption . ,(when caption (string-trim caption)))
        (caption-stable . ,stable)
        (citekey . ,(when citekey (string-trim citekey)))
        (rubric . ,rubric)
        (_metadata . from-rubric)))))

(defun ezeka--header-yamlify-key (key)
  "Return a YAML-formatted string name of the KEY symbol."
  (symbol-name key))

;;;=============================================================================
;;; Categories, Genera, Keywords
;;;=============================================================================

(defcustom ezeka-categories nil
  "A list of categories used for tempus currens Zettel."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-genera nil
  "An alist of genera used for numerus currens Zettel.
Each element should be in the form
\(LATIN-LETTER GENUS DESCRIPTION)"
  :type 'list
  :group 'ezeka)

(defcustom ezeka-placeholder-genus nil
  "Genus used in numerus currens Kasten to signify a placeholder.
Such placeholders are symbolic links that are not added to
the version control manifest, making it easier to find them."
  :type 'character
  :group 'ezeka)

(defcustom ezeka-placeholder-marker-format "%R %L"
  "Format of the text added to placeholder target.
See `ezeka-format-metadata' for details. The resulting
string is followed by the timestamp."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-keywords nil
  "A list of frequently-used keywords.
Each element should be a string beginning with #."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-rename-note-keyword "#rename"
  "Keyword to add to notes that need renaming.
Because git does not keep track of renames, significantly changing the
content confuses the default rename detection. It is helpful,
therefore, to split the operations into two commits."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-note-moving-keyword "#moving"
  "Keyword signifying that the note is being moved.
See `ezeka-move-to-another-kasten' for details."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-dynamic-keywords t
  "If non-nil, generate a dynamic list of keywords.
The list is then merged with `ezeka-keywords'."
  :type 'boolean
  :group 'ezeka)

;;;=============================================================================
;;; File Names
;;;=============================================================================
;;;-----------------------------------------------------------------------------
;;; Variables: Metadata and Headers
;;
;; Metadata refers to the information about the Zettel note, like its created or
;; midified time, and so on. Header, on the other hand, is the actual
;; representation of that metadata inside the Zettel note.
;;
;; Rubric is the compressed metadata information (including the caption, which
;; is a pasteurized or shortened title) that serves as the file name for the
;; note. This is included in the rubric for redundancy.
;;;-----------------------------------------------------------------------------

(defcustom ezeka-file-name-format "%i {%l} %c %k"
  "The `format-spec' string for generating a note's file name.
At the minimum, it must start with \"%i\"; everything else
is optional. See `ezeka-format-metadata' for details. This
should match `ezeka-file-name-regexp'."
  :type 'string
  :group 'ezeka)

;; TODO See `ezeka-header-rubric-regexp' about how to unhardcode this.
(defmacro ezeka-file-name-regexp ()     ; HARDCODED
  "Return regexp matching Zettel base file names (i.e. rubrics).
It should match the result of `ezeka-file-name-format`.

Group 1 is the ID.
Group 2 is the kasten.
Group 3 is the label (genus or category).
Group 4 is the caption (i.e. short title).
Group 5 is the citation key.
Group 6 is the stable caption mark."
  `(concat (ezeka-link-regexp)          ; \1 and \2
           ezeka-file-name-separator
           "\\(?:"                      ; everything else is optional
           "\\(?:\\.\\)*"               ; FIXME: optional historic period
           "\\(?:{\\(?3:[^}]+\\)} \\)*" ; \3
           "\\(?4:.+?\\)"               ; \4
           "\\(?: \\(?5:[@&]\\S-+\\)\\)*$" ; \5
           "\\)*"                          ; end of everything else
           ))

(defun ezeka-file-p (file-or-buffer &optional strict)
  "Return non-NIL if the FILE-OR-BUFFER is a Zettel.
It is a Zettel if all of these conditions are met:
1) its extension is `ezeka-file-extension';
2) its filename matches `ezeka-file-name-regexp'; and, if STRICT is non-NIL,
3) the file exists;
4) the file is inside `ezeka-directory'."
  (interactive "f")
  (when-let ((_ file-or-buffer)
             (file (cond ((bufferp file-or-buffer)
                          (buffer-file-name file-or-buffer))
                         ((stringp file-or-buffer)
                          (expand-file-name file-or-buffer))
                         (strict
                          (signal 'wrong-type-argument
                                  (list 'file-or-buffer-p file-or-buffer)))
                         (t nil))))
    (and (string-equal (file-name-extension file) ezeka-file-extension)
         (string-match (concat "^" (ezeka-file-name-regexp) "$")
                       (file-name-base file))
         (or (not strict)
             (and (file-exists-p file)
                  (string-prefix-p ezeka-directory file)))))) ; FIXME: Hack

(defun ezeka-file-name-valid-p (filename)
  "Return non-nil if FILENAME is a valid Zettel filename."
  (save-match-data
    (string-match (ezeka-file-name-regexp) (file-name-base filename))))

;; FIXME: Hardcoded
(defun ezeka--file-name-fields (filename)
  "Parse FILENAME, returning its matched fields an an alist."
  (let* ((base (file-name-base filename)))
    (save-match-data
      (when (string-match (ezeka-file-name-regexp) base)
        (mapcar (lambda (key-val)
                  (cons (car key-val)
                        (when (cdr key-val) (string-trim (cdr key-val)))))
                `((id . ,(match-string 1 base))
                  (label . ,(match-string 3 base))
                  (caption . ,(match-string 4 base))
                  (citekey . ,(match-string 5 base))))))))

(defun ezeka--file-name-part (filename part)
  "Return given PART (id, label, caption, or citekey) of FILENAME."
  (let ((base (file-name-base filename)))
    (save-match-data
      (when-let ((_ (string-match (ezeka-file-name-regexp) base))
                 (match (match-string (pcase part
                                        ('id 1)
                                        ('label 3)
                                        ('caption 4)
                                        ('citekey 5))
                                      base)))
        (string-trim match)))))

(defun ezeka-file-name-id (filename)
  "Return the ID part of the given Zettel FILENAME."
  (ezeka--file-name-part filename 'id))
(defun ezeka-file-name-label (filename)
  "Return the label part of the given Zettel FILENAME."
  (ezeka--file-name-part filename 'label))
(defun ezeka-file-name-caption (filename)
  "Return the caption part of the given Zettel FILENAME."
  (ezeka--file-name-part filename 'caption))
(defun ezeka-file-name-citekey (filename)
  "Return the citekey part of the given Zettel FILENAME."
  (ezeka--file-name-part filename 'citekey))

;; TODO Rewrite using rubric
(defun ezeka-format-file-name (format-string filename)
  "Format FILENAME according to FORMAT-STRING.
Unlike `ezeka-format-metadata', this function relies purely
on the filename.

The format control string can contain the following
%-sequences:

%c means caption (i.e. short title).
%e means file extension.
%f means base filename (i.e. without extension).
%i means ID or link.
%k means citation key.
%K means kasten.
%l means label (genus or category).

Unknown %-sequences are left intact."
  (let-alist (ezeka--file-name-fields filename)
    (let* ((case-fold-search nil)
           (basic (format-spec format-string
                               `((?e . ,(file-name-extension filename))
                                 (?f . ,(file-name-base filename))
                                 (?i . ,.id)
                                 (?l . ,.label)
                                 (?c . ,.caption)
                                 (?K . ,(ezeka-kasten-name
                                         (ezeka-file-kasten filename)))
                                 (?k . ,(or .citekey "")))
                               'ignore))) ; leave unknown %-sequences
      basic)))

;;;=============================================================================
;;; Header
;;;=============================================================================

(defcustom ezeka-header-separator-regexp "^$"
  "Regexp that matches the separator line between header and the note text."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-key "rubric"
  "The header metadata key for the rubric."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-stable-mark "§"
  "Mark that signifies that the caption is stable.
The mark is used in the rubric to show that any differences
between caption and title values should be ignored as long
as filename and rubric match."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-update-modified t
  "Whether `ezeka--update-file-header' updates the modification date.
Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (same as T)."
  :type 'symbol
  :group 'ezeka)

;;;=============================================================================
;;; Header Format
;;;=============================================================================

(defun ezeka--header-yamlify-value (value)
  "Return a YAML-formatted string for the given metadata VALUE."
  (pcase value
    ((pred stringp)
     value)
    ((and (pred ezeka--timep)
          (pred ezeka--full-time-p))
     (ezeka-timestamp value 'full))
    ((pred ezeka--timep)
     (ezeka-timestamp value nil nil 'noweekday))
    ((pred listp)
     (concat "[ "
             (mapconcat #'ezeka--header-yamlify-value
                        value
                        ", ")
             " ]"))
    (_
     (signal 'wrong-type-argument (list (type-of value) value)))))

(defun ezeka--header-deyamlify-value (value)
  "Return an elisp version of the given YAML-formatted VALUE."
  (pcase value
    ;; strip [[ ]] around wiki links
    ((rx bol "[[" (let inside (1+ anychar)) "]]" eol)
     inside)
    ;; convert ISO 8601 and/or `org-mode' timestamps to time values
    ((rx bol
         (* "[")
         (let timestamp
           (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) (* anychar)))
         (* "]")
         eol)
     (ezeka--parse-time-string timestamp))
    ;; remaining [ ] should be lists
    ((rx bol "[ " (let inside (1+ anychar)) " ]" eol)
     (mapcar #'ezeka--header-deyamlify-value
             (split-string inside "," t "[[:space:]]+")))
    (_
     (string-trim value))))

(defvar ezeka-header-line-regexp
  "\\(?1:\\w+\\):\\s-+\\(?2:.*\\)"
  "The regular expression that matches a line of YAML metadata.
Group 1 is the key.
Group 2 is the value.")

(defvar ezeka--validate-metadata-field-history nil
  "History variable for `ezeka--validate-metadata-field'.")

(defun ezeka--validate-metadata-field (field value &optional predicate)
  "Return VALUE if it is valid for metadata FIELD.
PREDICATE, if given, overrides `ezeka-metadata-fields'."
  (let ((pred (or predicate
                  (plist-get (alist-get field ezeka-metadata-fields)
                             :predicate)
                  #'identity)))
    (cond ((and (functionp pred) (funcall pred value))
           value)
          ((functionp pred)
           ;; TODO Any way to save the fixed value?
           (ezeka--validate-metadata-field
            field
            (ezeka--header-deyamlify-value
             (read-string
              (format "Value `%s' for %s doesn't satisfy `%s'; fix it: "
                      value field pred)
              value
              'ezeka--validate-metadata-field-history))
            pred))
          ((listp pred)
           (mapcar (lambda (v)
                     (ezeka--validate-metadata-field field v (car pred)))
                   value))
          (t
           (signal 'wrong-type-argument (cons 'functionp-or-listp pred))))))

(defun ezeka--decode-header-line (line &optional file)
  "Return the (KEY . VALUE) tuple from header LINE of FILE.
If LINE doesn't match `ezeka-header-line-regexp', prompt the
user to fix it."
  (cond ((string-empty-p line) nil)
        ((string-match ezeka-header-line-regexp line)
         (let* ((key (intern (match-string 1 line)))
                (val (match-string 2 line))
                (deval (ezeka--validate-metadata-field
                        key
                        (ezeka--header-deyamlify-value val))))
           (cons key deval)))
        (t
         (let ((edited
                (read-string (format "Malformed header line while reading %s:\n%s\nFix it? "
                                     (if file
                                         (file-name-base file)
                                       "unspecified file")
                                     line)
                             line nil line)))
           (if (equal edited line)
               (signal 'ezeka-error (list "Malformed header line: '%s'" line))
             (ezeka--decode-header-line edited))))))

(defun ezeka--decode-header (header &optional file)
  "Return metadata alist decoded from FILE's HEADER.
They keys are converted to symbols."
  (let* ((metadata (mapcar #'ezeka--decode-header-line
                           (split-string header "\n" 'omit-nulls "[ ]+")))
         (rubric (ezeka-decode-rubric (alist-get 'rubric metadata))))
    (append rubric metadata)))

(defun ezeka--normalize-metadata-timestamps (metadata)
  "Normalize the creation and modification times in METADATA.
The times are modified if either is missing time value (i.e.
just has date) or the time is 00:00. The created time is
taken from tempus currens, if there is a record of one.
Otherwise, use current time. The modification time is set to
current time."
  (let* ((created  (alist-get 'created metadata))
         (modified (alist-get 'modified metadata))
         (tempus (cl-find-if
                  (lambda (id)
                    (eq (ezeka-id-type id) :tempus))
                  (cons (alist-get 'id metadata)
                        (cons (ezeka-file-name-id (alist-get 'filename metadata))
                              (alist-get 'oldnames metadata))))))
    (setf (alist-get 'created metadata)
          (ezeka--complete-time created
                                (if tempus
                                    (ezeka--parse-time-string tempus)
                                  (current-time))))
    (setf (alist-get 'modified metadata)
          (when modified
            (ezeka--complete-time modified)))
    metadata))

(defun ezeka--header-region (buffer)
  "Return a tuple of (START. END) for the header in Ezeka BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (cons (point)
            (if (re-search-forward ezeka-header-separator-regexp nil t)
                (match-end 0)
              (point-max))))))

(defun ezeka-point-at-bob ()
  "Return point at the beginning of the body (BoB)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (if (re-search-forward ezeka-header-separator-regexp nil t)
          (1+ (match-end 0))
        (signal 'ezeka-error "Cannot find where the body begins")))))

;;;=============================================================================
;;; Updating Header
;;;=============================================================================

(defun ezeka-set-metadata-value (metadata field value)
  "Set METADATA's FIELD to VALUE after doing some checking.
Return the original METADATA with the field changed."
  (setf (alist-get field metadata) value)
  metadata)

(defun ezeka--add-oldname (metadata oldname)
  "Add OLDNAME to METADATA, returning the modified metadata."
  (setf (alist-get 'oldnames metadata)
        (remove (alist-get 'id metadata)
                (cl-union (list oldname)
                          (alist-get 'oldnames metadata))))
  metadata)

(defun ezeka-force-save-buffer (&optional update-modified)
  "Save the current buffer, even if it's unmodified.
Unless specified with UPDATE-MODIFIED, respect the value of
`ezeka-update-modified'. If called interactively with \\[universal-argument], however,
don't update the modification date. With \\[universal-argument] \\[universal-argument],
update it unconditionally."
  (interactive
   (let ((prefix (prefix-numeric-value current-prefix-arg)))
     (list (cond ((= prefix 4) 'never)
                 ((= prefix 16) 'always)
                 (t ezeka-header-update-modified)))))
  (let ((ezeka-header-update-modified update-modified)
        (before-save-hook
         (remove 'ezeka--update-file-header before-save-hook))
        (modified (buffer-modified-p)))
    (unwind-protect
        (when (ezeka-file-p buffer-file-name)
          (set-buffer-modified-p t)
          (ezeka--update-file-header nil nil
                                     (eq ezeka-header-update-modified 'always))
          (save-buffer))
      (set-buffer-modified-p modified))
    (set-buffer-modified-p nil)))

(defun ezeka--replace-file-header (filename metadata)
  "Replace FILENAME's file header with METADATA."
  (let ((inhibit-read-only t))
    (save-mark-and-excursion
      (with-current-buffer (find-buffer-visiting filename)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward ezeka-header-separator-regexp nil t 1)
            (narrow-to-region (point-min) (point)))
          (setf (alist-get 'rubric metadata)
                (ezeka-encode-rubric metadata 'stable-mark))
          (delete-region (point-min) (point-max))
          (mapc (lambda (cons)
                  (when cons
                    (insert (format "%s: %s\n"
                                    (ezeka--header-yamlify-key (car cons))
                                    (ezeka--header-yamlify-value (cdr cons))))))
                (mapcar (lambda (field)
                          (when (and (not (plist-get (cdr field) :hidden))
                                     (alist-get (car field) metadata))
                            (cons (car field)
                                  (alist-get (car field) metadata))))
                        ezeka-metadata-fields))
          (ezeka--make-header-read-only (current-buffer)))))))

(defun ezeka--metadata-equal-p (md1 md2)
  "Return non-nil if the values of MD1 and MD2 are equal."
  (and (= (length md1) (length md2))
       (cl-every (lambda (x)
                   (equal (cdr x) (alist-get (car x) md2)))
                 md1)))

(defvar ezeka--previously-updated nil
  "Temporarily save previoius file state to avoid repeated calls.
This is a an alist of (FILENAME . CHECKSUM METADATA). Used in
`ezeka--update-file-header'.")

(defun ezeka--update-file-header (&optional filename metadata force)
  "Replace FILENAME's header with one generated from METADATA.
If METADATA is not given, get it by parsing the FILENAME's existing
header. If FORCE is non-nil, update the header even if the the file
has not changed since last update."
  (interactive (list buffer-file-name nil current-prefix-arg))
  (let* ((fname (or filename buffer-file-name))
         (mdata (or metadata (ezeka-file-metadata fname)))
         (force (or force current-prefix-arg))
         (prev (assoc-string fname ezeka--previously-updated))
         (old-point (point))
         (inhibit-read-only t))
    (cond ((not mdata)
           (signal 'ezeka-error '("Cannot update header: can't parse metadata")))
          ((and (not force)
                (string= (buffer-hash) (cadr prev)))
           (message "The header is unchanged"))
          (t
           (setq mdata
             (ezeka--normalize-metadata-timestamps
              (ezeka--maybe-update-modified
               (ezeka--reconcile-title-and-caption fname mdata force))))
           (ezeka--replace-file-header fname mdata)
           (setf (alist-get fname ezeka--previously-updated nil nil #'string=)
                 (list (buffer-hash) mdata))))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

;;;=============================================================================
;;; Header Rendering
;;;=============================================================================

(defcustom ezeka-header-read-only t
  "When non-nil, the Ezeka file headers are made read-only."
  :type 'boolean
  :group 'ezeka)
;; The following is adapted from
;; https://emacs.stackexchange.com/a/46059
(defface ezeka-read-only '((t :slant italic))
  "Face for `ezeka--read-only-region'."
  :group 'ezeka)

(defun ezeka--read-only-region (begin end)
  "Make the marked region between BEGIN and END read-only.
The text property is nonsticky in the front and the rear.
See also `ezeka--writeable-region'.

Read-only text is given the face `ezeka-read-only'."
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties begin end
                           ;; If the ‘rear-nonsticky’ property is a list,
                           ;; properties are rear-sticky _unless_ their names
                           ;; are in the list.
                           '(read-only t rear-nonsticky (read-only)))
      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'ezeka-text-type 'read-only)
        (overlay-put overlay 'face 'ezeka-read-only)))))

(defun ezeka--writeable-region (begin end)
  "Make the marked region between BEGIN and END writeable.
See also `ezeka--read-only-region'."
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties begin end '(read-only t))
      (remove-overlays begin end 'ezeka-text-type 'read-only))))

(defun ezeka--make-header-read-only (buffer)
  "Make the header in the Zettel BUFFER read-only."
  (when ezeka-header-read-only
    (let ((beg-end (ezeka--header-region buffer)))
      (with-current-buffer buffer
        (ezeka--read-only-region (car beg-end) (cdr beg-end))))))

(defun ezeka-toggle-header-read-only ()
  "Toggle header being read-only in the current Zettel buffer."
  (interactive)
  (let ((beg-end (ezeka--header-region (current-buffer))))
    (save-excursion
      (if (cl-find-if (lambda (ol)
                        (overlay-get ol 'ezeka-text-type))
                      (overlays-at (car beg-end)))
          (ezeka--writeable-region (car beg-end) (cdr beg-end))
        (ezeka--read-only-region (car beg-end) (cdr beg-end))))))

;;;=============================================================================
;;; Updating Modified Line
;;;=============================================================================

(defun ezeka-toggle-update-header-modified (arg)
  "Toggle between different value of `ezeka-header-update-modified'.
With \\[universal-argument] ARG, show a list of options instead."
  (interactive "P")
  (let ((new-value
         (if arg
             (intern (completing-read
                      "When to update modification dates: "
                      '("sameday" "never" "confirm")
                      nil
                      t))
           (if-let ((result (member ezeka-header-update-modified
                                    '(#1=sameday never confirm #1#))))
               (cadr result)
             (user-error "Invalid current `ezeka-header-update-modified': %s"
                         ezeka-header-update-modified)))))
    (setq ezeka-header-update-modified new-value)
    (unless arg
      (message "Set `ezeka-header-update-modified' to %s" new-value))))

(defcustom ezeka-modified-updated-hook nil
  "List of functions to call after modifying the metadata header."
  :type '(or function list)
  :group 'ezeka)

(defun ezeka--maybe-update-modified (metadata)
  "Maybe update the modification time in the METADATA.
Whether to update is determined by `ezeka-update-modifaction-date'.
Return the new metadata."
  (let* ((mdata (ezeka--normalize-metadata-timestamps metadata))
         (now (current-time))
         (last-modified (or (alist-get 'modified mdata)
                            (alist-get 'created mdata)
                            (user-error "No created or modified time in %s"
                                        (alist-get 'link mdata))))
         (elapsed (- (time-to-seconds now) (time-to-seconds last-modified))))
    (unless (<= elapsed 60)             ; less than a minute
      (when (or (eq ezeka-header-update-modified 'always)
                (and (eq ezeka-header-update-modified 'sameday)
                     (= (time-to-days last-modified)
                        (time-to-days (current-time))))
                ;; Automatic updating conditions not met; need to confirm
                (and (member ezeka-header-update-modified '(sameday confirm t))
                     (y-or-n-p
                      (format "%s was last modified at %s. Update to now? "
                              (ezeka-format-metadata "%i {%l} %t" mdata)
                              (ezeka-timestamp last-modified 'full 'brackets)))))
        (setf (alist-get 'modified mdata) now)
        (ezeka--add-to-system-log 'update-modified nil
          'note (ezeka-encode-rubric metadata))
        (run-hooks 'ezeka-modified-updated-hook)))
    mdata))

(defun ezeka-update-modified (file)
  "Update the modification time in the current Zettel FILE's header.
This function ignores the value of `ezeka-header-update-modified',
treating it as if set to 'ALWAYS."
  (interactive (list buffer-file-name))
  (let ((ezeka-header-update-modified 'always))
    (ezeka--update-file-header file nil t)))

;;;=============================================================================
;;; Reconciling Filename, Caption, Title
;;;=============================================================================

;; There are three different places where files can be captioned or titled:
;; 1) The file name itself might contain a caption;
;; 2) The rubric in the file header should contain caption matching the
;;    caption in the file name; and, finally,
;; 3) The title in the file header contains a nicely formatted version
;;    of the caption that is used when inserting links.

(defun ezeka--reconcile-title-and-caption (file &optional metadata force)
  "Interactively reconcile title and caption in FILE's METADATA.
Returns modifed metadata. If FORCE is non-nil, attempt
reconciling even if CAPTION-STABLE is true."
  (let* ((mdata (or metadata (ezeka-file-metadata file)))
         (caption (or (alist-get 'caption mdata) ""))
         (title (or (alist-get 'title mdata)
                    (alist-get 'rubric mdata)))
         (_capitalp (lambda (c) (and (= ?w (char-syntax c)) (= c (upcase c))))))
    (unless (or (string= (ezeka--pasteurize-file-name title) caption)
                (and (not force)
                     (alist-get 'caption-stable mdata)))
      (let ((choice (read-char-choice
                     (format
                      (concat
                       "Caption: %s\n"
                       "  Title: %s\n"
                       "Press [c/u] to edit caption, [t/l] to edit title;\n"
                       "      [C/U] and [T/L] to use that one for the other;\n"
                       "      [n] or [q] to leave alone as is.")
                      (propertize caption 'face :bold)
                      (propertize title 'face :italic))
                     '(?c ?u ?t ?l ?C ?U ?T ?L ?n ?q))))
        (pcase choice
          ((or ?c ?u ?T ?L) (setf (alist-get 'caption mdata)
                                  (ezeka--minibuffer-edit-string
                                   (ezeka--pasteurize-file-name
                                    (if (funcall _capitalp choice)
                                        title
                                      caption)))))
          ((or ?t ?l ?C ?U) (setf (alist-get 'title mdata)
                                  (ezeka--minibuffer-edit-string
                                   (if (funcall _capitalp choice)
                                       (ezeka--depasturize-for-title caption)
                                     title))))))
      (setf (alist-get 'caption-stable mdata) t))
    (funcall clear-message-function)
    mdata))

(defvar ezeka--unnormalized-files-to-move nil
  "An alist of file names to rename with the caption inside metadata.
Each element should consist of the key (full file name), what it
should be renamed to (base file name), and current timestamp.")

(defun ezeka-rename-unnormalized-files (&optional confirm)
  "Rename files in `ezeka--unnormalized-files-to-move'.
If CONFIRM (\\[universal-argument]) is non-nil, confirm each rename."
  (interactive "P")
  (mapc
   (lambda (entry)
     (cl-destructuring-bind (filename caption time)
         entry
       (let ((renamed (expand-file-name
                       (file-name-with-extension caption
                                                 ezeka-file-extension)
                       (file-name-directory filename))))
         (cond ((file-exists-p renamed)
                (unless (y-or-n-p (format "File `%s' already exists. Skip? "
                                          caption))
                  (signal 'ezeka-error (list "File `%s' already exists" caption))))
               ((and (file-exists-p filename)
                     (or (not confirm)
                         (y-or-n-p
                          (format "Move [%s]\n  to [%s]\n(saved on %s)? "
                                  (file-name-base filename)
                                  caption
                                  (format-time-string "%F" time)))))
                (ezeka--rename-file filename renamed)))
         (if (file-exists-p renamed)
             (when (y-or-n-p "Success! Remove entry? ")
               (cl-delete filename ezeka--unnormalized-files-to-move
                          :test #'string= :key #'car))
           (message "Rename failed")))))
   ezeka--unnormalized-files-to-move))

(defvar ezeka--marked-for-rename nil
  "List of files that are marked for rename.")

(defun ezeka--mark-for-rename (filename &optional metadata)
  "Mark FILENAME (with METADATA) for rename."
  (interactive (list buffer-file-name))
  (let ((mdata (or metadata (ezeka-file-metadata filename))))
    (let-alist mdata
      (ezeka-add-keyword filename ezeka-rename-note-keyword nil mdata)
      (when (y-or-n-p "Create a symbolic link meanwhile? ")
        (ezeka--make-symbolic-link filename (ezeka-link-path \.id mdata)))
      (cl-pushnew (ezeka-file-name-id filename)
                  ezeka--marked-for-rename
                  :test #'string=))))

(defun ezeka--marked-for-rename-p (filename)
  "Return non-nil if FILENAME is marked for rename."
  (string-match-p ezeka-rename-note-keyword
                  (ezeka-file-content filename 'just-header)))

(defcustom ezeka-harmonize-file-name-preference 'ask
  "Can be 'ASK, 'FILENAME, or 'METADATA."
  :type 'symbol
  :options '(ask filename metadata)
  :group 'ezeka)

(defun ezeka--harmonize-file-name-choose (file-name rubric-name)
  "Prompt the user whether to use FILE-NAME or RUBRIC-NAME.
Return one of the options for `ezeka-harmonize-file-name-preference'."
  (let ((choice
         (read-char-choice
          (format (concat "Caption in filename and metadata differ:\n"
                          "[F]ilename: %s\n"
                          "[M]etadata: %s\n"
                          "Press [f/u] to set metadata from filename (uppercase to edit),\n"
                          "      [m/l] to set filename from metadata (uppercase to edit),\n"
                          "      [r] to add %s keyword for renaming later, or\n"
                          "      [n] or [q] to do noting:")
                  (propertize file-name 'face 'bold)
                  (propertize rubric-name 'face 'italic)
                  (propertize ezeka-rename-note-keyword 'face 'bold))
          '(?f ?F ?u ?U ?m ?M ?l ?L ?r ?R ?n ?N ?q ?Q))))
    (cond ((memq choice '(nil ?n ?q))
           'do-nothing)
          ((memq choice '(?r ?R))
           'mark-for-rename)
          ((memq choice '(?f ?F ?u ?U))
           'filename)
          ((memq choice '(?m ?M ?l ?L))
           'metadata))))

(defun ezeka-harmonize-file-name (&optional filename metadata force preference)
  "Ensure that FILENAME's captioned name matches the METADATA.
When called interactively with \\[universal-argument], or
FORCE is non-nil, offer to set metadata or rename the file
even if they are in agreement. If given, PREFERENCE
overrides `ezeka-harmonize-file-name-preference'."
  (interactive
   (list buffer-file-name
         nil
         current-prefix-arg
         'ask))
  (let* ((filename (or (and filename (file-truename filename))
                       buffer-file-name))
         (file-base (file-name-base filename))
         (mdata (if (null metadata)
                    (ezeka-file-metadata filename)
                  (ezeka--update-file-header filename metadata)
                  metadata))
         (mdata-base (ezeka-format-metadata ezeka-file-name-format mdata))
         (preference (or preference ezeka-harmonize-file-name-preference))
         (keep-which
          (cond ((eq preference 'metadata) ?m)
                ((eq preference 'filename) ?f)
                ((or force
                     (and (not (string= (ezeka--unaccent-string file-base)
                                        (ezeka--unaccent-string mdata-base)))
                          (not (member ezeka-rename-note-keyword
                                       (alist-get 'keywords mdata)))))
                 (ezeka--harmonize-file-name-choose file-base mdata-base)))))
    (cond (;; mark for rename
           (and (eq keep-which 'mark-for-rename)
                (not (member ezeka-rename-note-keyword
                             (alist-get 'keywords mdata))))
           (ezeka--mark-for-rename filename mdata)
           (ezeka--save-buffer-read-only filename))
          ;; set from filename
          ((eq keep-which 'filename)
           (when (memq keep-which '(?F ?U))
             (setq file-base (ezeka--minibuffer-edit-string file-base)))
           (setf (alist-get 'id mdata)
                 (ezeka-file-name-id file-base)
                 (alist-get 'label mdata)
                 (ezeka-file-name-label file-base)
                 (alist-get 'caption mdata)
                 (ezeka-file-name-caption file-base)
                 (alist-get 'citekey mdata)
                 (ezeka-file-name-citekey file-base)
                 (alist-get 'caption-stable mdata)
                 nil)
           (ezeka--replace-file-header filename mdata)
           (ezeka--save-buffer-read-only filename)
           (run-hooks 'ezeka-after-save-hook))
          ;; set from metadata
          ((eq keep-which 'metadata)
           (let ((pasteurized
                  (ezeka--pasteurize-file-name
                   (ezeka-format-metadata ezeka-file-name-format mdata))))
             (setf (alist-get 'keywords mdata)
                   (cl-remove ezeka-rename-note-keyword
                              (alist-get 'keywords mdata)
                              :test #'string=)
                   (alist-get 'caption mdata)
                   pasteurized)
             (ezeka--rename-file filename
                                 (file-name-with-extension
                                  (ezeka--minibuffer-edit-string pasteurized)
                                  ezeka-file-extension))
             (run-hooks 'ezeka-after-save-hook)
             (ezeka--git-stage-file filename)
             (when t                    ; TODO check if filename changed
               (message "You might want to do `ezeka-harmonize-file-name' again")))))))

(provide 'ezeka-meta)
;;; ezeka-meta.el ends here
