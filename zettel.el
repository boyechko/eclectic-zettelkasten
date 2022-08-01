;;;; -*- mode: emacs-lisp -*-
;;;;-----------------------------------------------------------------------------
;;;;        Author: Richard Boyechko <rb-emacs@diachronic.net>
;;;;   Description: Zettelkasten implementation based on Deft
;;;;  Date Created: 2015-06-31
;;;;      Comments:
;;;;-----------------------------------------------------------------------------

(require 'deft)

;;;=============================================================================
;;; Mode
;;;=============================================================================

;; Cretea a keymap for the mode
(defvar zettel-mode-map (make-sparse-keymap)
  "Keymap for Zettel mode.")

;; Define a minor mode for working with Zettelkasten in deft
(define-minor-mode zettel-mode
  "Make the keymap zettel-mode-map active."
  :lighter " Zettel"
  :keymap zettel-mode-map
  :require 'deft)

;; Enable zettel-mode for files that match the pattern
(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook
    '(lambda ()
       (when (zettel-p buffer-file-name)
         (zettel-mode 1)))))
(add-hook 'org-mode-hook
  '(lambda ()
     (when (zettel-p buffer-file-name)
       (setq org-export-with-broken-links t)
       (zettel-mode 1))))

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defun in-zettel-dir (&optional relative-path)
  "Returns absolute pathname of the given pathspec relative to
the Zettel directory."
  (expand-file-name (or relative-path "") zettel-directory))

;; FIXME: temporary
(defvar zettel-regexp-bolus-currens
  "\\([0-9]\\{3\\}\\)-\\([a-z]\\{3\\}\\)"
  "The regular expression that matches bolus currens like abc-123.")

(defvar zettel-regexp-numerus-currens
  "\\([a-z]\\)-\\([0-9]\\{4\\}\\)"
  "The regular expression that matches numerus currens like d-0503.")

(defvar zettel-regexp-tempus-currens
  "\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)T\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "The regular expression that matches the basic (but not extended) ISO 8601
timestamp.
Groups 1-3 are year, month, day.
Groups 4-5 are hour, minute.")

;; FIXME: Is this or the individually-named variables redundant?
(defvar zettel-type-regexp-alist
  `((:bolus   . ,zettel-regexp-bolus-currens) ; FIXME: temporary
    (:numerus . ,zettel-regexp-numerus-currens)
    (:tempus  . ,zettel-regexp-tempus-currens))
  "An alist of type and its regular expressions for the various slug types.")

(defvar zettel-type-example-alist
  '((:bolus   . "123-abc") ; FIXME: temporary
    (:numerus . "a-1234")
    (:tempus  . "20210123T1234"))
  "An alist of type and an example of what it looks like for the various slug
types.")

(defvar zettel-regexp-slug
  ;; Strip the groups in the component regexps
  (concat "\\("
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-numerus-currens)
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-tempus-currens)
          ;; FIXME: temporary
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-bolus-currens)
          "\\)")
  "A generalized regexp that matches any slug, whatever its slug type.")

(defvar zettel-regexp-link
  (concat "\\(\\([[:alpha:]]+\\):\\)*" zettel-regexp-slug)
  "The regular expression that matches Zettel links.
Group 2 is the kasten, if specified.
Group 3 is the slug.")

(defvar zettel-regexp-iso8601-date
  "\\<\\([0-9]\\{4\\}\\)-*\\([0-9]\\{2\\}\\)-*\\([0-9]\\{2\\}\\)"
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar zettel-regexp-iso8601-time
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar zettel-regexp-iso8601-datetime
  (concat zettel-regexp-iso8601-date zettel-regexp-iso8601-time)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defvar zettel-deft-active-kasten nil
  "The name of the active Zettelkasten, if any. This variable is set by
`zettel-deft-choose-kasten'.")

(defvar zettel-pregenerated-numeri "auto/unused-numeri.dat"
  "List of unused numri curentes to use for creating new numerus currens
Zettel in rumen when Emacs cannot check the list of existing files.")

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom zettel-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'zettel)

(defcustom zettel-kaesten
  ;; name | directory | slug type | sort-order (| deft-sort-method)
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
  :group 'zettel)

(defcustom zettel-kaesten-aliases nil
  "An alist of any other aliases for the `zettel-kaesten'. This is an alist of
the actual name followed by the alias."
  :type 'alist
  :group 'zettel)

(defcustom zettel-default-kasten
  ;; slug type | kasten name
  `((:numerus . "rumen")
    (:bolus . "esophagus")              ; FIXME: temporary
    (:tempus . "omasum"))
  "An alist of default Kasten (i.e. not requiring fully qualified link) for
each slug type."
  :type 'alist
  :group 'zettel)

(defcustom zettel-categories nil
  "A list of categories used for Zettel."
  :type 'list
  :group 'zettel)

(defcustom zettel-number-of-frames nil
  "Try to use only this many frames. Nil means single frame."
  :type 'symbol
  :options '(one two many)
  :group 'zettel)

(defcustom zettel-sort-by-name-descending t
  "When non-NIL, `deft-sort-files-by-name' will sort in a descending order,
otherwise ascending."
  :type 'boolean
  :group 'zettel)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun spacep (char)
  "Returns T if the character is some kind of a space."
  (when char
    (string-match-p "[[:space:][:punct:]]" (char-to-string char))))

(unless (fboundp 'rb-random-elt)
 (defun rb-random-elt (sequence)
   "Return a random element of SEQUENCE."
   (when (sequencep sequence)
     (elt sequence (random (length sequence))))))

(unless (fboundp 'rb-get-clipboard-data)
  (defun rb-get-clipboard-data ()
    "System-independent way to get current clipboard data. Returns
nil if there is nothing there."
    (case system-type
      (gnu/linux (x-get-selection 'CLIPBOARD))
      (windows-nt (w32-get-clipboard-data))
      (darwin (shell-command-to-string "/usr/bin/pbpaste"))
      (t nil))))

(unless (fboundp 'rb-set-clipboard-data)
  (defun rb-set-clipboard-data (string)
    "System-independent way to copy the given STRING to clipboard."
    (case system-type
      (gnu/linux (error "Not implemented"))
      (windows-nt (error "Not implemented"))
      (darwin
       (save-excursion
         (with-temp-buffer
           (insert string)
           (shell-command-on-region (point-min) (point-max)
                                    "/usr/bin/pbcopy"))))
      (t nil))))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun zettel-p (file-or-buffer)
  "Returns non-NIL if the file or buffer is a Zettel."
  (interactive "f")
  (when file-or-buffer
   (let ((file (typecase file-or-buffer
                 (buffer (buffer-file-name file-or-buffer))
                 (string file-or-buffer)
                 (t
                  (error "Don't know how to handle this type")))))
     (when file
       (and (string-equal (file-name-extension file) deft-extension)
            (string-match zettel-regexp-slug (file-name-base file)))))))

(defun zettel-kasten-directory (kasten)
  "Returns the directory of the given KASTEN."
  (if (assoc kasten zettel-kaesten)
      (file-name-as-directory (in-zettel-dir (zettel-kasten-truename kasten)))
    (error "Unknown Kasten: %s" kasten)))

(defun zettel-directory-kasten (directory)
  "Returns the kasten name of the given Zettel directory."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun zettel-kasten-truename (kasten)
  "Returns the true name of the given KASTEN."
  (or (cdr (assoc kasten zettel-kaesten-aliases))
      (car (assoc kasten zettel-kaesten))))

(defun zettel-file-slug (file)
  "Returns the slug part of the given Zettel file."
  (file-name-base file))

;; FIXME: Relies on the fact that the Kasten directory is 2nd from the last.
(defun zettel-file-kasten (file)
  "Returns the kasten of the given Zettel file."
  (let ((dirs (reverse (split-string (file-name-directory file) "/" t "/"))))
    (cond ((assoc (first dirs) zettel-kaesten)
           (zettel-kasten-truename (first dirs)))
          ((assoc (second dirs) zettel-kaesten)
           (zettel-kasten-truename (second dirs)))
          (t
           (error "Can't figure out kasten for %s" file)))))

(defun zettel-file-link (file)
  "Given the path to a Zettel FILE, returns a fully qualified link to it."
  (let ((kasten (zettel-file-kasten file)))
    (if (string= kasten
                 (alist-get (zettel-type file) zettel-default-kasten))
        (zettel-file-slug file)
      (concat kasten ":" (zettel-file-slug file)))))

(defun zettel-link-p (string)
  "Returns non-NIL if the string could be a link to a Zettel."
  (and (string-match (concat "^" zettel-regexp-link "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if (match-string-no-properties 2 string)
           (or (assoc (match-string-no-properties 2 string) zettel-kaesten)
               (assoc (match-string-no-properties 2 string) zettel-kaesten-aliases))
         t)))

(defun zettel-link-kasten (link)
  "Returns the kasten part of the given LINK. If no kasten is explicitly
specified, asks the user to resolve the ambiguity."
  (when (string-match zettel-regexp-link link)
    (let* ((kasten (match-string 2 link))
           (slug (match-string 3 link))
           (type (zettel-type slug)))
      (or kasten
          (if-let ((default (alist-get type zettel-default-kasten)))
              default
            (call-interactively #'zettel-set-default-kasten)
            (zettel-link-kasten link))))))

(defun zettel-set-default-kasten (type kasten)
  "Interactively set the default kasten for the given slug type. See
`zettel-default-kasten' for valid types."
  (interactive
   (list (intern (ivy-read "Set default for which type of Zettel? "
                           (mapcar #'first zettel-default-kasten)))
         (ivy-read "Set the default to what Kasten? "
                   (if (listp zettel-kaesten)
                       (mapcar #'first zettel-kaesten)
                     (error "No Zettelkästen defined")))))
  (setf (alist-get type zettel-default-kasten) kasten))

;; FIXME: Rename `zettel-type' to `zettel-slug-type'?
(defun zettel-kasten-slug-type (kasten)
  "Returns the Zettel slug naming type for the given kasten based on
`zettel-kaesten'."
  (second (assoc kasten zettel-kaesten #'string=)))

(defun zettel-link-slug (link)
  "Returns the slug part of the given LINK."
  (when (string-match zettel-regexp-link link)
    (match-string 3 link)))

(defun zettel-make-link (kasten slug)
  "Make a new proper link to SLUG in KASTEN."
  (let ((slug-type (zettel-kasten-slug-type kasten)))
    (cond ((not slug-type)
           (error "Unknown kasten: %s" kasten))
          ((not
            (string-match-p (alist-get slug-type zettel-type-regexp-alist) slug))
           (error "Slug doesn't match the slug type for %s kasten" kasten))
          ((rassoc kasten zettel-default-kasten)
           slug)
          (t
           (concat kasten ":" slug)))))

(defun zettel-numerus-subdirectory (slug)
  "Returns the right subdirectory for the given numerus currens slug."
  (when (string-match zettel-regexp-numerus-currens slug)
    (file-name-as-directory (subseq slug 0 1))))

(defun zettel-tempus-subdirectory (slug)
  "Returns the right subdirectory for the given tempus currens slug."
  (when (string-match zettel-regexp-tempus-currens slug)
    (file-name-as-directory (match-string 1 slug))))

(defun zettel-bolus-subdirectory (slug)
  "Finds the right directory for the given bolus currens slug."
  (when (stringp slug)
    (let ((result
           (case (elt slug 0)
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

(defun zettel-absolute-filename (link &optional noerror)
  "Return an absolute filename to the Zettel link. If NOERROR is non-NIL,
don't signal an error if the link is invalid.

This function replaces `deft-absolute-filename' for Zettel."
  (if (zettel-link-p link)
      (let ((kasten (zettel-link-kasten link))
            (slug (zettel-link-slug link)))
        (expand-file-name
         (concat slug "." deft-extension)
         (expand-file-name
          (case (zettel-type slug)
            (:numerus (zettel-numerus-subdirectory slug))
            (:tempus (zettel-tempus-subdirectory slug))
            (:bolus (zettel-bolus-subdirectory slug)) ; FIXME: temporary
            (t (unless noerror
                 (error "This is not a proper Zettel link: %s" link))))
          (zettel-kasten-directory kasten))))
    (unless noerror
      (error "This is not a proper Zettel link: %s" link))))

;; FIXME: Rename `zettel-type' to `zettel-slug-type'?
(defun zettel-type (slug-or-file)
  "Returns the type of the given slug or file: :NUMERUS or :TEMPUS."
  (let ((slug (file-name-base slug-or-file)))
    (cond ((string-match-p zettel-regexp-tempus-currens slug)
           :tempus)
          ((string-match-p zettel-regexp-numerus-currens slug)
           :numerus)
          ;; FIXME: Temporary
          ((string-match-p zettel-regexp-bolus-currens slug)
           :bolus)
          (t
           ;; Anything else is not a Zettel
           nil))))

(defun zettel-encode-iso8601-datetime (string)
  "Returns the internal encoded time given the ISO8601 date/time
expression, with or without time."
  (let ((second 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" zettel-regexp-iso8601-date) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match zettel-regexp-iso8601-time string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun zettel-file-content (file &optional noerror)
  "Returns the content of the file, getting it either from an opened buffer,
Deft cache, or the file itself. If NOERROR is non-NIL, don't signal an error
if cannot get the content."
  (cond ((get-file-buffer file)
         (with-current-buffer (get-file-buffer file)
           (save-restriction
             (widen)
             (buffer-substring-no-properties (point-min) (point-max)))))
        ((and deft-hash-contents (deft-file-contents file))
         (deft-file-contents file))
        ((file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-substring-no-properties (point-min) (point-max))))
        (t
         (unless noerror
          (error "Cannot get content for %s" file)))))

;;;=============================================================================
;;; Metadata
;;;=============================================================================

(defvar zettel-regexp-metadata-line
  "\\(\\w+\\):\\s-+\\(.*\\)"
  "The regular expression that matches a YAML metadata line.
Group 1 is the key.
Group 2 is the value.")

(defvar zettel-regexp-combined-title
  (concat "^§"
          zettel-regexp-link
          "\\. \\({\\([^}]+\\)}\\)*\\([^#@]+\\)*\\(@\\S-+\\)*\\(#.+\\)*")
  "Regular expression for a combined title string, used in `zettel-metadata'.
Group 2 is the kasten.
Group 3 is the slug.
Group 5 is the category.
Group 6 is the title itself.
Group 7 is the citation key.
Group 8 is the keyword block.")

(defun zettel-decode-combined-title (combined)
  "Returns an alist of metadata from a combined title. If cannot decode,
returns NIL."
  (when (and combined (string-match zettel-regexp-combined-title combined))
    (let ((slug     (match-string 3 combined))
          (category (match-string 5 combined))
          (title    (match-string 6 combined))
          (citekey  (match-string 7 combined))
          (keywords (match-string 8 combined)))
      (list (cons :slug slug)
            (cons :type (zettel-type slug))
            (cons :category category)
            (cons :title (if title (string-trim title) ""))
            (when citekey (cons :citekey (string-trim citekey)))
            (when keywords (cons :keywords (list (string-trim keywords))))))))

(defun zettel-encode-combined-title (metadata)
  "Returns a list of two elements: 1) string that encodes into the title line
the given METADATA, and 2) leftover metadata."
  (list (format "§%s. {%s} %s%s"
                (alist-get :link metadata)
                (or (alist-get :category metadata) "Unset")
                (alist-get :title metadata)
                (if (alist-get :citekey metadata)
                    (concat " " (alist-get :citekey metadata))
                  ""))
        (set-difference metadata '((:link) (:category) (:title) (:type) (:citekey))
                        :key #'car)))

(defun zettel-metadata-yaml-key (keyword)
  "Returns a YAML-formatted string that is the name of the KEY, a keyword
symbol."
  (subseq (symbol-name keyword) 1))

(defun zettel-metadata-yaml-value (value)
  "Returns a YAML-formatted string for the given metadata VALUE."
  (typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun zettel-normalize-metadata (file &optional metadata)
  "Replaces the FILE's metadata section with either the given METADATA or
by parsing the FILE's metadata."
  (let ((metadata (or metadata (zettel-metadata file)))
        (old-point (point)))
    (save-mark-and-excursion
      (with-current-buffer (get-file-buffer file)
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t 1)
            (narrow-to-region (point-min) (point)))
          (delete-region (point-min) (point-max))
          (multiple-value-bind (title remaining-metadata)
              (zettel-encode-combined-title metadata)
            (insert "title: " title "\n")
            (mapc #'(lambda (cons)
                      (insert (format "%s: %s\n"
                                      (zettel-metadata-yaml-key (car cons))
                                      (zettel-metadata-yaml-value (cdr cons)))))
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

(defun zettel-metadata (file)
  "Returns an alist of metadata for the given FILE based on the most current
content of the FILE. They keys are converted to keywords."
  (let* ((metadata-section
          (split-string
           (first (split-string
                   ;; Do a sane thing when I opened a Zettel file directly
                   ;; rather than through Deft interface.
                   (zettel-file-content file)
                   "\n\n"))
           "\n"))
         (metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match zettel-regexp-metadata-line line)
                   (let ((key (intern (concat ":" (match-string 1 line))))
                         (value (string-trim (match-string 2 line) " " " ")))
                     (cons key
                           ;; Handle lists properly
                           (if (string-match "^\\[\\(.*\\)\\]$" value)
                               (split-string (match-string 1 value)
                                             "," t "[[:space:]]+")
                             value)))
                 (error "Malformed metadata line: '%s'" line))))
           metadata-section))
         (title (alist-get :title metadata))
         (decoded (zettel-decode-combined-title title)))
    ;; When successfully decoded combined title, replace the original title with
    ;; the decoded metadata.
    (when decoded
      (setq metadata
        (append decoded (cl-remove :title metadata :key #'car))))
    (push (cons :kasten (zettel-file-kasten file)) metadata)
    (push (cons :link (zettel-file-link file)) metadata)))

(defcustom zettel-update-modification-date t
  "Determines whether `zettel-update-metadata-date' updates the modification
date. Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (or T)."
  :type 'symbol)

(defun zettel-update-metadata-date ()
  "Updates the modification time in the metadata section of the Zettel in the
current buffer according to the value of `zettel-update-modifaction-date'."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (now (format-time-string "%Y-%m-%d %a %H:%M"))
         (metadata (zettel-metadata buffer-file-name))
         (last-modified (or (alist-get :modified metadata)
                            (alist-get :created metadata))))
    (unless (string-equal (or last-modified "") now)
      ;; FIXME: Probably better to convert modification times to Emacs's encoded
      ;; time rather than doing it with strings.
      (when (or (equal zettel-update-modification-date 'always)
                (and (equal zettel-update-modification-date 'sameday)
                     (string= (subseq last-modified 0 (length today)) today))
                (and (member zettel-update-modification-date '(confirm t))
                     (y-or-n-p
                      (format "%s last modified at %s. Update to now? "
                              (file-name-base buffer-file-name)
                              last-modified))))
        (setf (alist-get :modified metadata) now)))
    (zettel-normalize-metadata buffer-file-name metadata)))

(defun zettel-update-title ()
  "Interactively asks for a different title and updates the Zettel's metadata."
  (interactive)
  (when (zettel-p buffer-file-name)
    (let ((metadata (zettel-metadata buffer-file-name)))
      (setf (alist-get :title metadata)
            (read-string "Change title to what? "
                         (alist-get :title metadata)))
      (zettel-normalize-metadata buffer-file-name metadata))))

(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)
     (add-hook 'after-save-hook
       '(lambda ()
          (deft-cache-file buffer-file-name))
       nil t)))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun zettel-make-numerus (number letters)
  "Returns a new numerus currens slug composed of the NUMBER and LETTERS,
both of which are strings."
  (concat number "-" letters))

(defun zettel-numerus-number (slug)
  "Returns the number part of the SLUG as a string."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 1 slug)))

(defun zettel-numerus-letters (slug)
  "Returns the letters part of the SLUG as a string."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 3 slug)))

(defun zettel-numerus-parts (slug)
  "Returns NIL if the slug is not a numerus currens slug, and otherwise
returns a list of two elements: the number and letters parts of the slug."
  (when (and (stringp slug)
             (string-match zettel-regexp-numerus-currens slug))
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

(defun zettel-generate-new-slug (type)
  "Generates a random new slug of the given type."
  (case type
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

(defun zettel-next-unused-slug (&optional type)
  "Returns the next unused slug, relying on `zettel-deft-active-kasten' and
`zettel-kaesten' to figure out the type if not given, and on `deft-all-files'
to avoid duplicates."
  (let* ((active-kasten-type (second (assoc zettel-deft-active-kasten
                                            zettel-kaesten
                                            #'string=)))
         (type (or type active-kasten-type))
         slug)
    (cond ((eq type :tempus)
           (setq slug (zettel-generate-new-slug type)))
          ((eq type active-kasten-type)
           (message "Generating next unused slug of type %s" type)
           (let ((used (mapcar #'zettel-file-slug deft-all-files)))
             (while (or (not slug) (find slug used))
               (setq slug (zettel-generate-new-slug type)))))
          ((and (eq type :numerus)
                (file-exists-p (in-zettel-dir zettel-pregenerated-numeri)))
           (message "Getting next numerus from `zettel-pregenerated-numeri'...")
           (let ((buffer (find-file-noselect
                          (in-zettel-dir zettel-pregenerated-numeri))))
             (with-current-buffer buffer
               (setq slug
                 (string-trim (delete-and-extract-region
                               1 (search-forward-regexp "[[:space:]]" nil t))))
               (basic-save-buffer))))
          (t
           (message "Generating unused slug without checking for duplicates...")
           (setq slug (zettel-generate-new-slug type))))
    slug))

(defun deft-new-unused-zettel ()
  "Create a new Zettel with unused numerus currens."
  (interactive)
  (deft-new-file-named (zettel-next-unused-slug)))

(defun zettel-rename-with-unused-slug ()
  "Rename the current file and buffer to an unused filename
slug (short name) in `deft-directory' with `deft-extension'.
Based on `rename-file-and-buffer'."
  (interactive)
  (rename-file-and-buffer (concat (zettel-next-unused-slug) "." deft-extension)))

(defun slug-pronounceable-p (letters next)
  "Returns NIL if NEXT is not pronounceable after LETTERS."
  (cl-flet* ((lastn (seq n)
                    "Returns last N members of SEQ, or nil if it's too
              short."
                    (when (<= n (length seq))
                      (subseq seq (- n))))
             (clusterp (str)
                       (member str '("ai" "au" "ea" "ia" "io" "oa" "oi" "ou" "ua"
                                     "ch" "ck" "ff" "gh" "gl" "mn" "ph"
                                     "qu" "rh" "rp" "rs" "rt" "rz" "sc" "sh" "sk"
                                     "st" "th" "zh")))
             (liquidp (ch) (member ch '("l" "r")))
             (sibilantp (ch) (member ch '("s" "z")))
             (vowelp (ch) (member ch '("a" "e" "i" "o" "u" "y")))
             (consonantp (ch) (not (vowelp ch))))
    (let* ((next (if (characterp next) (char-to-string next) next))
           (prev (lastn letters 1))
           (cluster (concat prev next)))
      (when (or (not letters)
                (clusterp cluster)
                (and (vowelp prev) (consonantp next))
                (and (consonantp prev) (vowelp next))
                (and (consonantp prev)
                     (or (and (not (liquidp prev)) (liquidp next))
                         (or (sibilantp prev) (sibilantp next)))))
        t))))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun zettel-decode-time-into-tempus-currens (time)
  "Returns a tempus currens slug based on the given Emacs time object."
  (format-time-string "%Y%m%dT%H%M" time))

(defun zettel-tempus-currens-slug-for (link)
  "Returns a suitable tempus currens slug for the given Zettel link."
  (if (eq (zettel-kasten-slug-type (zettel-link-kasten link)) :tempus)
      ;; If already tempus currens, just return that slug
      (zettel-link-slug link)
    ;; Otherwise come up with an appropriate slug based on the metadata
    (let ((metadata (zettel-metadata (zettel-absolute-filename link)))
          oldname)
      (cond ((setq oldname
               (find-if #'(lambda (l)
                            (eq (zettel-kasten-slug-type (zettel-link-kasten l))
                                :tempus))
                        (alist-get :oldnames metadata)))
             ;; One of the old names was a tempus currens; just use that
             (zettel-link-slug oldname))
            ((alist-get :created metadata)
             ;; Use the created metadata and make up the time of creation
             ;; FIXME: Any more elegant way to do this?
             (zettel-decode-time-into-tempus-currens
              ;; TODO: This needs to handle org-mode timestamps in metadata
              (zettel-encode-iso8601-datetime
               (concat (alist-get :created metadata)
                       "T"
                       (format-time-string "%H:%M")))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (zettel-next-unused-slug :tempus)))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun zettel-link-at-point-p ()
  "Returns T if the thing at point is a wiki link (i.e. [[XXX]] or org-mode
link). The first group is the link target."
  (thing-at-point-looking-at
   (concat "\\[\\[\\(" zettel-regexp-link "\\)\\]\\(\\[[^]]+\\]\\)*\\]")))

(defun zettel-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`zettel-link-at-point-p'."
  (match-string-no-properties 1))

(defun zettel-kill-link-or-sexp-at-point (&optional arg)
  "If there is a Zettel link at point, kill it, including the square
brackets. Otherwise, call `kill-sex'."
  (interactive "p")
  (if (zettel-link-at-point-p)
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

(defun zettel-org-format-link (target &optional description)
  "Returns a formatted org-link to TARGET, which can be either a link or a filepath."
  (let* ((file (or (if (file-name-absolute-p target)
                       target
                     (zettel-absolute-filename target))
                   (error "Link target doesn't exist; make sure it's saved")))
         (link (zettel-file-link file)))
    (format "[[%s]%s]"
            link
            (if description
                (format "[%s]" description) ""))))

(defun zettel-insert-link-with-metadata (link &optional field where confirm)
  "Inserts the Zettel link, optionally adding a metadata FIELD put
WHERE (:BEFORE, :AFTER, or in :DESCRIPTION). If CONFIRM is non-NIL, ask for
confirmation before inserting metadata."
  (let* ((file (zettel-absolute-filename link))
         (metadata (zettel-metadata file))
         (field (or field
                    (when (called-interactively-p 'any)
                      (intern-soft
                       (ivy-read
                        "Which metadata field? "
                        '(":none" ":title" ":citekey" ":category"))))))
         (value (alist-get field metadata))
         (where (or where
                    (when field
                      (intern-soft
                       (ivy-read "Where? "
                                 '(":before" ":after" ":description")))))))
    (insert (if (or (bolp) (spacep (char-before))) "" " ")
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
                        (zettel-org-format-link
                         link
                         (when (eq where :description)
                           value))
                        (if (eq where :after)
                            (concat " " value)
                          ""))
              (zettel-org-format-link link))
            (if (or (eolp) (spacep (char-after))) "" " "))))

(defun zettel-insert-link-to-cached-or-visiting (arg)
  "Inserts a link to another Zettel being currently visited or to those in
the Deft cache. With prefix argument, offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in the list
of cached or visiting Zettel, just insert the link to what was selected. If
the cursor in already inside a link, replace it instead."
  (interactive "P")
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (zettel-deft-parsed-title path) path))
                                (zettel-visiting-buffer-list t))
                        (zettel-ivy-titles-reverse-alist #'string>)))))
    (if choices
        (let* ((choice (zettel-ivy-read-reverse-alist-action
                        "Insert link to: " choices 'zettel-file-link nil))
               (link (or (cdr choice)
                         ;; Create a new child if there is no match
                         (let ((new-child (zettel-insert-new-child nil)))
                           (kill-new (car choice)) ; save the entered text
                           (save-excursion
                             (with-current-buffer
                                 (zettel-absolute-filename
                                  (zettel-find-link new-child))
                               (zettel-insert-metadata-template
                                nil (car choice))))
                           new-child))))
          (if (not (zettel-link-at-point-p))
              (if arg
                  (funcall-interactively #'zettel-insert-link-with-metadata link)
                (zettel-insert-link-with-metadata link :title :before t))
            ;; When replacing, don't including anything
            (delete-region (match-beginning 0) (match-end 0))
            (insert (zettel-org-format-link link))))
      (user-error "No Deft cache or visited Zettel"))))

(defun zettel-insert-link-from-clipboard (arg)
  "Link `zettel-insert-link' but attempts to get the link slug from OS
clipboard. With prefix argument, ask for a metadata field to include."
  (interactive "P")
  (let ((link (rb-get-clipboard-data))
        (backlink (when buffer-file-name
                    (zettel-link-slug buffer-file-name))))
    (when (zettel-link-p link)
      (if arg
          (funcall-interactively #'zettel-insert-link-with-metadata link)
        (zettel-insert-link-with-metadata link))
      (when backlink
        (rb-set-clipboard-data backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun zettel-kill-ring-save-link-title (arg)
  "Save the title of the wiki link at point or the buffer to the kill ring
and system clipboard. With prefix argument, saves the 'combinted title'."
  (interactive "P")
  (let ((file (cond ((zettel-link-at-point-p)
                     (zettel-absolute-filename (zettel-link-at-point)))
                    ((zettel-p buffer-file-name)
                     buffer-file-name)
                    ((equal major-mode 'deft-mode)
                     (button-get (button-at (point)) 'tag))
                    (t
                     (message "Save title of what?")))))
    (when file
      ;; FIXME: Is this too low-level for here? Should `zettel-file-content'
      ;; handle this situation (cache too old) somehow?
      (when (and deft-hash-contents (deft-file-contents file))
        (deft-cache-update-file file))
      (let* ((metadata (zettel-metadata file))
             (title (if arg
                        (car (zettel-encode-combined-title metadata))
                      (alist-get :title metadata))))
        (kill-new title)
        (unless select-enable-clipboard
          (rb-set-clipboard-data title))
        (message "Saved [%s] in the kill ring" title)))))

(defun zettel-kill-ring-save-link (arg)
  "Save the Zettel link at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `zettel-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (cond ((equal major-mode 'deft-mode)
                     (button-get (button-at (point)) 'tag))
                    ((zettel-link-at-point-p)
                     (zettel-absolute-filename (zettel-link-at-point) t))
                    (buffer-file-name
                     buffer-file-name)
                    (t
                     (message "Save a link to what?")))))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name file zettel-directory)
                    (zettel-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (rb-set-clipboard-data link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R %s &" file)))))))

(defun zettel-kill-ring-save-next-link ()
  "Save the first link at or after point (but before EOL)."
  (interactive)
  (save-excursion
   (let ((link (if (zettel-link-at-point-p)
                   (zettel-link-at-point)
                 (let ((eol (save-excursion (end-of-visual-line) (point))))
                   (when (re-search-forward zettel-regexp-link eol t)
                     (match-string-no-properties 0))))))
     (when link
       (kill-new link)
       (message "Saved [%s] to kill ring" link)))))

;; Modified from zetteldeft's `zetteldeft-avy-link-search'.
(defun zettel-avy-link-search ()
  "Use `avy' to follow a Zettel wiki link."
  (interactive)
  (save-excursion
    (when (consp (avy-jump zettel-regexp-link))
      (zettel-open-link-at-point))))

(defun zettel-links-to (arg)
  "List links to the current Zettel from anywhere else in the Zettelkasten."
  (interactive "p")
  (funcall (if arg
               #'async-shell-command
             #'shell-command)
           (concat "zlinksto" " " (zettel-file-link buffer-file-name)))
  (when arg
    (switch-to-buffer-other-window "*Async Shell Command*")))

(defun zettel-deft-parsed-title (file)
  "Returns the result of `deft-file-title' if available or the result of
`zettel-deft-parse-title-function' on the first line of the given FILE."
  (or (deft-file-title file)
      (zettel-deft-parse-title-function
       (first (split-string
               (or (zettel-file-content file t) "")
               "\n")))))

(defun zettel-show-link-title-in-minibuffer ()
  "Displays Zettel title of the link under cursor, less category and citekey,
in the minibuffer."
  (while-no-input
    (redisplay))
  (when (and (eq major-mode 'org-mode)
             (zettel-link-at-point-p))
    (let* ((file (zettel-absolute-filename (match-string 1) t))
           (title (zettel-deft-parsed-title file))
           (title
            (if (string-match "^\\([[:alnum:]-]+\\).*	.*	\\(.*\\)$" title)
                (format "%s / %s" (match-string 1 title) (match-string 2 title))
              title)))
      (message (s-center (window-width) title)))))
(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'post-command-hook
       'zettel-show-link-title-in-minibuffer)))
(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'after-save-hook
       'zettel-show-link-title-in-minibuffer)))

;; Show the beginning of Zettel title in mode-line
(defun zettel-mode-line-buffer-id ()
  (interactive)
  (when buffer-file-name
    (multiple-value-bind (slug category citekey title)
        ;; <slug>	<category>	<citekey>	<title>
        (split-string (or (deft-file-title buffer-file-name) "") "\t" t " +")
      ;; FIXME: Clunky, but some deft-titles don't have citekeys or categories
      (if (or title citekey category)
          (let* ((words (split-string (or title citekey category) " "))
                 (first3 (mapconcat #'identity
                           (subseq words 0 (min 3 (length words)))
                           " "))
                 (slug-end (if (or (eq (zettel-type slug) :numerus)
                                   (eq (zettel-type slug) :bolus)) ; FIXME: temporary
                               slug
                             (concat "~"
                                     (subseq slug (- (length slug)
                                                     (length "0101T0101"))))))
                 (buffer-id (format "%s: %s" slug-end first3)))
            (setq-local mode-line-buffer-identification buffer-id))
        (setq-local mode-line-buffer-identification
                    (format "%12s" (zettel-file-link buffer-file-name)))))))
(add-hook 'zettel-mode-hook 'zettel-mode-line-buffer-id)

;;;=============================================================================
;;; Inserting snippets
;;;=============================================================================

(defcustom zettel-insert-snippet-summary nil
  "Non-nil means insert the snippet summary."
  :type 'boolean)

(defcustom zettel-insert-snippet-footnotes nil
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
(defun zettel-insert-snippet-text (arg file)
  "Inserts the combination of Summary and Snippet sections from the given
snippet FILE into the current buffer. With prefix argument, forces update."
  (interactive
   ;; Assume the file is the last link on the current line
   (list current-prefix-arg
         (save-excursion
           (end-of-line)
           (org-previous-link)
           (when (zettel-link-at-point-p)
             (zettel-absolute-filename (zettel-link-at-point))))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((metadata (zettel-metadata file))
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
                (when (and zettel-insert-snippet-summary
                           (org-find-exact-headline-in-buffer "Summary"))
                  (goto-char (org-find-exact-headline-in-buffer "Summary"))
                  (forward-line)
                  (let ((copy-from (point)))
                    (org-end-of-subtree)
                    (mapcar #'(lambda (line)
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
              (unless zettel-insert-snippet-footnotes
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

(defun zettel-find-inserted-snippet ()
  "While the point is within the org entry, find the source of the snippet
inserted through `zettel-insert-snippet-text'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-next-link)
    (org-open-at-point)))

(defun zettel-transclude-snippet (link)
  "Inserts the transclusion statement from given snippet LINKE into the
current buffer."
  (interactive
   ;; Assume the file is the last link on the current line
   (list (save-excursion
           (org-back-to-heading)
           (end-of-line)
           (org-previous-link)
           (when (zettel-link-at-point-p)
             (zettel-link-at-point)))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((file (zettel-absolute-filename link))
             (metadata (zettel-metadata file)))
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

(defcustom zettel-snippet-heading "Snippet"
  "The text of the snippet heading."
  :type 'string)

(defun zettel-org-footnote-action-maybe-local (&optional arg)
  "This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets, making sure that the footnotes are placed locally
rather in whatever `org-footnote-section' is set to."
  (interactive "P")
  (let (snippet?)
    (save-excursion
      (org-back-to-heading t)
      (setq snippet?
        (string-equal (nth 4 (org-heading-components)) zettel-snippet-heading)))
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
(define-key org-mode-map (kbd "C-c C-x f") 'zettel-org-footnote-action-maybe-local)

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun zettel-trace-genealogy (file-or-link &optional degree)
  "Returns the FILE-OR-LINK's next genealogical link, or NIL if could not
figure out. With the optional DEGREE, try to find the Nth link (i.e.
grandparent if DEGREE is 2, child if DEGREE is -1, an so on), returning the
most remote link that could be found."
  (let ((degree (or degree 1)))
    (if (= (abs degree) 0)
        file-or-link
      (zettel-trace-genealogy (alist-get (if (plusp degree)
                                             :parent
                                           :firstborn)
                                         (zettel-metadata
                                          (if (zettel-link-p file-or-link)
                                              (zettel-absolute-filename file-or-link)
                                            file-or-link)))
                              (if (plusp degree)
                                  (1- degree)
                                (1+ degree))))))

(defun zettel-find-ancestor (n)
  "Opens the current Zettel's immediate ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((ancestor (zettel-trace-genealogy buffer-file-name n)))
      (if ancestor
          (zettel-find-link ancestor)
        (message "No ancestor found")))))

(defun zettel-find-descendant (n)
  "Opens the current Zettel's immediate descendant. With a prefix argument,
try to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((descendant (zettel-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (zettel-find-link descendant)
        (message "No descendant found")))))

(defun zettel-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current Zettel, adding its title (if
available) before the link. With a numerical prefix argument, try to find Nth
ancestor. With a universal argument, ask for confirmation before inserting."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (zettel-trace-genealogy buffer-file-name degree)))
    (if link
        (zettel-insert-link-with-metadata link :title :before (not arg))
      (message "Could not find such ancestor"))))

(defvar zettel-parent-of-new-child nil
  "An alist of new children and their respective parents.")

(defun zettel-generate-new-child (parent kasten)
  "Generates a new child of the given PARENT in the KASTEN."
  (let ((child-link (zettel-make-link
                     kasten
                     (zettel-next-unused-slug (zettel-kasten-slug-type kasten)))))
    (add-to-list 'zettel-parent-of-new-child (cons child-link parent))
    child-link))

(defun zettel-insert-new-child (&optional arg)
  "Creates a new Zettel in the current `deft-directory', inserting a link to
it at point, saves the current Zettel as its parent, and sets the
`zettel-link-backlink' to current Zettel. With prefix argument, allows the
user to select the Zettelkasten. With double prefix argument, asks for the
full link. Returns link the new child."
  (interactive "P")
  (let ((parent-link
         (zettel-file-link (cond ((zettel-p buffer-file-name)
                                  buffer-file-name)
                                 ((equal major-mode 'deft-mode)
                                  (button-get (button-at (point)) 'tag))
                                 (t
                                  (user-error "Child of what?")))))
        child-link)
    (if (equal arg '(16))
        (while (not child-link)
          (setq child-link (read-string "Enter link for new child: "))
          (when (file-exists-p (zettel-absolute-filename child-link))
            (message "This Zettel already exists; try again")))
      (let ((kasten (cond ((equal arg '(4))
                           (ivy-read "Zettelkasten: "
                                     (if (listp zettel-kaesten)
                                         (mapcar #'first zettel-kaesten)
                                       (error "No Zettelkasten defined"))))
                          (t
                           (unless (assoc :numerus zettel-default-kasten)
                             (call-interactively #'zettel-set-default-kasten))
                           (zettel-directory-kasten deft-directory)))))
        (setq child-link (zettel-generate-new-child parent-link kasten))))
    (if (equal major-mode 'deft-mode)
        (deft-new-file-named (zettel-link-slug child-link))
      (insert (zettel-org-format-link child-link)))
    child-link))

(defun zettel-ivy-set-parent ()
  "Sets the parent metadata of the current Zettel to the Zettel chosen by the
user from cached and visiting Zettel."
  (interactive)
  (let ((metadata (zettel-metadata buffer-file-name)))
    (zettel-ivy-read-reverse-alist-action
     "Set parent to: "
     (delete-dups
      (append (mapcar (lambda (path)
                        (cons (deft-file-title path) path))
                      (zettel-visiting-buffer-list t))
              (zettel-ivy-titles-reverse-alist)))
     (lambda (path)
       (setf (alist-get :parent metadata) (zettel-file-link path))
       (zettel-normalize-metadata buffer-file-name metadata)))))

;;;=============================================================================
;;; Buffers, Files, Categories
;;;=============================================================================

(defun zettel-find-file (file &optional same-window)
  "Edit the given file based on the value of `zettel-number-of-frames'. If
SAME-WINDOW is non-NIL, opens the buffer visiting the file in the same
window."
  (if same-window
      (find-file file)
    (case zettel-number-of-frames
      (two (if (< (length (frame-list)) 2)
               (find-file-other-frame file)
             (select-window (ace-select-window))
             (find-file file)))
      (one (let ((pop-up-windows t))
             (select-window (ace-select-window))
             (find-file file)))
      (nil (find-file file))
      (t (find-file-other-frame file)))))

(defun zettel-find-link (link &optional same-window)
  "Attempts to find the given Zettel link based on the value of
`zettel-number-of-frames'. If SAME-WINDOW is non-NIL, opens the link in the
same window. Returns T if the link is a Zettel link."
  (when (zettel-link-p link)
    (zettel-find-file (zettel-absolute-filename link) same-window)
    (when (zerop (buffer-size))
      (call-interactively #'zettel-insert-metadata-template))
    ;; make sure to return T for `org-open-link-functions'
    t))

(defun zettel-select-link (arg)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. With universal prefix, asks the user to type the link
instead."
  (interactive "P")
  (funcall #'zettel-find-link
           (if arg
               (read-string "Zettel link to find: ")
             (zettel-file-link (cdr (zettel-ivy-read-reverse-alist-action
                                     "Select title: "
                                     (zettel-ivy-titles-reverse-alist)
                                     #'identity))))))

(defun zettel-ivy-read-reverse-alist-action (prompt choices func &optional require-match)
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

(defun zettel-ivy-titles-reverse-alist (&optional sort)
  "Returns a reverse alist of choices consisting of cached Zettel titles and
their paths. For use with `zettel-ivy-read-reverse-alist-action'."
  (let (titles-alist)
    (cond (deft-hash-titles
            (maphash (lambda (key val)
                       (push (cons (or val key) key) titles-alist))
                     deft-hash-titles)
            (if (functionp sort)
                (cl-sort titles-alist sort :key #'car)
              titles-alist))
          (t
           (error "No Deft titles cached")))))

(defun zettel-ivy-metadata-reverse-alist (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `zettel-ivy-read-reverse-alist-action' as collection.
Relies on Zettel metadata, so slower than `zettel-ivy-titles-reverse-alist'."
  (let ((fmt (concat "%s%-12s %-10s %-53s %s")))
    (mapcar #'(lambda (file)
                (let ((metadata (zettel-metadata file))
                      (buf (get-file-buffer file)))
                  (cons (format fmt
                                (if (and buf (buffer-modified-p buf)) "*" " ")
                                (alist-get :slug metadata)
                                (alist-get :category metadata)
                                (subseq (alist-get :title metadata) 0
                                        (min (length (alist-get :title metadata))
                                             53))
                                (or (alist-get :keywords metadata) ""))
                        file)))
            files)))

(defun zettel-visiting-buffer-list (&optional skip-current)
  "Returns a list of Zettel files that are currently being visited. If
SKIP-CURRENT is T, remove the current buffer."
  (mapcar #'buffer-file-name
          (remove-if-not #'(lambda (buf)
                             (zettel-p (buffer-file-name buf)))
                         (remove (when skip-current
                                   (current-buffer))
                                 (buffer-list)))))

(defun zettel-kill-visiting-buffers ()
  "Kills all Zettel that are being currently visited."
  (interactive)
  (mapc #'(lambda (file)
            (kill-buffer (get-file-buffer file)))
        (zettel-visiting-buffer-list t)))

(defun zettel-switch-to-buffer (arg)
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
                 (zettel-visiting-buffer-list t))))
    (zettel-ivy-read-reverse-alist-action
     "Switch to Zettel: "
     (or choices (zettel-ivy-titles-reverse-alist #'string>))
     (if (not arg) 'find-file 'find-file-other-window))))

(defun zettel-ivy-read-category (&optional arg prompt sort-fn)
  "Uses `ivy-read' to select a category from `zettel-categories'. With prefix
argument, asks the user to type in the category directly. If SORT-FN is
given, use that to sort the list first."
  (let ((prompt (or prompt "Category: "))
        (categories (if (functionp sort-fn)
                        (cl-sort zettel-categories sort-fn :key #'cdr)
                      zettel-categories)))
    (if current-prefix-arg
        (read-string prompt)
      (ivy-read prompt zettel-categories))))

(defun zettel-set-category (file category)
  "Sets the category to the Zettel title based on `zettel-categories'. With
prefix argument, allows the user to type in a custom category."
  (interactive (list (cond ((zettel-p buffer-file-name)
                            buffer-file-name)
                           ((equal major-mode 'deft-mode)
                            (button-get (button-at (point)) 'tag))
                           (t
                            (user-error "Set category of what?")))
                     (zettel-ivy-read-category nil nil #'>)))
  (let ((orig-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (if (re-search-forward "^title: §?\\([^.]+\\)\\. \\({[^}]+} \\)*" nil t)
            (replace-match (format "title: §\\1. {%s} " category))
          (message "Not sure how to set the category here"))
        ;; only save buffer if was not initially visiting it
        (unless (eq orig-buffer (current-buffer))
          (save-buffer))))))

(defun zettel-populate-categories ()
  "Populate `zettel-categories' based on the titles in Deft cache."
  (interactive)
  (setq zettel-categories '())
  (dolist (file deft-all-files)
    (let* ((category (alist-get :category (zettel-metadata file)))
           (frequency (alist-get category zettel-categories 0 nil #'string-equal)))
      (setf (alist-get category zettel-categories nil nil #'string-equal)
            (1+ frequency))))
  (message "%d categories in %d files"
           (length zettel-categories) (length deft-all-files)))

(defun zettel-add-bibliographic-category ()
  "Add a category to the Zettel title based on the bibliographic title."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; 1: slug
    ;; 2: given name(s)
    ;; 3: family name
    ;; 4: title
    ;; 5: year
    (when (re-search-forward "^title: §?\\([0-9a-z:-]+\\)\\. \
\\(\\w+ \\)+\\(\\w+\\), \\([^(]+\\) (\\([0-9]+\\))")
      (replace-match (subseq (match-string 2) 0 1) nil nil nil 2)
      (replace-match "title: §\\1. {\\3\\5} \\2. \\3's \\4 (\\5)"))))

;;;=============================================================================
;;; Deft-Mode Integration
;;;=============================================================================

;; Adjust how Deft lists Zettel
(setq deft-strip-title-regexp "^\\(title: +\\)"
      ;; Default: "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
      deft-strip-summary-regexp ".*"
      ;; Default: "\\([\n	]\\|^#\\+[[:upper:]_]+:.*$\\)"
      ;; Modified: "\\(^\\w+: .*\\)"
      deft-time-format nil
      deft-use-filename-as-title nil
      deft-current-sort-method 'mtime)

(add-hook 'deft-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)))

(defun zettel-deft-choose-kasten (arg new-kasten &optional number-of-frames)
  "If there is an existing `deft-buffer', switches to it, otherwise
interactively selects the deft directory from among `zettel-kaesten'. With a
prefix argument, selects new deft directory regardless of `deft-buffer'; with
double prefix argument calls `zettel-deft-choose-directory' instead. With
optinal NUMBER-OF-FRAMES, set the `zettel-number-of-frames' to that value."
  (interactive
   (if (or (null (get-buffer deft-buffer))
           (equal current-prefix-arg '(4)))
       (list current-prefix-arg
             (ivy-read "Zettel kasten: " zettel-kaesten)
             (intern (ivy-read "Number of frames: " '(one two many))))
     (list current-prefix-arg nil zettel-number-of-frames)))
  (cond ((equal arg '(16))
         (call-interactively #'zettel-deft-choose-directory))
        ((not new-kasten)
         ;; FIXME: This should handle `zettel-number-of-frames' being TWO
         (case zettel-number-of-frames
           (nil (switch-to-buffer deft-buffer))
           (one (switch-to-buffer deft-buffer))
           (two (with-selected-window (ace-select-window)
                  (switch-to-buffer deft-buffer)))
           (many
            (switch-to-buffer-other-frame deft-buffer))))
        (t
         (setq zettel-deft-active-kasten new-kasten
               zettel-number-of-frames number-of-frames)
         (zettel-deft-choose-directory (zettel-kasten-directory new-kasten)))))

(defun zettel-deft-choose-directory (directory)
  "Interactively selects the directory, starting in `zettel-directory'."
  (interactive
   (list
    (read-directory-name "Deft directory: "
                         zettel-directory
                         "" t)))
  (when (and (get-buffer deft-buffer))
    (kill-buffer deft-buffer))
  (setq deft-directory directory
        deft-buffer (format "*Deft: %s*"
                            (capitalize
                             (file-name-base (directory-file-name directory)))))
  (deft)
  (deft-filter nil)
  (deft-refresh)
  (zettel-populate-categories))

(defun zettel-deft-parse-title-function (line &optional show-missing)
  "Function for post-processing titles for display in Deft buffer, intended
as the value for `deft-parse-title-function'. If SHOW-MISSING is non-NIL,
the missing metadata is explicitly displayed."
  (when line
    (let* ((metadata (zettel-decode-combined-title
                      (replace-regexp-in-string "^\\(title: +\\)" "" line)))
           (slug (or (alist-get :slug metadata)
                     (if show-missing "<slug>" "")))
           (cat (or (alist-get :category metadata)
                    (if show-missing "<category>" "")))
           (key (or (alist-get :citekey metadata)
                    (if show-missing "<citekey>" "")))
           (key (if (string-match "^@" key)
                    (replace-match "" nil nil key)
                  key))
           (title (or (alist-get :title metadata)
                      (if (not (zerop (length line)))
                          line
                        "<title>")))
           (SLUG-LEN
            (length
             (alist-get (or (zettel-type (alist-get :slug metadata))
                            (zettel-kasten-slug-type zettel-deft-active-kasten)
                            :tempus)    ; assume longest
                        zettel-type-example-alist)))
           (CAT-LEN 12)
           (KEY-LEN 10)
           ;; SLUG---CATEGORY---CITEKEY---TITLE [where --- is tab]
           (fmt (format "%%-%ds\t%%-%ds\t%%-%ds\t%%s" SLUG-LEN CAT-LEN KEY-LEN)))
      (format fmt                       ; requires 4 arguments
              slug
              (if (> (length cat) CAT-LEN)
                  (concat (subseq cat 0 (- CAT-LEN 1)) "…")
                cat)
              (if (> (length key) KEY-LEN)
                  (concat (subseq key 0 (- KEY-LEN 1)) "…")
                key)
              title))))
(setq deft-parse-title-function 'zettel-deft-parse-title-function)

(defun zettel-adv--deft-new-file-maybe-named (arg)
  "Extends `deft-new-file' to call `deft-new-file-named' if called with
prefix argument."
  (interactive "p")
  (if (= arg 4)
      (call-interactively #'deft-new-file-named)
    (deft-new-file)))

;; Don't ever auto populate the title
(advice-add 'deft-auto-populate-title-maybe :around #'list)

(defun zettel-add-section-sign-to-deft-filter ()
  "Inserts the Unicode section sign (§) to Deft filter string."
  (interactive)
  (setq last-command-event 167)
  (deft-filter-increment))

(defun deft-filter-zettel-category (category arg)
  "Inserts a category into deft-filter if there is no category there or
changes the existing one. With prefix argument, replaces the current
`deft-filter-regexp'."
  (interactive (list (zettel-ivy-read-category nil nil #'>)
                     current-prefix-arg))
  (deft-filter (format "{%s}" category)
    (or arg (null deft-filter-regexp))))

;;
;; Insert my zettel title string into new zettel rather than contents of deft's
;; filter string.
;;
(defun zettel-insert-metadata-template (category title)
  "Inserts the metadata template into the current buffer."
  (interactive (list (zettel-ivy-read-category nil nil #'>)
                     (read-string "Zettel title: ")))
  (let ((file (file-name-base buffer-file-name))
        (link (zettel-file-link buffer-file-name)))
    (when (zerop (buffer-size))
      (insert (format "title: §%s. {%s} %s" link (or category "Unset") (or title "Untitled")))
      (insert "\ncreated: "
              ;; Insert creation time, making it match a tempus currens filename
              (format-time-string
               "%Y-%m-%d %a %H:%M"
               (let ((today (format-time-string "%Y%m%d")))
                 (if (and (eq :tempus (zettel-type buffer-file-name))
                          (not (string-match-p (regexp-quote today) file))
                          (not
                           (when (called-interactively-p 'any)
                             (y-or-n-p "Past tempus currens; set created time to now? "))))
                     (zettel-encode-iso8601-datetime file)
                   nil)))
              "\n")                     ; i.e. current time
      (when (assoc link zettel-parent-of-new-child)
        (insert "parent: " (cdr (assoc link zettel-parent-of-new-child)) "\n"))
      (insert "\n"))))

(defun zettel-incorporate-file (file kasten &optional arg)
  "Moves the file in the current buffer to the appropriate Zettelkasten. With
prefix argument, asks for a different name."
  (interactive (list (buffer-file-name)
                     (ivy-read "Zettel kasten: " zettel-kaesten)
                     current-prefix-arg))
  (rename-file-and-buffer
   (if (not arg)
       (zettel-absolute-filename
        (zettel-make-link kasten (file-name-base file)))
     (call-interactively #'rename-file-and-buffer))))

(defun zettel-adv--deft-new-file-insert-metadata (orig-fun slug)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; `DEFT-NEW-FILE-NAMED' returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun slug))
    (let ((file (deft-absolute-filename slug)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        (call-interactively #'zettel-insert-metadata-template)))))
(advice-add 'deft-new-file-named :around #'zettel-adv--deft-new-file-insert-metadata)

(defun zettel-adv--deft-absolute-filename (orig-fun &rest args)
  "Replaces the default `deft-absolute-filename' with
`zettel-absolute-filename'."
  (let ((kasten (zettel-directory-kasten deft-directory)))
    (zettel-absolute-filename
     (if kasten
         (concat kasten ":" (first args))
       (first args)))))
(advice-add 'deft-absolute-filename :around 'zettel-adv--deft-absolute-filename)

;;
;; Sorting deft-buffer by filename
;;
(defun deft-sort-files-by-name (files)
  "Sort FILES by name, in reverse, ignoring case."
  (sort files (lambda (f1 f2)
                (funcall (if zettel-sort-by-name-descending
                             #'string-lessp
                           #'string-greaterp)
                         (downcase (file-name-base f2))
                         (downcase (file-name-base f1))))))

(defun zettel-title-lessp (file1 file2)
  "Return non-nil if the Zettel title of FILE1 is lexicographically less than
that of FILE2. Case is ignored."
  ;; FIXME: Hack to get the title from the result of
  ;; `zettel-deft-parse-title-function'
  (let ((title1 (third (split-string (or (deft-file-title file1) "") "  +")))
        (title2 (third (split-string (or (deft-file-title file2) "") "  +"))))
    (string-lessp (downcase (or title1 ""))
                  (downcase (or title2 "")))))

(defun deft-sort-files-by-zettel-title (files)
  "Sort FILES by the Zettel title."
  (sort files 'zettel-title-lessp))

(eval-after-load "deft"
  '(defalias 'deft-sort-files-by-title 'deft-sort-files-by-name))

;; Having a visual indicator of the sort method is helpful
(defun deft-set-mode-name ()
  "Set the mode line text based on search mode."
  (setq mode-name
    (format "Deft[%s]%s"
            deft-current-sort-method
            (if deft-incremental-search "" "/R"))))
(advice-add 'deft-toggle-sort-method :after 'deft-set-mode-name)

(defun zettel-adv--deft-open-button (orig-fun &rest args)
  "Advice :around `deft-open-button' to call `zettel-find-link' instead of
`deft-open-file'."
  (zettel-find-file (button-get (first args) 'tag) current-prefix-arg))
(advice-add 'deft-open-button :around #'zettel-adv--deft-open-button)

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun zettel-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current heading to
facilitate refiling."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM" (zettel-insert-link-with-metadata
                            buffer-file-name :title :after))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun zettel-org-interactive-tempus ()
  "Inserts a tempus currens link after having the user select the date using
org-mode's interactive `org-time-stamp' command."
  (interactive)
  (let ((start (point)))
    (org-time-stamp '(4) t)
    (insert
     "[["
     (org-timestamp-format (org-timestamp-from-string
                            (delete-and-extract-region start (point)))
                           "%Y%m%dT%H%M")
     "]]")))

;; TODO: Rewrite the following three functions based on the one above
(defun zettel-convert-org-timestamp-to-iso8601 ()
  "Convert the org-mode timestamp at point to the compact ISO 8601 form."
  (interactive)
  (let ((regexp "\\[\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) [A-Za-z]\\{3\\} \\([0-9]+\\):\\([0-9]+\\)\\]"))
    (when (thing-at-point-looking-at regexp)
      (let ((link (format "[[%s%s%sT%s%s]]"
                          (match-string 1)
                          (match-string 2)
                          (match-string 3)
                          (match-string 4)
                          (match-string 5))))
        (delete-region (match-beginning 0) (match-end 0))
        (insert link)))))

(defun zettel-convert-iso8601-to-org-timestamp ()
  "Convert the compact ISO 8601 timestamp at point to the org-mode timestamp."
  (interactive)
  (when (thing-at-point-looking-at zettel-regexp-iso8601-datetime)
    ;; FIXME: Cheating to use "Day"
    (let ((link (format "[%s-%s-%s Day %s:%s]"
                        (match-string 1)
                        (match-string 2)
                        (match-string 3)
                        (match-string 4)
                        (match-string 5))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert link))))

(defun zettel-org-timestamp-from-iso8601 (string)
  "Returns an org-mode timestamp from the given ISO8601 timestamp."
  (let ((regexp "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)"))
    (when (string-match regexp string)
      ;; FIXME: Cheating to use "Day"
      (format "[%s-%s-%s Day %s:%s]"
              (match-string 1 string)
              (match-string 2 string)
              (match-string 3 string)
              (match-string 4 string)
              (match-string 5 string)))))

(defun zettel-org-include-cached-file ()
  "Add an org-mode #+INCLUDE to a cached Zettel."
  (interactive)
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (deft-file-title path) path))
                                (zettel-visiting-buffer-list t))
                        (zettel-ivy-titles-reverse-alist)))))
    (if choices
        (zettel-ivy-read-reverse-alist-action
         "Include: "
         choices
         (lambda (file)
           (let ((summary-section "Summary")
                 (snippet-section (replace-regexp-in-string
                                   "ß" "Snippet "
                                   (first (split-string
                                           (alist-get :title (zettel-metadata file))
                                           ":")))))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t\n"
                             (file-relative-name file)
                             summary-section))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t"
                             (file-relative-name file)
                             snippet-section)))))
      (user-error "No Deft cache or visited Zettel"))))

(defun zettel-org-export-as-new-zettel ()
  "Creates a new Zettel file in the current Zettelkasten based on the current
org subtree."
  (interactive)
  (let ((parent-file buffer-file-name)
        (parent-link (zettel-file-link buffer-file-name))
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
                new-file (zettel-absolute-filename tempus-currens))
          (let* ((content (org-get-entry)))
            ;; Code adapted from `deft-new-file', since calling that function in
            ;; turn calls my `zettel-adv--deft-new-file-insert-metadata' that interferes
            ;; with populating the file properly.
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              (deft-open-file new-file)
              (with-current-buffer (get-file-buffer (file-truename new-file))
                (insert (format "title: §%s. {Memo} %s\n" tempus-currens new-title))
                (insert "created: " (format-time-string "%Y-%m-%d") "\n")
                (insert "parent: " parent-link "\n")
                (insert content)
                (save-buffer))
              (deft-cache-update-file new-file)
              (deft-refresh-filter))))))
    ;; Back in original buffer
    (with-current-buffer (get-file-buffer (file-truename parent-file))
      (org-cut-subtree)
      (insert (zettel-org-format-link (zettel-file-link new-file))
              " "
              (alist-get :title (zettel-metadata new-file))))))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'zettel-find-link org-open-link-functions)

     ;; Do the same for Zettel links that lack even the link markup. This is
     ;; useful for following parents/children.
     (push #'zettel-open-link-at-point org-open-at-point-functions)

     ;; This allows following links as part of #+INCLUDE statements.
     ;; TODO: Add a function to follow #+INCLUDE links
     ))

;; Treat : (colon) as part of the word, allowing forward/backward-word over full
;; Zettel links.
(add-hook 'zettel-mode-hook
  '(lambda ()
     (modify-syntax-entry ?: "w")))

(defun zettel-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link. With a
prefix argument, ignore `zettel-number-of-frames' and open the link in the
same window."
  (interactive "P")
  (save-excursion
    (forward-word)
    (backward-word)
    (when (thing-at-point-looking-at (concat "\\(" zettel-regexp-link "\\)"))
      ;; FIXME: Is it okay to check like this for prefix arg "upstream"?
      (zettel-find-link (match-string-no-properties 1) current-prefix-arg)
      ;; This function is later added to `org-open-at-point-functions', so "must
      ;; return t if they identify and follow a link at point. If they don’t find
      ;; anything interesting at point, they must return nil."
      t)))

(defun org-zettel-link-context (file)
  "Returns a string of Zettel context."
  (if (zettel-p file)
      (format "[[%s]] %s"
              (file-name-base file)
              (or (alist-get :title (zettel-metadata file)) ""))
    ;; (format "[[%s][%s]]" file (file-name-base file))
    (error "Not a Zettel")))

;;;=============================================================================
;;; Markdown-Mode Integration
;;;=============================================================================

(eval-after-load 'markdown-mode
  '(progn
     ;; This must be enabled to have wiki links
     (setq markdown-enable-wiki-links t)

     ;;
     ;; By default, `markdown-convert-wiki-link-to-filename' concatenates the
     ;; file extension of the current buffer's file to the link name when you
     ;; press C-c C-o over something like [[bib/Key2015.bib]], so it ends up
     ;; opening Key2015.bib.txt. The markdown-cwltf-fix-link removes the extra
     ;; extension, among other things.
     ;;
     ;; Unfortunately, `markdown-follow-wiki-link' also "ensure[s] that the new
     ;; buffer remains in `markdown-mode'", so I need yet another work-around to
     ;; fix that: `markdown-fwl-set-auto-mode'.
     ;;

     (defun markdown-fwl--set-auto-mode (&rest args)
       "After advice for `markdown-follow-wiki-link'. Reverses the default
behavir of ensuring that the buffer is in markdown mode, and instead sets it
back to the mode it 'wants to be'."
       (set-auto-mode t))
     (advice-add 'markdown-follow-wiki-link :after #'markdown-fwl--set-auto-mode)

     (defun markdown-cwltf--fix-link (orig-fun name)
       "Advice for `markdown-convert-wiki-link-to-filename',
completely overriding the originall functionality. It combines the not
clobbering of extension, finding the right directory directory for the Zettel
so that the links can be found across multiple directories within the main
Zettelkasten, and also handling 'subkasten:' notation."
       (save-match-data
         (let ((result (or (zettel-absolute-filename name)
                           (funcall orig-fun name))))
           (if (string-match "\\.\\w+$" name)
               (let ((orig-ext (match-string 0 name)))
                 (if (string-match (concat orig-ext "\\(\\.\\w+\\)$") result)
                     (replace-match orig-ext nil nil result)
                   result))
             result))))

     (advice-add 'markdown-convert-wiki-link-to-filename
                 :around #'markdown-cwltf--fix-link)))

;;;=============================================================================
;;; Citar Integration
;;;=============================================================================

(defvar zettel-bibliographic-note-directory (in-zettel-dir "reticulum")
  "Directory where `citar-open-notes' can find my notes about the source.")

(defun zettel-symlink-bibliographic-note (arg file citekey)
  "Creates a symbolic link from the given Zettel file to the specified
citekey. With prefix argument, replace the existing link."
  (interactive
   (list current-prefix-arg
         buffer-file-name
         (let ((citekey (alist-get :citekey
                                   (zettel-metadata buffer-file-name))))
           (if (string-match "^@*\\([[:alnum:]-]+\\)" citekey)
               (match-string 1 citekey)
             (read-string "Citekey (without @): ")))))
  (let ((note-name (concat citekey "." deft-default-extension)))
    (when (y-or-n-p (format "Create the symlink '%s' to this note? " note-name))
      (make-symbolic-link
       (file-relative-name file zettel-bibliographic-note-directory)
       (expand-file-name note-name zettel-bibliographic-note-directory)
       arg))))

;;;=============================================================================
;;; Bookmark Integration
;;;=============================================================================

(defun bookmark-make-record-zettel ()
  "Bookmark record function for Zettel bookmarks, setting the
bookmark's filename property to the Zettel link."
  (list (cons 'filename (zettel-file-link buffer-file-name))
        (cons 'handler 'bookmark-zettel-handler)))

(defun bookmark-zettel-handler (bmk-record)
  "Bookmark record handler for Zettel bookmarks."
  (find-file (zettel-absolute-filename (cdr (assoc 'filename bmk-record)))))

;; Use the special zettel bookmark handler in Zettel buffers
(add-hook 'zettel-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-zettel)))

;;;=============================================================================
;;; Frames
;;;=============================================================================

(defun zettel-formatted-frame-title ()
  "Returns a string suitable for `frame-title-format' as a way to
consistently format the frame title with useful information for
Zettelkasten work."
  (interactive)
  (concat (if zettel-deft-active-kasten
              (format "〔%s〕"
                      (upcase zettel-deft-active-kasten))
            "")
          (if (zettel-p buffer-file-name)
              (let ((metadata (zettel-metadata buffer-file-name)))
                (format "%s §%s@%s"     ; {%s} (alist-get :category metadata)
                        (alist-get :title metadata)
                        (alist-get :slug metadata)
                        (alist-get :kasten metadata)))
            "%b")))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defun zettel-zmove-to-another-kasten (source-file &optional target-link)
  "Generates a zmove shell command to move the current Zettel to another
kasten. With prefix argument, asks for a target link instead. Returns the
target link."
  (interactive (list (cond (zettel-mode
                            buffer-file-name)
                           ((eq major-mode 'magit-status-mode)
                            (magit-file-at-point))
                           ((eq major-mode 'deft-mode)
                            (button-get (button-at (point)) 'tag))
                           (t
                            (read-file-name "Move which Zettel? ")))))
  (let ((source-link (zettel-file-link source-file)))
    (if (and (not target-link) (called-interactively-p 'any))
        (if (equal current-prefix-arg '(4))
            (read-string "Enter target link: ")
          (let ((kasten (ivy-read "Which kasten to move to? "
                                  zettel-kaesten)))
            (setq target-link
              (zettel-make-link
               kasten
               (case (second (assoc kasten zettel-kaesten #'string=))
                 (:numerus (zettel-next-unused-slug :numerus))
                 (:tempus (zettel-tempus-currens-slug-for source-link))
                 (t
                  (error "Don't know how to handle this")))))))
      (error "Don't know where to move %s" source-link))
    (shell-command (format "zmove %s %s" source-link target-link))
    (cond ((string= source-file buffer-file-name)
           (kill-this-buffer)
           (unless (eq (length (frame-list)) 1)
             (delete-frame)))
          ((eq major-mode 'magit-status-mode)
           (magit-refresh))
          ((eq major-mode 'deft-mode)
           (deft-cache-update-file source-file)))
    target-link))

(defun zettel-filter-for-link-at-point ()
  "Modifies the Deft filter to look for the Zettel linked with
the link at point. If there is only one match, opens the note in
another window."
  (interactive)
  (when (zettel-link-at-point-p)
    (let ((link (zettel-link-at-point))
          (deft-incremental-search nil))
      (deft-filter (concat "^oldnames: \\[.*" link ".*\\]$") t)
      (unless deft-current-files
        (deft-filter (concat "§" link ".") t))
      (cond ((= (length deft-current-files) 1)
             (deft-open-file (first deft-current-files) t t))
            ((null deft-current-files)
             (message "No notes with current or old name matching `%s'" link))
            (t
             (switch-to-buffer-other-window deft-buffer))))))

(defun zettel-generate-n-new-slugs (how-many type)
  "Generates a bunch of new slugs, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (intern (ivy-read "Which type? "
                           (mapcar #'first zettel-default-kasten)))))
  (goto-char (point-max))
  (let (slugs)
    (dotimes (n how-many)
      (push (zettel-generate-new-slug type) slugs))
    (mapc #'(lambda (s)
              (insert s "\n"))
          (delete-dups slugs))
    (delete-duplicate-lines (point-min) (point-max))))

;; Based on https://stackoverflow.com/a/30328255
(defun magit-show-zettel-title-in-minibuffer ()
  "Displays Zettel title of the file under cursor in minibuffer."
  (while-no-input
    (redisplay)
    (let (file line)
      (when (and (eq major-mode 'magit-status-mode)
                 (setq file (magit-file-at-point))
                 (zettel-p file)
                 (setq line (magit-file-line file)))
        (let ((metadata (zettel-decode-combined-title
                         (first (split-string line "title: " t)))))
          (message "%s | %s"
                   (alist-get :slug metadata) (alist-get :title metadata)))))))
(add-hook 'post-command-hook 'magit-show-zettel-title-in-minibuffer)

;;;-----------------------------------------------------------------------------
;;; Zettel-Mode Key Bindings
;;;
;; According to key binding conventions, the only bindings reserved for minor
;; modes are "Sequences consisting of C-c followed by any other punctuation
;; character" than {, }, <, >, : or ;, which are reserved for major modes.
;;;-----------------------------------------------------------------------------

(define-key zettel-mode-map (kbd "C-c ^") 'zettel-find-ancestor)
(define-key zettel-mode-map (kbd "C-c _") 'zettel-find-descendant)
(define-key zettel-mode-map (kbd "C-c @") 'zettel-insert-ancestor-link)
(define-key zettel-mode-map (kbd "C-c ,") 'zettel-insert-new-child)
(define-key zettel-mode-map (kbd "C-c ~") 'zettel-kill-ring-save-link-title)
(define-key zettel-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key zettel-mode-map (kbd "C-c $") 'zettel-kill-ring-save-link-at-point)
;; This shadows org-mode's `org-agenda-file-to-front'
(define-key zettel-mode-map (kbd "C-c [") 'zettel-update-title)

;;
;; Unsafe mode keybindings
;;
(define-key zettel-mode-map (kbd "C-c C-s") 'zettel-select-link)
(define-key zettel-mode-map (kbd "C-c C-g") 'zettel-avy-link-search)
(define-key zettel-mode-map (kbd "C-c C-d") 'zettel-kill-ring-save-link-title)
(define-key zettel-mode-map (kbd "C-c C-'") 'zettel-set-category)

;; Shadows the default `kill-sexp'
(define-key zettel-mode-map (kbd "C-M-k") 'zettel-kill-link-or-sexp-at-point)

;; Shadows Org-mode's global "C-c l" and local "C-c C-l"
(define-key deft-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c C-l") 'zettel-insert-link-to-cached-or-visiting)
(define-key zettel-mode-map (kbd "C-c C-M-l") 'zettel-insert-link-from-clipboard)

;; Shadows org-mode's `org-ctrl-c-tab'
;;(define-key zettel-mode-map (kbd "C-c C-i") 'zettel-org-include-cached-file)

;; Shadows org-mode's `org-set-property-and-value'
(define-key zettel-mode-map (kbd "C-c C-x P") 'zettel-ivy-set-parent)
(define-key zettel-mode-map (kbd "C-c C-x F") 'zettel-org-set-todo-properties)

;; Ztools interaction
(define-key zettel-mode-map (kbd "C-c C-x z") 'zettel-zmove-to-another-kasten)
;; Shadows org-mode's `org-toggle-ordered-property'
(define-key zettel-mode-map (kbd "C-c C-x l") 'zettel-links-to)

;;;-----------------------------------------------------------------------------
;;; Deft-Mode Keybindings
;;;-----------------------------------------------------------------------------

;; "Shadow" the built-in slug generator to generate timestamps by default,
;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
(eval-after-load "deft"
  '(defalias 'deft-unused-slug 'zettel-next-unused-slug))
(define-key deft-mode-map (kbd "C-c C-S-n") 'deft-new-unused-zettel)

(define-key deft-mode-map (kbd "C-c s") 'zettel-add-section-sign-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-named)
(define-key deft-mode-map (kbd "C-c C-o") 'push-button)
(define-key deft-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c C-f") 'zettel-select-link) ; Was: deft-find-file
(define-key deft-mode-map (kbd "C-c C-'") 'deft-filter-zettel-category)
(define-key deft-mode-map (kbd "C-c C-p") 'zettel-populate-categories)
;; Was: deft-filter-clear
(define-key deft-mode-map (kbd "C-c C-c") 'zettel-set-category)

(provide 'zettel)
