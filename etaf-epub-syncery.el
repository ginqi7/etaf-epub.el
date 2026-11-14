;;; etaf-epub-syncery.el --- Syncery reading progress  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: hypermedia, ebooks

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

;; Syncery is a KOReader plugin that shares reading progress between
;; devices.  It keeps a `*.syncery-progress.json' in the book's `.sdr'
;; folder, holding a percentage and, for reflowable documents, the
;; XPointer of the last read text:
;;
;;   {"entries": {"3B0...": {"label": "Phone",
;;                           "percent": 0.42,
;;                           "xpath": "/body/DocFragment[7]/...text().12",
;;                           ...}}}
;;
;; The `xpath' field is a CREngine XPointer, the same format handled by
;; `etaf-epub-xpointer'.  It is preferred over the percentage because it
;; names the text that was read, while the percentage only names a place
;; in an estimate.  Entries are keyed by device id and carry a revision
;; and a timestamp.  Writing from Emacs adds or updates its own entry,
;; so a reader never mistakes Emacs's position for its own.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(require 'etaf-epub)
(require 'etaf-epub-progress)
(require 'etaf-epub-xpointer)

(defgroup etaf-epub-syncery nil
  "Read and write Syncery reading progress."
  :group 'etaf-epub)

(defcustom etaf-epub-syncery-local-device-name "Emacs"
  "Device label Syncery uses for progress written from Emacs.
The entry is stored under the upper-case MD5 of this name, which is
how the Syncery plugin derives device ids.  A separate entry matters:
other devices only offer a jump to the most recent entry that is not
their own."
  :type 'string
  :group 'etaf-epub-syncery)

(defcustom etaf-epub-syncery-remote-device-name nil
  "Label of the device whose progress is read, or nil for the newest.
`etaf-epub-syncery-goto' uses this entry.  `save-progress' uses it,
or the newest entry when this is nil, as the source of `page' and
`total_pages' hints for the entry it writes."
  :type '(choice (const :tag "Newest entry" nil)
                 (string :tag "Device label"))
  :group 'etaf-epub-syncery)

;;; The progress file

(defun etaf-epub-syncery--directory ()
  "Return the `.sdr' directory that belongs to the current EPUB, or nil."
  (let* ((epub (or etaf-epub-file-name buffer-file-name))
         (directory (and epub (file-name-directory epub)))
         (name (and epub (file-name-nondirectory epub)))
         (candidates
          (delq nil
                (list ;; KOReader strips the extension when naming the
                      ;; settings directory: `Book.sdr' for `Book.epub'.
                      (and epub
                           (expand-file-name
                            (concat (file-name-nondirectory
                                     (file-name-sans-extension epub))
                                    ".sdr")
                            directory))
                      (and epub
                           (expand-file-name (concat name ".sdr") directory))
                      (car (directory-files default-directory t "sdr\\'"))))))
    (seq-find (lambda (directory)
                (and directory (file-directory-p directory)))
              candidates)))

(defun etaf-epub-syncery-progress-file ()
  "Return the Syncery progress file of the current EPUB, or nil."
  (when-let* ((directory (etaf-epub-syncery--directory)))
    (seq-find #'file-readable-p
              (directory-files directory t "syncery-progress\\.json\\'"))))

(defun etaf-epub-syncery-progress ()
  "Return the Syncery progress data of the current EPUB, or nil."
  (when-let* ((file (etaf-epub-syncery-progress-file)))
    (condition-case nil
        (json-parse-string (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))
      (json-error
       (etaf-epub-warn (format "Could not parse Syncery progress file %s"
                               file))
       nil))))

;;; Reading

(defun etaf-epub-syncery--entries ()
  "Return the per-device entry table of the current EPUB, or nil."
  (when-let* ((progress (etaf-epub-syncery-progress)))
    (gethash "entries" progress)))

(defun etaf-epub-syncery--entry-timestamp (entry)
  "Return ENTRY's Syncery timestamp as a number, or 0."
  (let ((timestamp (gethash "timestamp" entry)))
    (if (numberp timestamp) timestamp 0)))

(defun etaf-epub-syncery--select-entry (entries)
  "Return the entry in ENTRIES to read and update, or nil.
This is the entry labelled `etaf-epub-syncery-remote-device-name'
when that is set, and the most recently updated entry otherwise."
  (when entries
    (let ((values (hash-table-values entries)))
      (or (and etaf-epub-syncery-remote-device-name
               (seq-find (lambda (entry)
                           (equal (gethash "label" entry)
                                  etaf-epub-syncery-remote-device-name))
                         values))
          (seq-reduce
           (lambda (newest entry)
             (if (> (etaf-epub-syncery--entry-timestamp entry)
                    (etaf-epub-syncery--entry-timestamp newest))
                 entry
               newest))
           (cdr values)
           (car values))))))

(defun etaf-epub-syncery--entry ()
  "Return the Syncery entry to read and update, or nil."
  (when-let* ((entries (etaf-epub-syncery--entries)))
    (etaf-epub-syncery--select-entry entries)))

(defun etaf-epub-syncery-percent ()
  "Return the reading percentage Syncery recorded, or nil.
It is a number between 0 and 1."
  (when-let* ((entry (etaf-epub-syncery--entry))
              (percent (gethash "percent" entry)))
    (and (numberp percent) percent)))

(defun etaf-epub-syncery-xpath ()
  "Return the CREngine XPointer from the `xpath' field Syncery recorded.
Older progress files may name the field `xpointer' instead.  Return
nil when the entry has no xpath, as for paginated documents."
  (when-let* ((entry (etaf-epub-syncery--entry)))
    (seq-some (lambda (key)
                (let ((xpath (gethash key entry)))
                  (and (stringp xpath)
                       (not (string-empty-p xpath))
                       xpath)))
              '("xpath" "xpointer"))))

(defun etaf-epub-syncery-xpointer ()
  "Return the XPointer Syncery recorded, or nil.
This is an alias for `etaf-epub-syncery-xpath'."
  (etaf-epub-syncery-xpath))

(defun etaf-epub-syncery-goto ()
  "Go to the reading position recorded by Syncery."
  (interactive)
  (if-let* ((xpath (etaf-epub-syncery-xpath)))
      (etaf-epub-xpointer-goto xpath)
    (if-let* ((percent (etaf-epub-syncery-percent)))
        (etaf-epub-progress-goto (* 100 percent))
      (user-error "No Syncery progress found for this EPUB"))))

;;; Writing

(defun etaf-epub-syncery--local-device-id ()
  "Return the Syncery device id used for progress written from Emacs."
  (upcase (md5 etaf-epub-syncery-local-device-name)))

(defun etaf-epub-syncery--entry-total-pages (entry)
  "Return ENTRY's positive `total_pages', or nil."
  (let ((total (and entry (gethash "total_pages" entry))))
    (and (numberp total) (> total 0) total)))

(defun etaf-epub-syncery--any-total-pages (entries)
  "Return a positive `total_pages' found in ENTRIES, or nil."
  (seq-some #'etaf-epub-syncery--entry-total-pages
            (hash-table-values entries)))

(defun etaf-epub-syncery-save-progress (&optional xpath)
  "Save the current reading position as XPATH in Syncery's progress file.
XPATH defaults to `etaf-epub-xpointer-at-point', the CREngine
XPointer of the text at point.  Write the entry of
`etaf-epub-syncery-local-device-name', which defaults to Emacs, so
that other devices see it as a peer and can offer the jump; never
modify another device's entry.

`percent' is estimated from the text position.  `page' and
`total_pages' are copied or derived from an existing entry so that a
failed xpath does not fall back to page 1.  `revision' and
`timestamp' make the entry the newest progress.

Interactively, prompt for the XPointer to store.  Return the entry
that was written."
  (interactive
   (list (read-string "XPointer: " (etaf-epub-xpointer-at-point))))
  (let* ((file (or (etaf-epub-syncery-progress-file)
                   (user-error "No Syncery progress file for this EPUB")))
         (progress (or (etaf-epub-syncery-progress)
                       (user-error "Could not parse Syncery progress file %s"
                                   file)))
         (entries (or (gethash "entries" progress)
                      (puthash "entries" (make-hash-table :test #'equal)
                               progress)))
         (id (etaf-epub-syncery--local-device-id))
         (entry (or (gethash id entries)
                    (puthash id (make-hash-table :test #'equal) entries)))
         (reference (etaf-epub-syncery--select-entry entries))
         (xpath (or xpath (etaf-epub-xpointer-at-point)))
         (percent (max 0.0
                       (min 1.0 (/ (etaf-epub-progress-percent) 100.0))))
         (total-pages (or (etaf-epub-syncery--entry-total-pages entry)
                          (etaf-epub-syncery--entry-total-pages reference)
                          (etaf-epub-syncery--any-total-pages entries)
                          0))
         (page (if (> total-pages 0)
                   (max 1 (min total-pages
                               (truncate (* total-pages percent))))
                 (let ((page (and reference (gethash "page" reference))))
                   (if (numberp page) (max 1 page) 1)))))
    (puthash "device_id" id entry)
    (puthash "label" etaf-epub-syncery-local-device-name entry)
    (puthash "file" (or etaf-epub-file-name
                        buffer-file-name
                        (and reference (gethash "file" reference)))
             entry)
    (puthash "is_rolling" t entry)
    (puthash "xpath" xpath entry)
    (puthash "percent" percent entry)
    (puthash "total_pages" total-pages entry)
    (puthash "page" page entry)
    (puthash "revision" (1+ (or (gethash "revision" entry) 0)) entry)
    (puthash "timestamp" (time-convert nil 'integer) entry)
    (with-temp-file file
      (insert (json-serialize progress) "\n"))
    (message "Saved Syncery progress at %s" xpath)
    entry))

;;; Key bindings

;; `s' is free in `etaf-epub-mode-map'; the other reader keys are bound
;; in `etaf-epub', `etaf-epub-progress' and `etaf-epub-xpointer'.
(define-key etaf-epub-mode-map (kbd "s") #'etaf-epub-syncery-save-progress)

(provide 'etaf-epub-syncery)
;;; etaf-epub-syncery.el ends here
