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
;; devices.  It keeps a `syncery-progress.json' in the book's `.sdr'
;; folder, holding a percentage and, in newer versions, an XPointer.
;; The XPointer is preferred: it names the paragraph that was read,
;; while the percentage only names a place in an estimate.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(require 'etaf-epub)
(require 'etaf-epub-progress)
(require 'etaf-epub-xpointer)

(defun etaf-epub-syncery--directory ()
  "Return the `.sdr' directory that belongs to the current EPUB, or nil."
  (let* ((epub (or etaf-epub-file-name buffer-file-name))
         (candidates
          (delq nil
                (list (and epub
                           (expand-file-name
                            (concat (file-name-nondirectory epub) ".sdr")
                            (file-name-directory epub)))
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
    (json-parse-string (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))))

(defun etaf-epub-syncery--entry ()
  "Return the Syncery entry that belongs to the current EPUB, or nil."
  (when-let* ((progress (etaf-epub-syncery-progress)))
    (car (hash-table-values (gethash "entries" progress)))))

(defun etaf-epub-syncery-percent ()
  "Return the reading percentage Syncery recorded, or nil.
It is a number between 0 and 1."
  (when-let* ((entry (etaf-epub-syncery--entry)))
    (gethash "percent" entry)))

(defun etaf-epub-syncery-xpointer ()
  "Return the XPointer Syncery recorded, or nil."
  (when-let* ((entry (etaf-epub-syncery--entry)))
    (seq-find #'stringp
              (mapcar (lambda (key)
                        (when (string-match-p "xpointer" key)
                          (gethash key entry)))
                      (hash-table-keys entry)))))

(defun etaf-epub-syncery-goto ()
  "Go to the reading position recorded by Syncery."
  (interactive)
  (if-let* ((xpointer (etaf-epub-syncery-xpointer)))
      (etaf-epub-xpointer-goto xpointer)
    (if-let* ((percent (etaf-epub-syncery-percent)))
        (etaf-epub-progress-goto (* 100 percent))
      (user-error "No Syncery progress found for this EPUB"))))

(provide 'etaf-epub-syncery)
;;; etaf-epub-syncery.el ends here
