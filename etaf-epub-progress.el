;;; etaf-epub-progress.el --- Percentage reading progress  -*- lexical-binding: t; -*-

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

;; Reading progress stored as a percentage of the whole book, which is
;; what KOReader, Syncery and most other readers record as well.  The
;; length of the book is estimated from the amount of text in its
;; documents and the position inside a document from the rendered
;; page, so a percentage can only ever be approximate: prefer
;; `etaf-epub-xpointer-goto' when the progress comes with an XPointer.

;;; Code:

(require 'dom)
(require 'seq)

(require 'etaf-epub)
(require 'etaf-epub-xpointer)

(defun etaf-epub-progress-length (path)
  "Return the number of characters of text in the document at PATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents path)
        (let* ((dom (or (libxml-parse-xml-region (point-min) (point-max))
                        (libxml-parse-html-region (point-min) (point-max))))
               (root (etaf-epub-xpointer--root dom)))
          (length (etaf-epub-xpointer--text root))))
    (error 0)))

(defun etaf-epub-progress-documents ()
  "Return (INDEX . LENGTH) for the readable documents, in reading order.
The TOC document is left out, as are documents without any text."
  (let (result)
    (dotimes (i (length etaf-epub-documents))
      (unless (eq (car (aref etaf-epub-documents i)) etaf-epub-toc-id)
        (let ((length (etaf-epub-progress-length
                       (cdr (aref etaf-epub-documents i)))))
          (when (> length 0)
            (push (cons i length) result)))))
    (nreverse result)))

(defun etaf-epub-progress--ratio ()
  "Return how far into the current document point is, between 0 and 1."
  (/ (float (- (point) (point-min)))
     (max 1 (- (point-max) (point-min)))))

(defun etaf-epub-progress-goto (percent)
  "Jump to PERCENT of the way through the current EPUB.
PERCENT is a number between 0 and 100.  The length of the book is
estimated from the amount of text in its documents, and the position
inside the target document is interpolated over its rendered page."
  (interactive "nJump to percent (0-100): ")
  (unless (and (numberp percent) (<= 0 percent 100))
    (user-error "Percent must be between 0 and 100"))
  (let* ((documents (etaf-epub-progress-documents))
         (total (apply #'+ (mapcar #'cdr documents)))
         (target (* percent 0.01 total))
         (offset 0)
         index ratio)
    (when (zerop total)
      (user-error "Cannot estimate the length of this EPUB"))
    (catch 'done
      (dolist (document documents)
        (let ((length (cdr document)))
          (if (>= (+ offset length) target)
              (progn
                (setq index (car document)
                      ratio (/ (- target offset) (float length)))
                (throw 'done nil))
            (setq offset (+ offset length)))))
      ;; Rounding can leave the target just past the end of the book.
      (setq index (car (car (last documents)))
            ratio 1.0))
    (etaf-epub-goto-document index)
    (etaf-epub-xpointer--goto-ratio (min 1.0 (max 0.0 ratio)))
    (when (get-buffer-window (current-buffer))
      (recenter (1- (max 1 scroll-margin))))
    (point)))

(defun etaf-epub-progress-percent ()
  "Return how far into the current EPUB point is, as a number 0-100."
  (let* ((documents (etaf-epub-progress-documents))
         (total (max 1 (apply #'+ (mapcar #'cdr documents))))
         (index etaf-epub-documents-index)
         (ratio (etaf-epub-progress--ratio))
         (before 0)
         (length 0))
    (catch 'done
      (dolist (document documents)
        (when (= (car document) index)
          (setq length (cdr document))
          (throw 'done nil))
        (setq before (+ before (cdr document)))))
    (* 100 (/ (+ before (* length ratio)) (float total)))))

;;; Key bindings

(define-key etaf-epub-mode-map (kbd "%") #'etaf-epub-progress-goto)

(provide 'etaf-epub-progress)
;;; etaf-epub-progress.el ends here
