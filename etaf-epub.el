;;; etaf-epub.el --- EPUB reader built on ETAF  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; URL: https://github.com/ginqi7/etaf-epub.el
;; Keywords: hypermedia, ebooks
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.3.5") (esxml "0.3.8"))

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

;; An EPUB reader that renders each spine document with `etaf-html' and
;; mounts the resulting ETAF View in the EPUB buffer itself.  Because the
;; rendered page and the EPUB state (`etaf-epub-documents', the TOC file,
;; metadata, ...) live in the same buffer, link callbacks can navigate
;; between documents using the buffer-local state, and reading positions
;; are ordinary buffer positions.
;;
;; Opening an EPUB file creates a temporary working directory, extracts
;; the archive there, parses the OPF container and renders the first spine
;; document.  The mode also records the last reading place, and integrates
;; with bookmarks, Org links, Imenu and multi-isearch.
;;
;; The heavy lifting of HTML translation is left to `etaf-html'; this file
;; only adapts its mounting step so that:
;;
;; - the View is mounted into the EPUB buffer rather than a side buffer;
;; - links call `etaf-epub-browse-url', which knows how to resolve EPUB
;;   relative links and open external URLs;
;; - RET and mouse-2 on rendered text follow the link at point, including
;;   inline links that `etaf-html' renders as propertized text;
;; - a spine item that is an image is mounted with ETAF's `image' Host.
;;
;; Companion libraries in the same project:
;;
;; - `etaf-epub-xpointer' reads and writes reading positions in the
;;   XPointer format used by CREngine and KOReader;
;; - `etaf-epub-progress' jumps by percentage of the whole book;
;; - `etaf-epub-syncery' reads and writes the progress recorded by the
;;   KOReader plugin of the same name.
;;
;; A mounted Ebox canvas is application output: Ebox claims the movement
;; keys for its own focus model and installs a keymap on every painted
;; node, so text cannot be selected and a click does not place point.
;; `etaf-epub-user-keys-mode' removes all of that for as long as it is
;; on, which makes the page ordinary read-only text that follows the
;; user's own key bindings; turning it off restores the canvas.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'dom)
(require 'esxml-query)
(require 'etaf)
(require 'etaf-html)
(require 'image)
(require 'imenu)
(require 'org)
(require 'recentf)
(require 'seq)
(require 'shr)
(require 'url-parse)
(require 'url-util)

(declare-function dom-inner-text "dom" (node))

;;; EPUB preparation

(defgroup etaf-epub nil
  "EPUB reader mode."
  :group 'multimedia)

(defcustom etaf-epub-unzip-program (executable-find "unzip")
  "Path to decompression executable."
  :type '(file :must-match t)
  :group 'etaf-epub)

(defcustom etaf-epub-unzip-args '("-od" directory filename)
  "Arguments to decompression executable.
This variable expects a list of strings, except for `directory'
and `filename' symbols, which will be replaced accordingly when
opening an EPUB file."
  :type '(repeat (choice (string :tag "Argument")
                         (const :tag "Directory" directory)
                         (const :tag "Filename" filename)))
  :group 'etaf-epub)

(defcustom etaf-epub-render-html-function 'etaf-epub-render-html
  "Function used to render HTML.
It is called without arguments with a buffer containing HTML and
should change it to contain the rendered version of it."
  :type 'function
  :group 'etaf-epub)

(defcustom etaf-epub-pre-html-render-hook nil
  "Hook run before `etaf-epub-render-html'."
  :type 'hook
  :group 'etaf-epub)

(defcustom etaf-epub-post-html-render-hook nil
  "Hook run after `etaf-epub-render-html'."
  :type 'hook
  :group 'etaf-epub)

(defcustom etaf-epub-save-place-file (locate-user-emacs-file "etaf-epub-places")
  "File name where last reading places are saved to and restored from.
If set to nil, no saving and restoring is performed."
  :type '(choice (file  :tag "File name")
                 (const :tag "Don't save last reading places" nil))
  :group 'etaf-epub)

(defcustom etaf-epub-page-margin 2
  "Columns of margin on each side of the rendered page."
  :type 'integer
  :group 'etaf-epub)

(defcustom etaf-epub-auto-resize nil
  "Whether to re-render the page when its window changes size.
When nil, the current layout is kept after a window size or
configuration change; call `etaf-epub-render-document' to lay the
page out for the current window."
  :type 'boolean
  :group 'etaf-epub)

(defcustom etaf-epub-font nil
  "Font used to display EPUB pages.
The value is one of:

- nil, to use `etaf-html-font' if it is set, or the default face's
  font otherwise;
- a font family name (a string);
- a font spec or font object;
- a plist of face attributes, for example
  \(:family \"Noto Serif CJK SC\" :height 140).  `:size' is also
  accepted, in points, and is converted to `:height'.

The font is applied to the text rendered by `etaf-epub-render-html'
and is in effect while measuring the page, so that line breaking
matches the font used for display."
  :type '(choice (const :tag "Same as `etaf-html-font'" nil)
                 (string :tag "Font family")
                 (plist :key-type symbol :value-type sexp))
  :group 'etaf-epub)

(defvar-local etaf-epub-file-name nil
  "Path to the EPUB file backing this buffer.")

(defvar-local etaf-epub-initialize-work-dir-p t
  "Set to nil if no working directory should be initialized.")
(put 'etaf-epub-initialize-work-dir-p 'permanent-local t)

(defvar-local etaf-epub-remove-work-dir-on-exit-p t
  "Set to nil if working directory should be preserved after exit.")
(put 'etaf-epub-remove-work-dir-on-exit-p 'permanent-local t)

(defvar-local etaf-epub-work-dir nil
  "Directory containing the buffer's EPUB files.")
(put 'etaf-epub-work-dir 'permanent-local t)

(defvar-local etaf-epub-content-file nil
  "Path to the EPUB buffer's .opf file.")

(defvar-local etaf-epub-epub-version nil
  "Version string of the EPUB buffer.")

(defvar-local etaf-epub-metadata nil
  "Metadata of the EPUB buffer.")

(defvar-local etaf-epub-documents nil
  "Alist for the EPUB buffer's documents.
Each alist item consists of the identifier and full path.")

(defvar-local etaf-epub-documents-index 0
  "Index of the currently rendered document in the EPUB buffer.")

(defvar-local etaf-epub-spine-ids nil
  "Identifiers of the spine documents, in reading order.
CREngine and KOReader number documents by their position in this
list when they write XPointers.")

(defvar-local etaf-epub-toc-id nil
  "TOC identifier of the EPUB buffer.")

(defvar-local etaf-epub-source-buffer nil
  "Buffer holding the EPUB state for the rendered page.
The page is normally mounted in the EPUB buffer itself, but a
custom `etaf-epub-render-html-function' may render elsewhere.  This
variable lets link callbacks get back to the buffer that owns the
EPUB state.")

(defvar-local etaf-epub-history nil
  "Stack of documents user has visited.
Each element of the stack is a list (NODEINDEX BUFFERPOS).")

(defvar-local etaf-epub-history-forward nil
  "Stack of documents user has visited with `etaf-epub-history-back' command.
Each element of the stack is a list (NODEINDEX BUFFERPOS).")

(defvar-local etaf-epub--rendered-width nil
  "Character width used by the current render.")

(defvar-local etaf-epub--rendered-height nil
  "Window body height in lines used by the current render.")

(defvar-local etaf-epub--resize-timer nil
  "Idle timer that re-renders after a window size change.")

(defvar-local etaf-epub--resizing nil
  "Non-nil while a resize re-render is in progress.")

(defun etaf-epub-make-path (directory file)
  "Create a path from DIRECTORY and FILE."
  (concat (file-name-as-directory directory) file))

(defun etaf-epub-directory-files (directory)
  "Returns a list of files in DIRECTORY except for . and .."
  (seq-remove (lambda (file) (string-match-p "/\\.\\(?:\\.\\)?\\'" file))
              (directory-files directory t)))

(defun etaf-epub-contains-nested-directory-p (directory)
  "Non-nil if DIRECTORY contains exactly one directory."
  (let* ((files (etaf-epub-directory-files directory))
         (file (car files)))
    (and (= (length files) 1)
         (file-directory-p file)
         file)))

(defun etaf-epub-unnest-directory (directory child)
  "Move contents of CHILD into DIRECTORY, then delete CHILD."
  ;; FIXME: this will most certainly fail for con/con
  (dolist (item (etaf-epub-directory-files child))
    (rename-file item directory))
  (delete-directory child))

(defun etaf-epub--fix-permissions (file-or-directory mode)
  "Give FILE-OR-DIRECTORY the permissions selected by MODE."
  (let* ((modes (file-modes file-or-directory))
         (fixed-mode (file-modes-symbolic-to-number mode modes)))
    (set-file-modes file-or-directory fixed-mode)))

(defun etaf-epub-fix-permissions (directory)
  "Iterate recursively through DIRECTORY to fix its files."
  (etaf-epub--fix-permissions directory "+rx")
  (dolist (file (etaf-epub-directory-files directory))
    (if (file-directory-p file)
        (etaf-epub-fix-permissions file)
      (etaf-epub--fix-permissions file "+r"))))

(defun etaf-epub-unzip-epub (directory filename)
  "Extract FILENAME into DIRECTORY.
Unnecessary nesting is removed with `etaf-epub-unnest-directory'."
  (let* ((status (apply #'call-process etaf-epub-unzip-program nil "*etaf-epub unzip*" t
                        (mapcar (lambda (arg)
                                  (cond
                                   ((eq arg 'directory) directory)
                                   ((eq arg 'filename) filename)
                                   (t arg)))
                                etaf-epub-unzip-args)))
         child)
    (while (setq child (etaf-epub-contains-nested-directory-p directory))
      (etaf-epub-unnest-directory directory child))
    ;; HACK: unzip preserves file permissions, no matter how silly they
    ;; are, so ensure files and directories are readable
    (etaf-epub-fix-permissions directory)
    status))

(defun etaf-epub-warn (message &optional level)
  "Like `display-warning', but for etaf-epub-specific warnings.
Displays MESSAGE in a warnings buffer, with LEVEL as severity."
  (display-warning 'etaf-epub message level))

(defmacro etaf-epub-ignore-file-errors (&rest body)
  "Like `ignore-errors', but for file errors in BODY."
  `(condition-case nil (progn ,@body) (file-error nil)))

(defun etaf-epub-slurp (filename &optional parse-xml-p)
  "Return the contents of FILENAME.
If PARSE-XML-P is t, return the contents as parsed by libxml."
  (with-temp-buffer
    (insert-file-contents filename)
    (if parse-xml-p
        (libxml-parse-xml-region (point-min) (point-max))
      (buffer-string))))

(defun etaf-epub-mimetype-valid-p (directory)
  "Return t if DIRECTORY contains a valid EPUB mimetype file."
  (etaf-epub-ignore-file-errors
    (let ((filename (etaf-epub-make-path directory "mimetype")))
      (equal (etaf-epub-slurp filename) "application/epub+zip"))))

(defun etaf-epub-container-filename (directory)
  "Return the container filename for DIRECTORY."
  (let ((filename (etaf-epub-make-path directory "META-INF")))
    (etaf-epub-make-path filename "container.xml")))

(defun etaf-epub-container-content-filename (content)
  "Return the content filename for CONTENT."
  (let* ((query "container>rootfiles>rootfile[media-type='application/oebps-package+xml']")
         (node (esxml-query query content)))
    (dom-attr node 'full-path)))

(defun etaf-epub-container-valid-p (directory)
  "Return t if DIRECTORY holds a valid EPUB container."
  (let ((filename (etaf-epub-container-filename directory)))
    (when (and filename (file-exists-p filename))
      (let* ((content (etaf-epub-slurp filename t))
             (content-file (etaf-epub-container-content-filename content)))
        (when (and content content-file)
          (file-exists-p (etaf-epub-make-path directory content-file)))))))

(defun etaf-epub-epub-valid-p (directory)
  "Return t if DIRECTORY makes up a valid EPUB document."
  (when (not (etaf-epub-mimetype-valid-p directory))
    (message "Invalid mimetype"))
  (etaf-epub-container-valid-p directory))

(defun etaf-epub-urldecode (string)
  "Return urldecoded version of STRING or nil."
  (when string
    (url-unhex-string string)))

(defun etaf-epub-content-version (content)
  "Return the EPUB version for CONTENT."
  (let* ((node (esxml-query "package" content))
         (version (dom-attr node 'version)))
    (when (not version)
      (error "Version not specified"))
    version))

(defun etaf-epub-content-unique-identifier-name (content)
  "Return the unique identifier name referenced in CONTENT.
This is used in `etaf-epub-content-unique-identifier' to retrieve the
the specific type of unique identifier."
  (let* ((node (esxml-query "package[unique-identifier]" content))
         (name (dom-attr node 'unique-identifier)))
    (when (not name)
      (error "Unique identifier name not specified"))
    name))

(defun etaf-epub-content-unique-identifier (content)
  "Return the the unique identifier for CONTENT."
  (let* ((name (etaf-epub-content-unique-identifier-name content))
         (selector (format "package>metadata>identifier[id='%s']"
                           (esxml-query-css-escape name)))
         (id (car (dom-children (esxml-query selector content)))))
    (when (not id)
      (error "Unique identifier not found by its name: %s" name))
    (intern id)))

;; NOTE: unique identifier is queried separately as identifiers can
;; appear more than once and only one of them can be the unique one
(defvar etaf-epub-required-metadata-tags '(title language)
  "Required metadata tags used for `etaf-epub-content-metadata'.")

(defvar etaf-epub-optional-metadata-tags
  '(contributor coverage creator date description format
    publisher relation rights source subject type)
  "Optional metadata tags used for `etaf-epub-content-metadata'.")

(defun etaf-epub-content-metadata (content)
  "Return a metadata alist for CONTENT.
Required keys are \\='identifier and everything in
`etaf-epub-required-metadata-tags', optional keys are in
`etaf-epub-optional-metadata-tags'."
  (let* ((identifier (etaf-epub-content-unique-identifier content))
         (candidates (mapcar (lambda (node)
                               (cons (dom-tag node) (car (dom-children node))))
                             (esxml-query-all "package>metadata>*" content)))
         (required (mapcar (lambda (tag)
                             (let ((candidate (cdr (assq tag candidates))))
                               (when (not candidate)
                                 ;; NOTE: this should ideally be a
                                 ;; warning, but `warn' is too obtrusive
                                 (message "Required metadatum %s not found" tag))
                               (cons tag candidate)))
                           etaf-epub-required-metadata-tags))
         (optional (mapcar (lambda (tag) (cons tag (cdr (assq tag candidates))))
                           etaf-epub-optional-metadata-tags)))
    (append `((identifier . ,identifier)) required optional)))

(defun etaf-epub-content-manifest (directory content)
  "Extract an alist of manifest files for CONTENT in DIRECTORY.
Each alist item consists of the identifier and full path."
  (mapcar (lambda (node)
            (let ((id (dom-attr node 'id))
                  (href (dom-attr node 'href)))
              (cons (intern id)
                    (etaf-epub-make-path directory (etaf-epub-urldecode href)))))
          (esxml-query-all "package>manifest>item" content)))

(defun etaf-epub-content-spine (content)
  "Extract a list of spine identifiers for CONTENT."
  (mapcar (lambda (node) (intern (dom-attr node 'idref)))
          (esxml-query-all "package>spine>itemref" content)))

(defun etaf-epub--content-epub2-files (content manifest files)
  "Return FILES for an EPUB 2 CONTENT, adding its NCX from MANIFEST."
  (let* ((node (esxml-query "package>spine[toc]" content))
         (id (dom-attr node 'toc)))
    (when (not id)
      (error "EPUB 2 NCX ID not found"))
    (setq etaf-epub-toc-id (intern id))
    (let ((toc-file (assq etaf-epub-toc-id manifest)))
      (when (not toc-file)
        (error "EPUB 2 NCX file not found"))
      (cons toc-file files))))

(defun etaf-epub--content-epub3-files (content manifest files)
  "Return FILES for an EPUB 3 CONTENT, adding its nav document from MANIFEST."
  (let* ((node (esxml-query "package>manifest>item[properties~=nav]" content))
         (id (dom-attr node 'id)))
    (when (not id)
      (error "EPUB 3 <nav> ID not found"))
    (setq etaf-epub-toc-id (intern id))
    (let ((toc-file (assq etaf-epub-toc-id manifest)))
      (when (not toc-file)
        (error "EPUB 3 <nav> file not found"))
      (setq files (seq-remove (lambda (item) (eq (car item) etaf-epub-toc-id)) files))
      (cons toc-file files))))

(defun etaf-epub-content-files (directory content)
  "Create correctly ordered file alist for CONTENT in DIRECTORY.
Each alist item consists of the identifier and full path."
  (let* ((manifest (etaf-epub-content-manifest directory content))
         (spine (etaf-epub-content-spine content))
         (files (mapcar (lambda (item) (assq item manifest)) spine)))
    ;; Remember the spine before the TOC gets prepended, so that
    ;; CREngine XPointers (`DocFragment[N]') can be mapped back.
    (setq etaf-epub-spine-ids (mapcar #'car files))
    (if (version< etaf-epub-epub-version "3.0")
        (etaf-epub--content-epub2-files content manifest files)
      (etaf-epub--content-epub3-files content manifest files))))

(defun etaf-epub--walk-ncx-node (node)
  "Insert NODE of an NCX document as HTML at point."
  (let ((tag (dom-tag node))
        (children (seq-filter (lambda (child) (eq (dom-tag child) 'navPoint))
                              (dom-children node))))
    (cond
     ((eq tag 'navMap)
      (insert "<ol>\n")
      (mapc (lambda (node) (etaf-epub--walk-ncx-node node)) children)
      (insert "</ol>\n"))
     ((eq tag 'navPoint)
      (let* ((navlabel-node (dom-by-tag node 'navLabel))
             (label-node (dom-by-tag navlabel-node 'text))
             (content-node (dom-by-tag node 'content))
             (href (etaf-epub-urldecode (dom-attr content-node 'src)))
             (label (and label-node (car (dom-children label-node)))))
        (when (not href)
          (error "Navigation point is missing href attribute"))
        (let ((link (format "<a href=\"%s\">%s</a>"
                            (url-insert-entities-in-string href)
                            (url-insert-entities-in-string (or label href)))))
          (if children
              (progn
                (insert (format "<li>\n%s\n<ol>\n" link))
                (mapc (lambda (node) (etaf-epub--walk-ncx-node node))
                      children)
                (insert (format "</ol>\n</li>\n")))
            (insert (format "<li>\n%s\n</li>\n" link)))))))))

(defun etaf-epub-ncx-to-html (path)
  "Convert NCX document at PATH to HTML."
  (let ((root (esxml-query "navMap" (etaf-epub-slurp path t))))
    (with-temp-buffer
      (etaf-epub--walk-ncx-node root)
      (buffer-string))))

;;; Rendering with ETAF

(defun etaf-epub--page-view (view)
  "Wrap VIEW so reader keys work on the rendered page.
`etaf-html' renders a standalone anchor as an ETAF button, but an
anchor inside a paragraph becomes propertized text.  The root Host
adds keyboard bindings for the latter without changing dropdown
or button activation, which stays nearer in the Host chain.

The root also binds the document-navigation keys.  Ebox binds `n'
and `p' to scrolling in its minor mode map, which would otherwise
shadow the major mode bindings; declaring them on the root Host
puts them in ETAF's scoped key layer, which has priority."
  (etaf-node
   'box
   (list :width 'fit-content
         :keys
         '(("RET" etaf-epub-follow-link "Follow link")
           ("<mouse-2>" etaf-epub-follow-link "Follow link")
           ("n" etaf-epub-next-document "Next document")
           ("p" etaf-epub-previous-document "Previous document")
           ("]" etaf-epub-next-document "Next document")
           ("[" etaf-epub-previous-document "Previous document")
           ("t" etaf-epub-goto-toc "Table of contents")
           ("l" etaf-epub-history-back "Back")
           ("r" etaf-epub-history-forward "Forward")
           ("f" etaf-epub-follow-link "Follow link")
           ("w" etaf-epub-copy-url "Copy link")))
   (list view)))

(defun etaf-epub--font ()
  "Return the font used to display EPUB pages.
This is `etaf-epub-font', falling back to `etaf-html-font'."
  (or etaf-epub-font etaf-html-font))

(defvar etaf-epub--font-context nil
  "Buffer whose page font is currently installed by `etaf-epub--with-font'.")

(defvar-local etaf-epub--installed-font nil
  "Page font currently installed in this buffer.")

(defun etaf-epub--set-buffer-font (font)
  "Display the current buffer with FONT.
The buffer-local face remapping is updated only when FONT is not the
font already installed.  A real font change also flushes glyph-metric
caches: EKP and Ebox key most of them by the remapping, but a few
\(Ebox's cluster and space caches, for example) are global, so they
must not survive a switch to another font."
  (unless (and (equal font etaf-epub--installed-font)
               (or (null font) etaf-html--font-remap))
    (setq etaf-epub--installed-font font)
    (etaf-html--set-buffer-font (current-buffer) font)
    (run-hooks 'after-setting-font-hook)))

(defun etaf-epub--with-font (function)
  "Call FUNCTION with the EPUB page font in effect in the current buffer.
The font is installed before FUNCTION runs, both as a buffer-local
remapping of the `default' face and as the global default face, so
that EKP measures glyphs with the font that will be used for display.
Without this, `etaf-html' lays out lines with the default font and the
page later renders with a different one, which breaks EKP's breaks.

While FUNCTION runs, `after-setting-font-hook' is inhibited: the
global face change is temporary and the buffer's face remapping
already identifies the page font.  Letting the hook flush every
glyph-metric cache on entry and exit would make EKP re-break the
whole page on every render.  Caches are flushed by
`etaf-epub--set-buffer-font' when the font actually changes."
  (if (eq etaf-epub--font-context (current-buffer))
      (funcall function)
    (let ((font (etaf-epub--font))
          (etaf-epub--font-context (current-buffer)))
      (etaf-epub--set-buffer-font font)
      (let ((after-setting-font-hook nil))
        (if font
            (etaf-html-with-default-font font (funcall function))
          (funcall function))))))

(defun etaf-epub--mount-view (view)
  "Mount VIEW in the current buffer, honouring `etaf-epub-font'.
The font defaults to `etaf-html-font' when `etaf-epub-font' is nil.
Return the buffer."
  (let ((buffer (current-buffer)))
    (etaf-epub--with-font
     (lambda ()
       (etaf-mount buffer view etaf-html-mount-options)))
    buffer))

(defun etaf-epub--window-width ()
  "Return the character width available to the current EPUB page.
The width is strictly less than the window's pixel body width minus
`etaf-epub-page-margin' on both sides, converted with the same glyph
width Ebox uses for its `(ch N)' length."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (or window (null etaf-epub--rendered-width))
        (let* ((window (or window (selected-window)))
               ;; Measure with the buffer's face remapping rather than
               ;; temporarily changing the global default face: this runs
               ;; on every window change, and the remapping is what Ebox
               ;; uses for its `(ch N)' lengths anyway.
               (_ (etaf-epub--set-buffer-font (etaf-epub--font)))
               (char-pixels (max 1 (etaf-string-pixel-width "0")))
               (pixel-width (or (window-body-width window t)
                                (* (window-body-width window)
                                   (frame-char-width))))
               (margin-pixels (* etaf-epub-page-margin char-pixels))
               (columns (/ (- pixel-width (* 2 margin-pixels))
                            char-pixels)))
          (max 20 (1- columns)))
      ;; If the buffer is buried, keep the last width instead of using
      ;; an unrelated window's width.
      etaf-epub--rendered-width)))

(defun etaf-epub--window-height ()
  "Return the window body height available to the current EPUB page.
The height is in lines.  If the buffer is buried, keep the last
rendered height instead of using an unrelated window's height."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (or window (null etaf-epub--rendered-height))
        (max 1 (window-body-height (or window (selected-window))))
      etaf-epub--rendered-height)))

(defun etaf-epub--schedule-resize ()
  "Arrange a window size check after the current operation settles."
  (when (and etaf-epub-auto-resize
             (derived-mode-p 'etaf-epub-mode)
             (not etaf-epub--resizing))
    (let ((buffer (current-buffer)))
      (when (timerp etaf-epub--resize-timer)
        (cancel-timer etaf-epub--resize-timer))
      (setq etaf-epub--resize-timer
            (run-with-idle-timer
             0.2 nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq etaf-epub--resize-timer nil)
                   (etaf-epub--resize)))))))))

(defun etaf-epub--resize ()
  "Re-render the current EPUB buffer if its window size changed.
Both width and height are checked: Ebox lays the page out for the
window it is displayed in, so a height-only change needs a fresh
mount even though it does not change where lines break."
  (unless etaf-epub--resizing
    (let ((width (etaf-epub--window-width))
          (height (etaf-epub--window-height)))
      (unless (and (eq width etaf-epub--rendered-width)
                   (eq height etaf-epub--rendered-height))
        (let ((etaf-epub--resizing t)
              (point (point)))
          (setq etaf-epub--rendered-width width
                etaf-epub--rendered-height height)
          (setq-local etaf-html-max-width width)
          (etaf-epub-render-document)
          (etaf-next-tick
           (lambda ()
             (when (buffer-live-p (current-buffer))
               (goto-char (min point (point-max)))))))))))

(defun etaf-epub--window-size-change (frame)
  "Schedule a re-render for EPUB buffers shown in FRAME."
  (when etaf-epub-auto-resize
    (dolist (window (window-list frame 'nomini))
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'etaf-epub-mode)
          (etaf-epub--schedule-resize))))))

(defun etaf-epub--window-configuration-change ()
  "Schedule a re-render for EPUB buffers after a window change."
  (when etaf-epub-auto-resize
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 'nomini))
        (with-current-buffer (window-buffer window)
          (when (derived-mode-p 'etaf-epub-mode)
            (etaf-epub--schedule-resize)))))))

(add-hook 'window-size-change-functions #'etaf-epub--window-size-change)
(add-hook 'window-configuration-change-hook
          #'etaf-epub--window-configuration-change)

(defun etaf-epub--image-cells (path)
  "Return (COLUMNS . ROWS) for the image at PATH."
  (let* ((max-columns (max 1 (or etaf-html-max-width 80)))
         (char-width (max 1 (frame-char-width)))
         (char-height (max 1 (frame-char-height)))
         (size (ignore-errors
                 (image-size
                  (create-image path nil nil
                                :max-width (* max-columns char-width))
                  t)))
         (pixel-width (car-safe size))
         (pixel-height (cdr-safe size)))
    (if (and (numberp pixel-width)
             (numberp pixel-height)
             (> pixel-width 0)
             (> pixel-height 0))
        (cons (max 1 (min max-columns
                          (round (/ pixel-width (float char-width)))))
              (max 1 (round (/ pixel-height (float char-height)))))
      (cons max-columns (max 1 (round (* max-columns 0.75)))))))

(defun etaf-epub--mount-image (path)
  "Mount the image at PATH in the current buffer."
  (let* ((cells (etaf-epub--with-font
                 (lambda () (etaf-epub--image-cells path))))
         (view (etaf-node
                'column nil
                (list (etaf-node 'image
                                 (list :src path
                                       :columns (car cells)
                                       :rows (cdr cells))
                                 nil)))))
    (etaf-epub--mount-view view)))

(defun etaf-epub--trim-trailing-whitespace (string)
  "Remove trailing whitespace from every line of STRING."
  (mapconcat #'string-trim-right (split-string string "\n") "\n"))

(defun etaf-epub-render-html ()
  "Render HTML in the current EPUB buffer with `etaf-html'.
The rendered page is mounted in this buffer, so the buffer-local EPUB
state stays available to link callbacks."
  (run-hooks 'etaf-epub-pre-html-render-hook)
  ;; Callbacks look this up when a link is clicked, long after this
  ;; function returns, so install it buffer-locally before mounting.
  (setq-local etaf-html-browse-url #'etaf-epub-browse-url)
  (setq-local etaf-epub-source-buffer (current-buffer))
  (let ((html (etaf-epub--trim-trailing-whitespace (buffer-string))))
    ;; The font must be in effect while `etaf-html' translates the
    ;; document, because EKP breaks and justifies lines by measuring
    ;; glyphs at that point.
    (etaf-epub--with-font
     (lambda ()
       (etaf-epub--mount-view (etaf-epub--page-view (etaf-html html))))))
  (run-hooks 'etaf-epub-post-html-render-hook))

(defun etaf-epub-render-document ()
  "Render the document referenced by `etaf-epub-documents-index'.
If the document path refers to an image (as determined by
`image-type-file-name-regexps'), an image is mounted, otherwise
the HTML is rendered with `etaf-epub-render-html-function'."
  (interactive)
  (seq-let (id &rest path) (aref etaf-epub-documents etaf-epub-documents-index)
    (let* ((width (etaf-epub--window-width))
           (height (etaf-epub--window-height))
           ;; HACK: this should be looked up in the manifest
           (imagep (seq-find (lambda (item) (string-match-p (car item) path))
                             image-type-file-name-regexps))
           ;; NOTE: allows resolving image references correctly
           (default-directory (file-name-directory path))
           (inhibit-read-only t)
           buffer-read-only)
      (setq-local etaf-html-max-width width)
      (setq etaf-epub--rendered-width width
            etaf-epub--rendered-height height)
      (erase-buffer)

      (cond
       (imagep
        (etaf-epub--mount-image path))
       ((and (version< etaf-epub-epub-version "3.0")
             (eq id etaf-epub-toc-id))
        (insert (etaf-epub-ncx-to-html path))
        (funcall etaf-epub-render-html-function))
       (t
        (insert (etaf-epub-slurp path))
        (funcall etaf-epub-render-html-function)))

      ;; NOTE: this is how doc-view avoids overwriting the file
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

;;; UI

(defun etaf-epub-clean-up ()
  "Delete temporary files of the current EPUB buffer."
  (when (timerp etaf-epub--resize-timer)
    (cancel-timer etaf-epub--resize-timer)
    (setq etaf-epub--resize-timer nil))
  (when (and etaf-epub-remove-work-dir-on-exit-p etaf-epub-work-dir)
    (let ((identifier (cdr (assq 'identifier etaf-epub-metadata)))
          (index (if (integerp etaf-epub-documents-index)
                     etaf-epub-documents-index
                   0)))
      (etaf-epub-save-place identifier index (point)))
    (etaf-epub-ignore-file-errors
     (delete-directory etaf-epub-work-dir t))))

(defun etaf-epub-clean-up-all ()
  "Delete temporary files of all opened EPUB buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'etaf-epub-mode)
        (etaf-epub-clean-up)))))

(defun etaf-epub-external-url-p (url)
  "Return t if URL refers to an external document."
  (and (url-type (url-generic-parse-url url)) t))

(defun etaf-epub-url-filename-and-target (url)
  "Return a list of URL's filename and target."
  (setq url (url-generic-parse-url url))
  (mapcar 'etaf-epub-urldecode (list (url-filename url) (url-target url))))

(defun etaf-epub-find-document (predicate)
  "Return first item in `etaf-epub-documents' PREDICATE is true for."
  (let ((i 0)
        done)
    (while (and (not done)
                (< i (length etaf-epub-documents)))
      (when (funcall predicate (aref etaf-epub-documents i))
        (setq done t))
      (setq i (1+ i)))
    (when done
      (1- i))))

(defun etaf-epub-goto-document (index)
  "Go to the document denoted by INDEX."
  (let ((history (cons (list etaf-epub-documents-index (point))
                       etaf-epub-history)))
    (setq etaf-epub-documents-index index)
    (etaf-epub-render-document)
    (setq etaf-epub-history history)))

(defun etaf-epub-goto-toc ()
  "Go to the TOC index and render the TOC document."
  (interactive)
  (let ((index (etaf-epub-find-document (lambda (doc) (eq (car doc) etaf-epub-toc-id)))))
    (when (not index)
      (error "Couldn't locate TOC"))
    (etaf-epub-goto-document index)))

(defun etaf-epub-view-source ()
  "View the source of the current document in a new buffer."
  (interactive)
  (find-file (cdr (aref etaf-epub-documents etaf-epub-documents-index))))

(defun etaf-epub-view-content-source ()
  "View the source of the content file in a new buffer."
  (interactive)
  (find-file etaf-epub-content-file))

(defun etaf-epub-reopen-as-archive ()
  "Reopen the EPUB document using `archive-mode'."
  (interactive)
  (with-current-buffer (find-file-literally etaf-epub-file-name)
    (archive-mode)))

(defun etaf-epub-display-metadata ()
  "View the metadata of the EPUB document in a new buffer."
  (interactive)
  (let ((buffer "*EPUB metadata*")
        (metadata etaf-epub-metadata)
        (version etaf-epub-epub-version))
    (with-current-buffer (get-buffer-create buffer)
      (special-mode)
      (let (buffer-read-only)
        (erase-buffer)
        (insert (format "EPUB Version: %s\n" version))
        (dolist (item metadata)
          (seq-let (key &rest value) item
            (insert (format "%s: " (capitalize (symbol-name key))))
            (if value
                (if (eq key 'description)
                    (let ((beg (point)))
                      (insert value)
                      (shr-render-region beg (point)))
                  (insert (format "%s" value)))
              (insert (propertize "None" 'face 'italic)))
            (insert "\n")))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun etaf-epub-next-document (&optional count)
  "Go to the next document and render it.
Takes an optional COUNT, goes backward if COUNT is negative."
  (interactive "p")
  (etaf-epub-goto-document (min (1- (length etaf-epub-documents))
                           (+ etaf-epub-documents-index (or count 1)))))

(defun etaf-epub-previous-document (&optional count)
  "Go to the previous document and render it.
Takes an optional COUNT, goes forward if COUNT is negative."
  (interactive "p")
  (etaf-epub-goto-document (max 0 (- etaf-epub-documents-index (or count 1)))))

(defun etaf-epub-scroll-up (arg)
  "Scroll with `scroll-up' or visit next chapter if at bottom.
ARG is passed to `scroll-up'."
  (interactive "P")
  (if (>= (window-end) (point-max))
      (etaf-epub-next-document)
    (scroll-up arg)))

(defun etaf-epub-scroll-down (arg)
  "Scroll with `scroll-down' or visit previous chapter if at top.
ARG is passed to `scroll-down'."
  (interactive "P")
  (if (and (<= (window-start) (point-min))
           (> etaf-epub-documents-index 0))
      (progn
        (etaf-epub-previous-document)
        (goto-char (point-max)))
    (scroll-down arg)))

(defun etaf-epub--link-directories ()
  "Return directories used to resolve relative EPUB links."
  (let ((current-path (cdr (aref etaf-epub-documents etaf-epub-documents-index))))
    (delete-dups
     (delq nil
           (list (file-name-directory current-path)
                 (and etaf-epub-content-file
                      (file-name-directory etaf-epub-content-file)))))))

(defun etaf-epub--document-index (filename)
  "Return the index of the document named by FILENAME, or nil.
FILENAME is resolved relative to the current document first, then
relative to the EPUB content root.  The latter covers EPUBs whose
links are written relative to the root rather than to the
containing document."
  (let ((paths (mapcar (lambda (dir)
                         (file-truename (expand-file-name filename dir)))
                       (etaf-epub--link-directories))))
    (etaf-epub-find-document
     (lambda (doc)
       (member (file-truename (cdr doc)) paths)))))

(defun etaf-epub--resolve-file (filename)
  "Return the first existing file named FILENAME in the EPUB, or nil."
  (seq-find #'file-exists-p
            (mapcar (lambda (dir) (expand-file-name filename dir))
                    (etaf-epub--link-directories))))

(defun etaf-epub-visit-relative-file (filename target)
  "Visit the document as specified by FILENAME and TARGET.
FILENAME is resolved relative to the current document first, then
relative to the EPUB content root."
  (let (index)
    (when (not (zerop (length filename)))
      (let ((match (etaf-epub--document-index filename)))
        (when (not match)
          (error "Couldn't locate document: %s" filename))
        (setq index match)))
    ;; HACK: this binding is only need for Emacs 27.1 and older, as of
    ;; Emacs 28.1, shr.el always adds the shr-target-id property
    (let ((shr-target-id target))
      (etaf-epub-goto-document (or index etaf-epub-documents-index))))
  (when target
    ;; Pressing a button dispatches an ETAF event, and the new mount is
    ;; painted only when that event batch drains.  Search for the anchor
    ;; after the paint, or the properties would still belong to the old
    ;; document.  Outside a batch `etaf-next-tick' runs immediately.
    (etaf-next-tick
     (lambda ()
       (let ((pos (point-min))
             done)
         (while (and (not done) pos)
           (let ((property (get-text-property pos 'shr-target-id)))
             (if (or (equal property target)
                     ;; NOTE: as of Emacs 28.1 this may be a list of targets
                     (and (consp property) (member target property)))
                 (progn
                   (goto-char pos)
                   (recenter (1- (max 1 scroll-margin)))
                   (setq done t))
               ;; NOTE: to catch the case of cursor starting out at the
               ;; `shr-target-id' property, change pos *after* checking
               (setq pos (next-single-property-change pos 'shr-target-id)))))
         (when (not done)
           (error "Couldn't locate target")))))))

;; adapted from `shr-browse-url'
(defun etaf-epub--state-buffer ()
  "Return the buffer that owns the current EPUB's buffer-local state."
  (if (and etaf-epub-source-buffer (buffer-live-p etaf-epub-source-buffer))
      etaf-epub-source-buffer
    (current-buffer)))

(defun etaf-epub-follow-link ()
  "Follow the link at point in the rendered EPUB page."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (unless url
      (user-error "No link under point"))
    (etaf-epub-browse-url url)))

(defun etaf-epub-browse-url (url)
  "Follow URL.
External URLs are opened with `browse-url'.  URLs pointing to an
EPUB document are visited with `etaf-epub-visit-relative-file';
other files packaged in the EPUB (images, stylesheets, ...) are
opened with `find-file'."
  (when (not url)
    (user-error "No link under point"))
  (if (etaf-epub-external-url-p url)
      (browse-url url)
    (let (file)
      (with-current-buffer (etaf-epub--state-buffer)
        (seq-let (filename target) (etaf-epub-url-filename-and-target url)
          (if (or (zerop (length filename))
                  (etaf-epub--document-index filename))
              (etaf-epub-visit-relative-file filename target)
            (setq file (etaf-epub--resolve-file filename))
            (when (not file)
              (error "Couldn't locate document: %s" filename)))))
      ;; Open the file outside `with-current-buffer', which would
      ;; otherwise restore the EPUB buffer and undo `find-file'.
      (when file
        (find-file file)))))

(defun etaf-epub-copy-url (&optional mouse-event)
  "Copy the URL of the link at point to the kill ring.
MOUSE-EVENT, when it is a mouse event, moves point to the click
position first."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p mouse-event)
    (mouse-set-point mouse-event))
  (let ((url (get-text-property (point) 'shr-url)))
    (when (not url)
      (user-error "No link under point"))
    (kill-new url)
    (message "%s" url)))

(defun etaf-epub-saved-places ()
  "Retrieve saved places in `etaf-epub-save-place-file'."
  (when (and etaf-epub-save-place-file (file-exists-p etaf-epub-save-place-file))
    (with-temp-buffer
      (insert-file-contents-literally etaf-epub-save-place-file)
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (error
         (etaf-epub-warn "Failed to retrieve saved places from `etaf-epub-save-place-file'")
         nil)))))

(defun etaf-epub-saved-place (identifier)
  "Retrieve saved place for IDENTIFIER in `etaf-epub-saved-place-file'."
  (cdr (assq identifier (etaf-epub-saved-places))))

(defun etaf-epub-save-place (identifier index point)
  "Save place as identified by IDENTIFIER, INDEX and POINT.
Saving is only done if `etaf-epub-save-place-file' is set."
  (when etaf-epub-save-place-file
    (let* ((place `(,identifier (index . ,index)
                                (point . ,point)))
           (places (cons place (assq-delete-all identifier (etaf-epub-saved-places))))
           print-level
           print-length)
      (with-temp-file etaf-epub-save-place-file
        (insert (prin1-to-string places))))))

(defun etaf-epub--index-valid-p (documents index)
  "Return non-nil when INDEX addresses an entry of DOCUMENTS."
  (and (integerp index)
       (>= index 0)
       (< index (length documents))))

(defun etaf-epub-history-back ()
  "Go back in the history to the last visited document."
  (interactive)
  (or etaf-epub-history
      (user-error "This is the first document you looked at"))
  (let ((history-forward (cons (list etaf-epub-documents-index (point))
                               etaf-epub-history-forward)))
    (seq-let (index opoint) (car etaf-epub-history)
      (setq etaf-epub-history (cdr etaf-epub-history))
      (etaf-epub-goto-document index)
      (setq etaf-epub-history (cdr etaf-epub-history))
      (setq etaf-epub-history-forward history-forward)
      (etaf-next-tick
       (lambda ()
         (goto-char opoint)
         (recenter (1- (max 1 scroll-margin))))))))

(defun etaf-epub-history-forward ()
  "Go forward in the history of visited documents."
  (interactive)
  (or etaf-epub-history-forward
      (user-error "This is the last document you looked at"))
  (let ((history-forward (cdr etaf-epub-history-forward)))
    (seq-let (index opoint) (car etaf-epub-history-forward)
      (etaf-epub-goto-document index)
      (setq etaf-epub-history-forward history-forward)
      (etaf-next-tick
       (lambda ()
         (goto-char opoint)
         (recenter (1- (max 1 scroll-margin))))))))

(defun etaf-epub--initialize-temp-dir (path)
  "Extract the EPUB at PATH into a new temporary work directory."
  (when (not path)
    (error "EPUB must be associated with file"))
  (when (not etaf-epub-unzip-program)
    (error "Unzip executable not found, customize `etaf-epub-unzip-program'"))
  (setq etaf-epub-work-dir (make-temp-file "etaf-epub-" t ".epub"))
  (let ((exit-code (etaf-epub-unzip-epub etaf-epub-work-dir path)))
    (when (not (integerp exit-code))
      (etaf-epub-clean-up)
      (error "EPUB extraction aborted by signal %s" exit-code))
    (when (> exit-code 1) ; exit code 1 is most likely a warning
      (etaf-epub-clean-up)
      (error "EPUB extraction failed with exit code %d (see *etaf-epub unzip* buffer)"
             exit-code)))
  (when (not (etaf-epub-epub-valid-p etaf-epub-work-dir))
    (etaf-epub-clean-up)
    (error "Invalid EPUB file")))

(defun etaf-epub-open-directory (dir)
  "Open EPUB buffer using DIR as directory of the document."
  (interactive "DEPUB directory to read: ")
  (let ((buf (generate-new-buffer (format "%s<etaf-epub>" dir))))
    (with-current-buffer buf
      (setq etaf-epub-initialize-work-dir-p nil)
      (setq etaf-epub-remove-work-dir-on-exit-p nil)
      (setq etaf-epub-work-dir dir)
      (etaf-epub-mode))
    (switch-to-buffer buf)))

;;;###autoload
(define-derived-mode etaf-epub-mode special-mode "EPUB"
  "Major mode for reading EPUB documents.
The rendered page is mounted by ETAF in this buffer, so page text
is read-only and navigation happens through ETAF buttons and the
link-following commands below.

\\{etaf-epub-mode-map}"
  (when etaf-epub-remove-work-dir-on-exit-p
    (add-hook 'kill-buffer-hook 'etaf-epub-clean-up nil t)
    (add-hook 'kill-emacs-hook 'etaf-epub-clean-up-all)
    (add-hook 'change-major-mode-hook 'etaf-epub-clean-up nil t))

  (when etaf-epub-initialize-work-dir-p
    (etaf-epub--initialize-temp-dir buffer-file-name))

  (let* ((content (etaf-epub-slurp (etaf-epub-container-filename etaf-epub-work-dir) t))
         (content-file-name (etaf-epub-container-content-filename content))
         (content-file (etaf-epub-make-path etaf-epub-work-dir content-file-name))
         (work-dir (file-name-directory content-file))
         (content (etaf-epub-slurp content-file t)))
    (setq etaf-epub-content-file content-file)
    (setq etaf-epub-epub-version (etaf-epub-content-version content))
    (setq etaf-epub-metadata (etaf-epub-content-metadata content))
    (setq etaf-epub-documents (apply 'vector (etaf-epub-content-files work-dir content)))
    (setq etaf-epub-documents-index 0))
  (setq buffer-undo-list t)
  (setq etaf-epub-file-name (buffer-file-name)) ; kept for compatibility reasons
  (setq-local bookmark-make-record-function
              'etaf-epub-bookmark-make-record)
  (let ((place (etaf-epub-saved-place (cdr (assq 'identifier etaf-epub-metadata)))))
    (if place
        (let ((index (cdr (assq 'index place)))
              (point (cdr (assq 'point place))))
          (if (etaf-epub--index-valid-p etaf-epub-documents index)
              (progn
                (setq etaf-epub-documents-index index)
                (etaf-epub-render-document)
                (goto-char point))
            (etaf-epub-warn "Couldn't restore last position")
            (etaf-epub-render-document)))
      (etaf-epub-render-document))))

;;; Own Emacs keys

(defvar-local etaf-epub-user-keys--saved nil
  "State `etaf-epub-user-keys-mode' replaced, as a plist.
Holds the cursor and input-modality settings it overrode, and which of
ETAF's scoped keys and Ebox's minor mode it had to remove.")

(defvar-local etaf-epub-user-keys--keymaps nil
  "Per-node keymaps removed from the page, as (START END KEYMAP).
Ebox installs one on every painted node, holding that node's click and
RET commands; they are put back when the mode is turned off.")

(defun etaf-epub-user-keys--strip-keymaps ()
  "Remember and remove the per-node keymaps of the painted page."
  (setq etaf-epub-user-keys--keymaps nil)
  (let ((position (point-min))
        (limit (point-max))
        ;; A mounted canvas is read-only; its text properties are ours.
        (inhibit-read-only t)
        end)
    (while (< position limit)
      (let ((keymap (get-text-property position 'keymap)))
        (setq end (next-single-property-change position 'keymap nil limit))
        (when keymap
          (push (list position end keymap) etaf-epub-user-keys--keymaps)
          (remove-text-properties position end '(keymap nil)))
        (setq position end)))
    (setq etaf-epub-user-keys--keymaps
          (nreverse etaf-epub-user-keys--keymaps))))

(defun etaf-epub-user-keys--restore-keymaps ()
  "Put the per-node keymaps removed by `--strip-keymaps' back."
  (let ((limit (point-max))
        (inhibit-read-only t))
    (dolist (entry etaf-epub-user-keys--keymaps)
      (add-text-properties (nth 0 entry) (min (nth 1 entry) limit)
                           (list 'keymap (nth 2 entry)))))
  (setq etaf-epub-user-keys--keymaps nil))

(defun etaf-epub-user-keys--setup ()
  "Remove every key binding the mounted canvas brought with it.
Idempotent, and called again after every render: a new mount re-installs
ETAF's scoped keys, re-enables Ebox's minor mode and repaints the node
keymaps."
  (unless etaf-epub-user-keys--saved
    (setq etaf-epub-user-keys--saved
          (list :cursor (cons (local-variable-p 'cursor-type) cursor-type)
                :modality
                (cons (local-variable-p 'etaf-cursor-follows-input-modality)
                      etaf-cursor-follows-input-modality)
                :etaf-keys (and (memq 'etaf--keys-alist
                                      emulation-mode-map-alists)
                                t)
                :ebox-mode (and (bound-and-true-p ebox-buffer-mode) t))))
  (setq-local emulation-mode-map-alists
              (delq 'etaf--keys-alist emulation-mode-map-alists))
  ;; Ebox's minor mode owns the movement keys, the wheel and RET; turning
  ;; it off also drops the hook that cancels the mark.
  (when (bound-and-true-p ebox-buffer-mode)
    (ebox-buffer-mode -1))
  (etaf-epub-user-keys--strip-keymaps)
  ;; A pointer press normally hides the cursor; a selection needs it.
  (setq-local etaf-cursor-follows-input-modality nil)
  (setq-local cursor-type t))

(defun etaf-epub-user-keys--teardown ()
  "Undo `etaf-epub-user-keys--setup'."
  (etaf-epub-user-keys--restore-keymaps)
  (when-let* ((saved etaf-epub-user-keys--saved))
    (when (plist-get saved :ebox-mode)
      (ebox-buffer-mode 1))
    (when (plist-get saved :etaf-keys)
      (setq-local emulation-mode-map-alists
                  (cons 'etaf--keys-alist
                        (delq 'etaf--keys-alist emulation-mode-map-alists))))
    (let ((cursor (plist-get saved :cursor))
          (modality (plist-get saved :modality)))
      (if (car cursor)
          (setq-local cursor-type (cdr cursor))
        (kill-local-variable 'cursor-type))
      (if (car modality)
          (setq-local etaf-cursor-follows-input-modality (cdr modality))
        (kill-local-variable 'etaf-cursor-follows-input-modality)))
    (setq etaf-epub-user-keys--saved nil))
  (deactivate-mark))

(defun etaf-epub-user-keys--reapply ()
  "Strip the canvas keys again after the page was rendered."
  (when (bound-and-true-p etaf-epub-user-keys-mode)
    (etaf-epub-user-keys--setup)))

(add-hook 'etaf-epub-post-html-render-hook #'etaf-epub-user-keys--reapply)

;;;###autoload
(define-minor-mode etaf-epub-user-keys-mode
  "Give the mounted page back to the user's own Emacs keys.
A mounted Ebox canvas claims the movement keys for its own focus model
and installs a keymap on every painted node, so the text cannot be
selected and a click does not place point.  This mode removes all of
that for as long as it is on: ETAF's scoped keys, Ebox's minor mode and
the per-node keymaps.  The page then behaves like any read-only buffer,
which means the mark, the movement commands, mouse drag and every key
binding of the user's own work, while the reader's commands from
`etaf-epub-mode-map' stay available.  Turning it off restores the canvas,
including link activation and the wheel.

`etaf-epub-selection-copy' copies the marked region without the padding
spaces the rendered page puts at the end of its lines."
  :lighter " Emacs"
  (if etaf-epub-user-keys-mode
      (progn
        (etaf-epub-user-keys--setup)
        (message "Emacs keys: set the mark and move the cursor as usual"))
    (etaf-epub-user-keys--teardown)))

;;;###autoload
(defun etaf-epub-selection-copy ()
  "Copy the marked page text without the rendering padding.
The mark is used whether or not the region is highlighted, since
`transient-mark-mode' has nothing to do with what was selected."
  (interactive)
  (unless (and (mark t) (/= (mark t) (point)))
    (user-error "Nothing selected: set the mark first"))
  (let ((text (etaf-epub--trim-trailing-whitespace
               (buffer-substring-no-properties (region-beginning)
                                               (region-end)))))
    (kill-new text)
    (when (bound-and-true-p etaf-epub-user-keys-mode)
      (etaf-epub-user-keys-mode -1))
    (message "Copied %d characters" (length text))))

;;; Key bindings

(define-key etaf-epub-mode-map (kbd "n") #'etaf-epub-next-document)
(define-key etaf-epub-mode-map (kbd "p") #'etaf-epub-previous-document)
(define-key etaf-epub-mode-map (kbd "]") #'etaf-epub-next-document)
(define-key etaf-epub-mode-map (kbd "[") #'etaf-epub-previous-document)
(define-key etaf-epub-mode-map (kbd "t") #'etaf-epub-goto-toc)
(define-key etaf-epub-mode-map (kbd "l") #'etaf-epub-history-back)
(define-key etaf-epub-mode-map (kbd "r") #'etaf-epub-history-forward)
(define-key etaf-epub-mode-map (kbd "f") #'etaf-epub-follow-link)
(define-key etaf-epub-mode-map (kbd "w") #'etaf-epub-copy-url)
(define-key etaf-epub-mode-map (kbd "v") #'etaf-epub-user-keys-mode)

;;; recentf interop

(defun etaf-epub-add-to-recentf ()
  "Add real path to recentf list if possible."
  (when etaf-epub-file-name
    (recentf-add-file etaf-epub-file-name)))

(add-hook 'etaf-epub-mode-hook 'etaf-epub-add-to-recentf)
(add-hook 'etaf-epub-mode-hook 'hack-dir-local-variables-non-file-buffer)

(defun etaf-epub--find-file (file index point)
  "Open FILE in etaf-epub-mode and go to the specified INDEX and POINT.
If FILE is nil, the current buffer is used."
  (when file
    (find-file file))
  (unless (eq major-mode 'etaf-epub-mode)
    (etaf-epub-mode))
  (when (not (etaf-epub--index-valid-p etaf-epub-documents index))
    (error "Invalid documents index"))
  (setq etaf-epub-documents-index index)
  (etaf-epub-render-document)
  (etaf-next-tick (lambda () (goto-char point))))

;; Bookmark interop
(defun etaf-epub-bookmark-make-record ()
  "Create a bookmark EPUB record."
  (cons (buffer-name)
        `((filename . ,etaf-epub-file-name)
          (index . ,etaf-epub-documents-index)
          (position . ,(point))
          (handler . etaf-epub-bookmark-jump-handler))))

;;;###autoload
(defun etaf-epub-bookmark-jump-handler (bmk)
  "The bookmark handler-function interface for bookmark BMK.

See also `etaf-epub-bookmark-make-record'."
  (let ((file (bookmark-prop-get bmk 'filename))
        (index (bookmark-prop-get bmk 'index))
        (position (bookmark-prop-get bmk 'position)))
    (etaf-epub--find-file file index position)))

;;; Org interop

;;;###autoload
(defun etaf-epub-org-link-follow (path)
  "Follow etaf-epub: link designated by PATH."
  (if (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path)
      (let ((file (match-string 1 path))
            (index (string-to-number (match-string 2 path)))
            (point (string-to-number (match-string 3 path))))
        (etaf-epub--find-file file index point))
    (error "Invalid etaf-epub.el link")))

;;;###autoload
(defun etaf-epub-org-link-store ()
  "Store current EPUB location as etaf-epub: link."
  (when (and (eq major-mode 'etaf-epub-mode) etaf-epub-file-name)
    (when (not (integerp etaf-epub-documents-index))
      (setq etaf-epub-documents-index 0))
    (let ((org-store-props-function
           (if (fboundp 'org-link-store-props)
               'org-link-store-props
             'org-store-link-props))
          (link (format "etaf-epub:%s::%d:%d"
                        etaf-epub-file-name
                        etaf-epub-documents-index
                        (point)))
          (description (or (alist-get 'title etaf-epub-metadata)
                           (format "EPUB file at %s" etaf-epub-file-name))))
      (funcall org-store-props-function
               :type "etaf-epub"
               :link link
               :author (alist-get 'creator etaf-epub-metadata)
               :language (alist-get 'language etaf-epub-metadata)
               :initial (alist-get 'description etaf-epub-metadata)
               :publisher (alist-get 'publisher etaf-epub-metadata)
               :description description))))

;;;###autoload
(cond
 ((fboundp 'org-link-set-parameters)
  (org-link-set-parameters
   "etaf-epub"
   :follow 'etaf-epub-org-link-follow
   :store 'etaf-epub-org-link-store))
 ((fboundp 'org-add-link-type)
  (org-add-link-type "etaf-epub" 'etaf-epub-org-link-follow)
  (add-hook 'org-store-link-functions 'etaf-epub-org-link-store)))

;;; Imenu interop

(defun etaf-epub-imenu-goto-function (_name filename target)
  "Visit imenu item using FILENAME and TARGET."
  ;; Make sure file is visited relative to toc file.
  (let ((etaf-epub-documents-index 0))
    (etaf-epub-visit-relative-file filename target)))

(defun etaf-epub--dom-text (node)
  "Return the concatenated text content of NODE."
  (if (fboundp 'dom-inner-text)
      (dom-inner-text node)
    (with-no-warnings (dom-text node))))

(defun etaf-epub-imenu-create-index ()
  "Generate Imenu index."
  (let* ((toc-path (cdr (aref etaf-epub-documents 0)))
         (ncxp (version< etaf-epub-epub-version "3.0"))
         (toc (with-temp-buffer
                (if ncxp
                    (insert (etaf-epub-ncx-to-html toc-path))
                  (insert-file-contents toc-path))
                (libxml-parse-html-region (point-min) (point-max)))))
    (mapcar
     (lambda (node)
       (let ((href (dom-attr node 'href))
             (label (etaf-epub--dom-text node)))
         (seq-let (filename target) (etaf-epub-url-filename-and-target href)
           (list label filename 'etaf-epub-imenu-goto-function target))))
     (esxml-query-all "a" toc))))

(defun etaf-epub-imenu-setup ()
  "Install the EPUB Imenu index function in the current buffer."
  (setq imenu-create-index-function 'etaf-epub-imenu-create-index))
(add-hook 'etaf-epub-mode-hook 'etaf-epub-imenu-setup)

;;; multi-isearch interop

(defun etaf-epub-misearch-next-buffer (_buffer wrap)
  "Return the next EPUB document buffer for multi-isearch.
WRAP non-nil wraps around at the last document."
  (if isearch-forward
      (cond
       ((< etaf-epub-documents-index (1- (length etaf-epub-documents)))
        (etaf-epub-goto-document (1+ etaf-epub-documents-index))
        (current-buffer))
       (wrap
        (etaf-epub-goto-document 0)
        nil))
    (cond
     ((> etaf-epub-documents-index 0)
      (etaf-epub-goto-document (1- etaf-epub-documents-index))
      (current-buffer))
     (wrap
      (etaf-epub-goto-document (1- (length etaf-epub-documents)))
      nil))))

(defun etaf-epub-misearch-setup ()
  "Install EPUB multi-isearch support in the current buffer."
  (setq-local multi-isearch-next-buffer-function #'etaf-epub-misearch-next-buffer))
(add-hook 'etaf-epub-mode-hook #'etaf-epub-misearch-setup)

(provide 'etaf-epub)
;;; etaf-epub.el ends here
