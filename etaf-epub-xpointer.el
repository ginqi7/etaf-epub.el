;;; etaf-epub-xpointer.el --- CREngine XPointers  -*- lexical-binding: t; -*-

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

;; Support reading progress stored in the XPointer format used by
;; CREngine, KOReader and friends, e.g.
;;
;;   /body/DocFragment[7]/body/div[2]/p[24]/text()[1].12
;;
;; The XPointer names a document by its 1-based position in the spine
;; (`DocFragment[N]'), then a path inside that document's DOM, then an
;; offset into the final text node.  Jumping is a three step process:
;;
;;   1. map `DocFragment[N]' to an entry of `etaf-epub-documents',
;;   2. resolve the DOM path against the source document, degrading
;;      gracefully to the deepest resolvable ancestor, as CREngine's
;;      DOM and the source document rarely match exactly,
;;   3. translate the DOM position to a buffer position.  The buffer
;;      holds the rendered text, not the markup, so the anchor text is
;;      searched for; the fraction of the document that precedes the
;;      anchor disambiguates repeated text, and doubles as a fallback
;;      when the anchor cannot be found.
;;
;; The reverse direction, `etaf-epub-xpointer-at-point', locates the
;; text of each node of the document in the buffer, in document order.
;; That yields the node under point and the offset inside it, and it
;; survives the decorations the renderer adds -- bullets, list numbers,
;; the brackets around links -- because those belong to no text node.
;;
;; `etaf-epub' renders through ETAF, which wraps text to a fixed page
;; width and justifies it, so the buffer holds hard line breaks, extra
;; spacing, zero-width markers for link targets and box borders around
;; quoted text.  All of that is whitespace or punctuation, which the
;; anchor search below skips.  ETAF also inserts whole words in two
;; places -- `image' for a figure and the number in front of a list
;; item -- so when the literal anchor cannot be found, a skeleton of
;; its letters and digits is matched instead, which tolerates them.

;;; Code:

(require 'dom)
(require 'seq)
(require 'subr-x)

(require 'etaf-epub)

(defgroup etaf-epub-xpointer nil
  "Read and write CREngine XPointers in etaf-epub buffers."
  :group 'etaf-epub)

;;; Parsing

(defun etaf-epub-xpointer--components (xpointer)
  "Split XPOINTER into its path components."
  (seq-remove #'string-empty-p
              (split-string (string-trim xpointer) "/")))

(defun etaf-epub-xpointer--parse-step (step)
  "Return (TAG . INDEX) for one XPointer path component STEP.
TAG is the symbol `text' for text nodes."
  (cond
   ((string-match "\\`text()[[:blank:]]*\\(?:\\[\\([0-9]+\\)\\]\\)?\\'" step)
    (cons 'text (if (match-string 1 step)
                    (string-to-number (match-string 1 step))
                  1)))
   ((string-match "\\`\\([^][[:blank:]]+\\)\\(?:\\[\\([0-9]+\\)\\]\\)?\\'" step)
    (cons (intern (downcase (match-string 1 step)))
          (if (match-string 2 step)
              (string-to-number (match-string 2 step))
            1)))
   ;; Neither a known element step nor a text step: keep it so that
   ;; resolution fails gracefully instead of aborting the jump.
   (t (cons 'unknown 1))))

(defun etaf-epub-xpointer-parse (xpointer)
  "Parse CREngine XPOINTER.
Return a list (FRAGMENT STEPS OFFSET).  FRAGMENT is the 1-based
`DocFragment' index, STEPS a list of (TAG . INDEX) pairs and
OFFSET the character offset into the target text node."
  (unless (stringp xpointer)
    (user-error "XPointer must be a string"))
  (let ((components (etaf-epub-xpointer--components xpointer))
        fragment offset)
    (while (and components (not fragment))
      (when (string-match "\\`DocFragment\\[\\([0-9]+\\)\\]\\'" (car components))
        (setq fragment (string-to-number (match-string 1 (car components)))))
      (setq components (cdr components)))
    (unless fragment
      (user-error "Not a CREngine XPointer: %s" xpointer))
    ;; The offset is appended to the last component: `text()[1].12'.
    (when components
      (let ((last (car (last components))))
        (when (string-match "\\`\\(.*\\)\\.\\([0-9]+\\)\\'" last)
          (setq offset (string-to-number (match-string 2 last)))
          (setq components (append (butlast components)
                                   (list (match-string 1 last)))))))
    (list fragment
          (mapcar #'etaf-epub-xpointer--parse-step components)
          (or offset 0))))

;;; DOM

(defun etaf-epub-xpointer--element-p (node)
  "Return non-nil if NODE is an element node."
  (and (consp node) (symbolp (car node))))

(defun etaf-epub-xpointer--elements (node)
  "Return the element children of NODE."
  (seq-filter #'etaf-epub-xpointer--element-p (dom-children node)))

(defun etaf-epub-xpointer--tag-equal (node tag)
  "Return non-nil if NODE's tag is TAG, ignoring case."
  (eq (intern (downcase (symbol-name (dom-tag node)))) tag))

(defun etaf-epub-xpointer--child (node tag index)
  "Return the element child of NODE named TAG at INDEX."
  (let ((n 0) result)
    (catch 'done
      (dolist (child (etaf-epub-xpointer--elements node))
        (when (etaf-epub-xpointer--tag-equal child tag)
          (setq n (1+ n))
          (when (= n index)
            (setq result child)
            (throw 'done nil)))))
    result))

(defun etaf-epub-xpointer--text-node (node index &optional skip-blank)
  "Return the text child of NODE at INDEX.
With SKIP-BLANK, whitespace-only text nodes are not counted, which
is how CREngine numbers them."
  (let ((n 0) result)
    (catch 'done
      (dolist (child (dom-children node))
        (when (and (stringp child)
                   (or (not skip-blank) (not (string-empty-p (string-trim child)))))
          (setq n (1+ n))
          (when (= n index)
            (setq result child)
            (throw 'done nil)))))
    result))

(defun etaf-epub-xpointer--root (dom)
  "Return the element that XPointer paths of DOM are relative to.
CREngine paths start at the fragment's `body', so the head of the
document is left out of the position estimates as well."
  (or (etaf-epub-xpointer--child dom 'body 1) dom))

(defun etaf-epub-xpointer--resolve (root steps &optional skip-blank)
  "Follow STEPS down ROOT, returning (TARGET CHAIN COMPLETE).
SKIP-BLANK is passed to `etaf-epub-xpointer--text-node'.
TARGET is the resolved text node or element, CHAIN the list of
nodes from ROOT down to TARGET and COMPLETE non-nil when every
step resolved.  When a step cannot be resolved, the deepest
resolvable ancestor is returned instead, as CREngine's DOM does
not always match the source document."
  (let ((node root) (chain (list root)) done complete)
    ;; An XPointer that names only the fragment starts at its body.
    (setq steps (or steps (list (cons 'body 1))))
    ;; A leading `body' step describes the fragment itself.
    (when (eq (car (car steps)) 'body)
      (setq steps (cdr steps)))
    (while (and steps (not done))
      (let* ((step (car steps))
             (tag (car step))
             (index (cdr step))
             (child (if (eq tag 'text)
                        (etaf-epub-xpointer--text-node node index skip-blank)
                      (etaf-epub-xpointer--child node tag index))))
        (setq steps (cdr steps))
        (cond
         ((null child) (setq done t))
         ((eq tag 'text)
          (setq node child)
          (push node chain)
          (setq done t
                complete t))
         (t
          (setq node child)
          (push node chain)
          (when (null steps)
            (setq complete t))))))
    (list node (nreverse chain) complete)))

(defun etaf-epub-xpointer--text (node)
  "Return the text content of NODE."
  (cond
   ((stringp node) node)
   ((etaf-epub-xpointer--element-p node)
    (if (fboundp 'dom-inner-text)
        (dom-inner-text node)
      (with-suppressed-warnings ((obsolete dom-texts))
        (dom-texts node ""))))
   (t "")))

(defun etaf-epub-xpointer--text-length (node)
  "Return the number of characters of text contained in NODE."
  (cond
   ((stringp node) (length node))
   ((etaf-epub-xpointer--element-p node)
    (seq-reduce (lambda (total child)
                  (+ total (etaf-epub-xpointer--text-length child)))
                (dom-children node) 0))
   (t 0)))

(defun etaf-epub-xpointer--preceding-length (node path)
  "Return the length of the text in NODE that precedes PATH.
PATH is a list of descendants, from a child of NODE down to the
target node."
  (if (null path)
      0
    (let ((total 0))
      (catch 'done
        (dolist (child (dom-children node))
          (if (eq child (car path))
              (progn
                (setq total (+ total (etaf-epub-xpointer--preceding-length
                                      child (cdr path))))
                (throw 'done nil))
            (setq total (+ total (etaf-epub-xpointer--text-length child))))))
      total)))

(defun etaf-epub-xpointer--document-dom ()
  "Return the parsed DOM of the current EPUB document, or nil."
  (let ((path (cdr (aref etaf-epub-documents etaf-epub-documents-index))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (or (condition-case nil
                (libxml-parse-xml-region (point-min) (point-max))
              (error nil))
            (condition-case nil
                (libxml-parse-html-region (point-min) (point-max))
              (error nil)))))))

;;; Locating the position in the rendered buffer

(defun etaf-epub-xpointer--snippets (root chain offset)
  "Return anchors for the target of CHAIN at OFFSET, longest first.
CHAIN is the list of DOM nodes from ROOT down to the target.  Each
anchor starts at that very position and continues past the end of
the target node, so a position near the end of a text node still
yields an anchor long enough to be unambiguous.  Shorter anchors
are the fallback for text the renderer decorates, such as the
brackets it puts around links or the numbers it puts before the
items of a list."
  (let* ((text (etaf-epub-xpointer--text root))
         (start (+ offset (etaf-epub-xpointer--preceding-length
                           root (cdr chain))))
         result)
    (dolist (length '(40 24 12 6))
      (let ((snippet (string-trim
                      (substring text (min start (length text))
                                 (min (length text) (+ start length))))))
        (when (>= (length snippet) 2)
          (push snippet result))))
    (seq-uniq (nreverse result))))

(defun etaf-epub-xpointer--regexp (string)
  "Return a regexp matching STRING, allowing whitespace between characters.
Both the renderer and CREngine reflow text, so a line break may
appear anywhere, including between two CJK characters."
  (mapconcat (lambda (char)
               (if (memq char '(?\s ?\t ?\n ?\r))
                   "[[:space:]]*"
                 (regexp-quote (char-to-string char))))
             (string-to-list string)
             "[[:space:]]*"))

(defun etaf-epub-xpointer--skeleton (string)
  "Return a regexp matching the letters and digits of STRING in order.
Anything else in the buffer is allowed between them.  When the
renderer has inserted a word inside the text -- `image' for a
figure, the number of a list item -- the escaped text is not in
the buffer, but its letters and digits still are, in order.  Return
nil when STRING holds too few of them to be worth matching."
  (let (chars)
    (dolist (char (string-to-list string))
      (when (string-match-p "[[:alnum:]]" (char-to-string char))
        (push (regexp-quote (char-to-string char)) chars)))
    (when (>= (length chars) 6)
      (mapconcat #'identity (nreverse chars) "[^[:alnum:]]*"))))

(defun etaf-epub-xpointer--find (regexp ratio)
  "Return the position in the current buffer that best matches REGEXP.
RATIO, a number between 0 and 1, is the position of the target
within the document; it selects among repeated matches."
  (let* ((expected (+ (point-min)
                      (round (* ratio (max 0 (- (point-max) (point-min)))))))
         (best nil)
         (best-distance nil)
         (matches 0))
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (and (< matches 200) (re-search-forward regexp nil t))
        (setq matches (1+ matches))
        (let ((position (match-beginning 0))
              (closeness (abs (- (match-beginning 0) expected))))
          (when (or (null best-distance) (< closeness best-distance))
            (setq best position
                  best-distance closeness)))))
    best))

;;; Jumping

(defun etaf-epub-xpointer--spine ()
  "Return the identifiers of the spine documents, in reading order."
  (or etaf-epub-spine-ids
      ;; Fall back for buffers whose spine was not recorded, e.g. one
      ;; opened before this feature.
      (delq nil
            (mapcar (lambda (doc)
                      (unless (eq (car doc) etaf-epub-toc-id)
                        (car doc)))
                    (append etaf-epub-documents nil)))))

(defun etaf-epub-xpointer--document-index (fragment)
  "Return the `etaf-epub-documents' index of 1-based spine FRAGMENT."
  (when (> fragment 0)
    (let ((id (nth (1- fragment) (etaf-epub-xpointer--spine))))
      (and id (etaf-epub-find-document (lambda (doc) (eq (car doc) id)))))))

(defun etaf-epub-xpointer--default ()
  "Return a likely XPointer to use as the default for prompting.
Use the last position recorded by KOReader when available, and the
top of the kill ring when it holds an XPointer."
  (or (ignore-errors (etaf-epub-xpointer-koreader))
      (let ((kill (ignore-errors (current-kill 0 t))))
        (when (and (stringp kill)
                   (string-match-p "DocFragment\\[[0-9]+\\]" kill))
          (string-trim kill)))))

(defun etaf-epub-xpointer--goto-ratio (ratio)
  "Move point to RATIO, a number between 0 and 1, of the buffer."
  (goto-char (min (point-max)
                  (+ (point-min)
                     (round (* ratio (- (point-max) (point-min))))))))

(defun etaf-epub-xpointer--locate (root steps offset skip-blank)
  "Return (RATIO . POSITION) for STEPS resolved in ROOT.
POSITION is where the text of the resolved node was found in the
buffer, or nil when it could not be found; RATIO then says how far
into the document the position is estimated to be.  OFFSET is the
offset the XPointer asks for and SKIP-BLANK is passed on to
`etaf-epub-xpointer--resolve'."
  (seq-let (target chain complete) (etaf-epub-xpointer--resolve root steps skip-blank)
    ;; A partially resolved path says nothing about where inside the
    ;; ancestor the target sits, so its offset is dropped.
    (let* ((offset (if complete offset 0))
           (text (etaf-epub-xpointer--text target))
           (offset (min offset (length text)))
           (total (max 1 (etaf-epub-xpointer--text-length root)))
           (before (+ offset (etaf-epub-xpointer--preceding-length
                              root (cdr chain))))
           (ratio (/ (min before total) (float total)))
           (snippets (etaf-epub-xpointer--snippets root chain offset))
           (regexps (delq nil (append (mapcar #'etaf-epub-xpointer--regexp snippets)
                                      (mapcar #'etaf-epub-xpointer--skeleton snippets))))
           position)
      (dolist (regexp regexps)
        (unless position
          (setq position (etaf-epub-xpointer--find regexp ratio))))
      (cons ratio position))))

;;;###autoload
(defun etaf-epub-xpointer-goto (xpointer)
  "Jump to the reading position described by CREngine XPOINTER.
XPOINTER looks like

  /body/DocFragment[7]/body/div[2]/p[24]/text()[1].12

where `DocFragment[7]' is the seventh spine document and `text()[1].12'
is the twelfth character of that paragraph's text node.  Shorter
forms such as `/body/DocFragment[7]' or `.../text().12' are
accepted too, as are paths that CREngine's DOM does not exactly
share with the source document.  Return the position point was
moved to."
  (interactive (list (read-string "XPointer: " (etaf-epub-xpointer--default))))
  (with-current-buffer (if etaf-epub-documents
                           (current-buffer)
                         (etaf-epub--state-buffer))
    (seq-let (fragment steps offset) (etaf-epub-xpointer-parse xpointer)
      (let ((index (etaf-epub-xpointer--document-index fragment)))
        (unless index
          (user-error "DocFragment[%d] is not part of this EPUB" fragment))
        (etaf-epub-goto-document index)
        (let* ((dom (etaf-epub-xpointer--document-dom))
               (root (and dom (etaf-epub-xpointer--root dom)))
               (ratio 0.0)
               (position nil))
          (when root
            ;; CREngine numbers text nodes without the whitespace-only
            ;; ones, the source document may or may not have them, so
            ;; both readings are tried before giving up on the text.
            (let* ((first (etaf-epub-xpointer--locate root steps offset t))
                   (second (and (null (cdr first))
                                (etaf-epub-xpointer--locate root steps offset nil)))
                   (location (or (and (cdr first) first) second first)))
              (setq ratio (car location)
                    position (cdr location))))
          (cond
           (position (goto-char position))
           (root
            (etaf-epub-warn
             (format "Couldn't locate XPointer text, approximating from %d%%"
                     (round (* 100 ratio))))
            (etaf-epub-xpointer--goto-ratio ratio))
           (t (goto-char (point-min)))))
        (when (get-buffer-window (current-buffer))
          (recenter (1- (max 1 scroll-margin))))
        (point)))))

;;; Producing XPointers

(defun etaf-epub-xpointer--fragment (index)
  "Return the 1-based `DocFragment' number of `etaf-epub-documents' INDEX."
  (let* ((id (and (integerp index)
                  (>= index 0)
                  (< index (length etaf-epub-documents))
                  (car (aref etaf-epub-documents index))))
         (position (and id (seq-position (etaf-epub-xpointer--spine) id))))
    (when position
      (1+ position))))

(defun etaf-epub-xpointer--step-string (steps)
  "Format STEPS as the path portion of an XPointer."
  (mapconcat (lambda (step)
               (if (eq (car step) 'text)
                   (format "/text()[%d]" (cdr step))
                 (format "/%s[%d]" (car step) (cdr step))))
             steps ""))

(defun etaf-epub-xpointer--segments (node prefix)
  "Return (TEXT . STEPS) for every text node under NODE.
PREFIX is prepended to STEPS, so that recursion can build the
XPointer path from the root down to each text node.  The list is in
document order.  Whitespace-only nodes are skipped and not counted,
which is how CREngine numbers text nodes."
  (let ((result nil)
        (tag-counts (make-hash-table :test #'eq))
        (text-index 0))
    (dolist (child (dom-children node))
      (cond
       ((stringp child)
        (unless (string-empty-p (string-trim child))
          (setq text-index (1+ text-index))
          (push (cons child (append prefix (list (cons 'text text-index))))
                result)))
       ((etaf-epub-xpointer--element-p child)
        (let* ((tag (intern (downcase (symbol-name (dom-tag child)))))
               (index (1+ (gethash tag tag-counts 0))))
          (puthash tag index tag-counts)
          (dolist (segment (etaf-epub-xpointer--segments
                            child (append prefix (list (cons tag index)))))
            (push segment result))))))
    (nreverse result)))

(defun etaf-epub-xpointer--search-text (text from)
  "Return the buffer position where TEXT starts, at or after FROM, or nil.
Only the beginning of TEXT is used, so a text node whose tail was
changed by the renderer can still be found."
  (let* ((length 20)
         (snippet (substring text 0 (min (length text) length)))
         (position nil))
    (when (>= (length (string-trim snippet)) 2)
      (let ((case-fold-search nil))
        (save-excursion
          (goto-char from)
          (when (re-search-forward (etaf-epub-xpointer--regexp snippet) nil t)
            (setq position (match-beginning 0))))
        (when (null position)
          (when-let* ((skeleton (etaf-epub-xpointer--skeleton snippet)))
            (save-excursion
              (goto-char from)
              (when (re-search-forward skeleton nil t)
                (setq position (match-beginning 0))))))))
    position))

(defun etaf-epub-xpointer--align (segments position)
  "Return ((START . SEGMENT) ...) mapping buffer positions to SEGMENTS.
SEGMENTS are matched in document order, each search starting where
the previous one ended, which keeps repeated text unambiguous.
Alignment stops once it runs past POSITION in the buffer."
  (let ((start (point-min))
        result)
    (catch 'done
      (dolist (segment segments)
        (let ((found (etaf-epub-xpointer--search-text (car segment) start)))
          (when found
            (push (cons found segment) result)
            (setq start found)))
        (when (> start position)
          (throw 'done nil))))
    (nreverse result)))

(defun etaf-epub-xpointer--aligned-segment (aligned position)
  "Return the (START . SEGMENT) of ALIGNED that best covers POSITION.
A position in the gap between two text nodes is given to the
closer one.  That matters for list items and links, whose bullet,
list number or opening bracket belong to no text node: pointing
at them should give the text they introduce, while pointing right
after a paragraph should still give that paragraph."
  (let ((item (car aligned))
        (next nil))
    (dolist (entry aligned)
      (cond
       ((<= (car entry) position) (setq item entry))
       ((null next) (setq next entry))))
    (if (and next
             (< (- (car next) position)
                (- position (+ (car item) (length (car (cdr item)))))))
        next
      item)))

(defun etaf-epub-xpointer--align-offset (text start position)
  "Return the offset in TEXT that corresponds to buffer POSITION.
START is the buffer position where TEXT begins.  TEXT is matched
against the buffer one character at a time, so the characters the
renderer adds -- extra whitespace, bullets, list numbers, the
brackets around links -- do not shift the offset."
  (let ((i 0)
        (j start)
        (length (length text))
        (stop (min position (point-max)))
        (space '(?\s ?\t ?\n ?\r ?\u00a0)))
    (while (and (< i length) (< j stop))
      (cond
       ;; whitespace of the source is always part of the offset
       ((memq (aref text i) space) (setq i (1+ i)))
       ((memq (char-after j) space) (setq j (1+ j)))
       ((eq (char-after j) (aref text i))
        (setq i (1+ i)
              j (1+ j)))
       ;; inserted by the renderer
       (t (setq j (1+ j)))))
    i))

;;;###autoload
(defun etaf-epub-xpointer-at-point (&optional position)
  "Return the CREngine XPointer for POSITION in the current EPUB buffer.
POSITION defaults to point.  Every text node of the current
document is located in the buffer in document order, which yields
the path of the node at POSITION and the offset inside it.  When
nothing can be located, for instance in an image-only document,
only the fragment is returned.  Interactively, the XPointer is
also put in the kill ring."
  (interactive "d")
  (with-current-buffer (if etaf-epub-documents
                           (current-buffer)
                         (etaf-epub--state-buffer))
    (let* ((position (or position (point)))
           (fragment (etaf-epub-xpointer--fragment etaf-epub-documents-index))
           (dom (etaf-epub-xpointer--document-dom))
           (root (and dom (etaf-epub-xpointer--root dom)))
           (segments (and root (etaf-epub-xpointer--segments root nil)))
           (aligned (and segments (etaf-epub-xpointer--align segments position)))
           (item (and aligned
                      (etaf-epub-xpointer--aligned-segment aligned position)))
           (segment (cdr item))
           (location (and item
                          (cons (min (etaf-epub-xpointer--align-offset
                                      (car segment) (car item) position)
                                     (length (car segment)))
                                (cdr segment)))))
      (unless fragment
        (user-error "Document %s is not part of the spine"
                    etaf-epub-documents-index))
      (let ((xpointer (if location
                          (format "/body/DocFragment[%d]/body%s.%d"
                                  fragment
                                  (etaf-epub-xpointer--step-string (cdr location))
                                  (car location))
                        (format "/body/DocFragment[%d]" fragment))))
        (when (called-interactively-p 'interactive)
          (kill-new xpointer)
          (message "%s" xpointer))
        xpointer))))

;;; KOReader interop

(defun etaf-epub-xpointer-koreader-metadata-file ()
  "Return the KOReader metadata file of the current EPUB, or nil."
  (let* ((epub (or etaf-epub-file-name buffer-file-name))
         (candidates
          (delq nil
                (list (and epub
                           (expand-file-name
                            (concat (file-name-nondirectory epub) ".sdr")
                            (file-name-directory epub)))
                      (car (directory-files default-directory t "sdr\\'"))))))
    (when-let* ((directory (seq-find (lambda (directory)
                                       (and directory
                                            (file-directory-p directory)))
                                     candidates)))
      (car (directory-files directory t "\\`metadata\\..*\\.lua\\'")))))

(defun etaf-epub-xpointer-koreader ()
  "Return the last XPointer recorded by KOReader, or nil."
  (when-let* ((file (etaf-epub-xpointer-koreader-metadata-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             "last_xpointer[\"']?\\]?[[:blank:]]*=[[:blank:]]*\"\\([^\"]+\\)\""
             nil t)
        (match-string 1)))))

;;;###autoload
(defun etaf-epub-goto-koreader-progress ()
  "Jump to the position last read in KOReader."
  (interactive)
  (if-let* ((xpointer (etaf-epub-xpointer-koreader)))
      (etaf-epub-xpointer-goto xpointer)
    (user-error "No KOReader progress found for this EPUB")))

;;; Key bindings

;; These keys are free in `etaf-epub-mode-map'.  The reader keys that
;; Ebox would capture are declared on ETAF's root Host instead, see
;; `etaf-epub--page-view'; `g' would otherwise be `revert-buffer', which
;; has nothing to revert in a rendered EPUB buffer.
(define-key etaf-epub-mode-map (kbd "x") #'etaf-epub-xpointer-at-point)
(define-key etaf-epub-mode-map (kbd "g") #'etaf-epub-xpointer-goto)
(define-key etaf-epub-mode-map (kbd "K") #'etaf-epub-goto-koreader-progress)

(provide 'etaf-epub-xpointer)
;;; etaf-epub-xpointer.el ends here
