;;; passages.el --- Seamless inline excerpts -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Your Name
;; Version: 3.0.0
;; Package-Requires: ((emacs "27.1") (pdf-tools "1.0") (nov "0.4.0"))
;; Keywords: pdf, epub, notes, annotations, denote, citar

;;; Commentary:

;; Passages provides a lightweight note-taking system for PDFs and EPUBs.
;; Features:
;; - Seamless inline excerpts with precise anchors
;; - Preserves original text formatting from PDFs
;; - Compact format: ● Pxx [content]
;; - Optimized single-overlay rendering system
;; - Robust file path resolution with intelligent search
;; - JIT-based incremental rendering with forced synchronization
;; - Protected metadata with modification hooks
;; - Fast O(1) jumping using text properties and marker parsing
;; - Full citar-denote integration for bibliography management
;; - Denote integration for general notes
;; - Bidirectional navigation: PDF ↔ Notes
;; - Comprehensive debugging utilities
;;
;; Architecture:
;; - `passages-mode': Minor mode for Org buffers (overlays, JIT-lock)
;; - `passages-doc-mode': Minor mode for PDF/EPUB buffers (keybindings)

;;; Code:

(require 'pdf-view)
(require 'nov)
(require 'org)
(require 'jit-lock)
(require 'subr-x)

;;; Customization

(defgroup passages nil
  "Settings for passages."
  :group 'text
  :prefix "passages-")

(defcustom passages-auto-parse t
  "Automatically parse excerpts when opening a file."
  :type 'boolean
  :group 'passages)

(defcustom passages-directory nil
  "Directory for storing note files."
  :type '(choice (const :tag "Next to source files" nil)
                 (directory :tag "Custom directory"))
  :group 'passages)

(defcustom passages-use-citar t
  "Try to use existing citar-denote notes if available."
  :type 'boolean
  :group 'passages)

(defcustom passages-use-denote t
  "Use denote for note management when citar-denote is not available."
  :type 'boolean
  :group 'passages)

(defcustom passages-denote-keyword "excerpt"
  "Denote keyword (file tag) for excerpt notes."
  :type 'string
  :group 'passages)

(defcustom passages-use-relative-paths t
  "Store relative paths instead of absolute paths for portability."
  :type 'boolean
  :group 'passages)

(defcustom passages-search-paths
  '("~/Library/CloudStorage/Dropbox/org/bib/files/"
    "~/Library/CloudStorage/Dropbox/org/books/"
    "~/Library/CloudStorage/Dropbox/org/")
  "Directories to search for source files when resolving relative paths.
Paths are tried in order until file is found."
  :type '(repeat directory)
  :group 'passages)

(defcustom passages-debug nil
  "Enable debug logging for troubleshooting."
  :type 'boolean
  :group 'passages)

(defcustom passages-smart-text-extraction t
  "Use position-based text extraction for PDFs.
When non-nil, uses character position information to properly
merge text, handling Drop Caps and mixed font sizes correctly."
  :type 'boolean
  :group 'passages)

;;; Faces

(defface passages-content-face
  '((t :foreground "#666666" :extend t))
  "Face for excerpt content (gray text)."
  :group 'passages)

(defface passages-marker-face
  '((t :foreground "#e6a23c"))
  "Face for page marker like '● P24'."
  :group 'passages)

(defface passages-separator-face
  '((((background light)) :foreground "#e0e0e0")
    (((background dark))  :foreground "#3a3a3a"))
  "Face for separator line -----."
  :group 'passages)

;;; Citar-Denote integration

(defun passages--citar-denote-available-p ()
  "Check if citar-denote is available and active."
  (and passages-use-citar
       (require 'citar nil t)
       (require 'citar-denote nil t)
       (bound-and-true-p citar-denote-mode)
       (fboundp 'citar-denote--get-notes)
       (fboundp 'denote-directory-files)))

(defun passages--expand-bib-file (filename)
  "Expand FILENAME relative to bibliography or library paths."
  (let ((possible-paths '()))

    (when (file-name-absolute-p filename)
      (push (expand-file-name filename) possible-paths))

    (when-let* ((lib-paths (bound-and-true-p citar-library-paths)))
      (dolist (lib-path lib-paths)
        (let ((full-path (expand-file-name filename lib-path)))
          (when (file-exists-p full-path)
            (push full-path possible-paths)))))

    (when-let* ((bib-files (bound-and-true-p citar-bibliography)))
      (dolist (bib-file bib-files)
        (when (file-exists-p bib-file)
          (let* ((bib-dir (file-name-directory bib-file))
                 (full-path (expand-file-name filename bib-dir)))
            (when (file-exists-p full-path)
              (push full-path possible-paths))))))

    (delete-dups possible-paths)))

(defun passages--find-citar-key-for-file (file)
  "Find citar citation key for FILE by searching bibliography entries."
  (when (require 'citar nil t)
    (let ((file-normalized (expand-file-name file))
          (file-name (file-name-nondirectory file)))

      (when passages-debug
        (message "=== Searching for citekey ===")
        (message "Looking for file: %s" file-normalized)
        (message "Filename: %s" file-name))

      (catch 'found
        (condition-case err
            (maphash
             (lambda (key entry-data)
               (let ((file-field (or (cdr (assoc "file" entry-data))
                                    (cdr (assoc "_file" entry-data)))))

                 (when (and file-field passages-debug)
                   (message "Entry %s has file field: %s" key file-field))

                 (when file-field
                   (let* ((bib-files (mapcar #'string-trim
                                            (split-string file-field ";" t "[ \t\n]+")))
                          (expanded-files (mapcan #'passages--expand-bib-file bib-files)))

                     (when passages-debug
                       (message "  Expanded to: %s" expanded-files))

                     (when (or (member file-normalized expanded-files)
                               (member file-name
                                      (mapcar #'file-name-nondirectory
                                             (append bib-files expanded-files))))
                       (when passages-debug
                         (message "  ✓ MATCH! Citekey: %s" key))
                       (throw 'found key))))))
             (citar-get-entries))
          (error
           (message "Error searching citar entries: %s" (error-message-string err))
           nil))

        (when passages-debug
          (message "  ✗ No match found"))
        nil))))

(defun passages--find-citar-note (source-file)
  "Try to find existing citar-denote note for SOURCE-FILE."
  (when (passages--citar-denote-available-p)
    (if-let* ((citekey (passages--find-citar-key-for-file source-file)))
        (condition-case nil
            (when-let* ((notes-hash (citar-denote--get-notes (list citekey)))
                        (note-files (gethash citekey notes-hash))
                        (note-file (car note-files)))
              (message "Using existing citar-denote note: %s"
                      (file-name-nondirectory note-file))
              (passages--normalize-path note-file))
          (error nil))
      (when passages-debug
        (message "No citation key found in bibliography for: %s"
                (file-name-nondirectory source-file))))))

;;; Denote integration

(defun passages--denote-available-p ()
  "Check if denote is available."
  (and passages-use-denote
       (require 'denote nil t)
       (fboundp 'denote)
       (fboundp 'denote-directory-files)
       (bound-and-true-p denote-directory)))

(defun passages--find-denote-note (source-file)
  "Find existing denote note for SOURCE-FILE."
  (when (passages--denote-available-p)
    (let* ((source-normalized (expand-file-name source-file))
           (source-name (file-name-nondirectory source-file))
           (excerpt-files (denote-directory-files
                          (concat "_" passages-denote-keyword))))
      (catch 'found
        (dolist (note-file excerpt-files)
          (with-temp-buffer
            (insert-file-contents note-file)
            (goto-char (point-min))
            (when-let* ((_ (re-search-forward
                           "^#\\+SOURCE_FILE:[ \t]*\\(.+\\)$" nil t))
                        (stored-path (string-trim (match-string 1)))
                        (resolved-path (passages--resolve-path stored-path))
                        (_ (or (string= resolved-path source-normalized)
                               (string= (file-name-nondirectory stored-path)
                                       source-name))))
              (message "Using existing denote note: %s"
                      (file-name-nondirectory note-file))
              (throw 'found (passages--normalize-path note-file)))))
        nil))))

(defun passages--create-denote-note (source-file)
  "Create a new denote note for SOURCE-FILE."
  (when (passages--denote-available-p)
    (let* ((title (concat "Notes: " (file-name-nondirectory source-file)))
           (keywords (list passages-denote-keyword))
           (subdirectory nil)
           ;; Temporarily prevent denote from opening the new file
           ;; This avoids disrupting the current window/buffer
           (denote-open-after-create nil)
           (note-file (save-window-excursion
                        (denote
                         title
                         keywords
                         'org
                         subdirectory
                         nil
                         nil
                         nil))))

      (with-current-buffer (find-file-noselect note-file)
        (save-excursion
          (goto-char (point-min))
          (while (and (not (eobp))
                     (looking-at "^#\\+"))
            (forward-line 1))
          (insert "#+SOURCE_FILE: "
                 (passages--make-relative-path source-file)
                 "\n\n")
          (save-buffer)))

      (message "Created denote note: %s" (file-name-nondirectory note-file))
      (passages--normalize-path note-file))))

;;; Anchor utilities

(defun passages--get-location-page (location)
  "Get page/chapter from LOCATION."
  (cond
   ((numberp location) location)
   ((consp location) (car location))
   ((stringp location)
    (when (string-match "\\([0-9]+\\)" location)
      (string-to-number (match-string 1 location))))
   (t 0)))

(defun passages--get-location-top (location)
  "Get vertical position from LOCATION."
  (cond
   ((numberp location) 0)
   ((consp location)
    (if (consp (cdr location))
        (cadr location)
      (cdr location)))
   (t 0)))

(defun passages--get-location-left (location)
  "Get horizontal position from LOCATION."
  (when (and (consp location) (consp (cdr location)))
    (cddr location)))

;;; Path utilities

(defun passages--get-project-root ()
  "Get project root directory if available."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (and (fboundp 'project-current)
           (when-let* ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj)))))
      default-directory))

(defun passages--normalize-path (path)
  "Normalize PATH to canonical absolute path."
  (expand-file-name path))

(defun passages--make-relative-path (absolute-path)
  "Convert ABSOLUTE-PATH to relative path if possible."
  (let ((project-root (when passages-use-relative-paths
                        (passages--get-project-root))))
    (if (and project-root (string-prefix-p project-root absolute-path))
        (file-relative-name absolute-path project-root)
      absolute-path)))

(defun passages--find-file-in-search-paths (filename)
  "Search for FILENAME in configured search paths.
Returns absolute path if found, nil otherwise."
  (catch 'found
    ;; Try configured search paths
    (dolist (dir passages-search-paths)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-directory-p expanded-dir)
          (let ((candidate (expand-file-name filename expanded-dir)))
            (when (file-exists-p candidate)
              (throw 'found candidate))))))

    ;; Try citar-library-paths if available
    (when (bound-and-true-p citar-library-paths)
      (dolist (dir citar-library-paths)
        (let ((candidate (expand-file-name filename dir)))
          (when (file-exists-p candidate)
            (throw 'found candidate)))))

    ;; Try recursive search in project root (slower, last resort)
    (let ((project-root (passages--get-project-root)))
      (when (and project-root (file-directory-p project-root))
        (condition-case nil
            (when-let* ((matches (directory-files-recursively
                                 project-root
                                 (concat "^" (regexp-quote filename) "$")
                                 nil)))
              (throw 'found (car matches)))
          (error nil))))

    nil))

(defun passages--buffer-base-dir ()
  "Get base directory for resolving relative paths."
  (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
      (passages--get-project-root)))

(defun passages--resolve-path (path)
  "Resolve PATH to absolute path with intelligent search.
Handles:
1. Absolute paths - returned as-is
2. Relative paths with directory - resolved relative to current buffer/project
3. Bare filenames - searched in configured directories"
  (cond
   ((file-name-absolute-p path)
    (passages--normalize-path path))
   ((string-match-p "/" path)
    (passages--normalize-path
     (expand-file-name path (passages--buffer-base-dir))))
   (t
    (or (passages--find-file-in-search-paths path)
        (passages--normalize-path
         (expand-file-name path (passages--buffer-base-dir)))))))

;;; PDF functions

(defun passages--compute-bounding-box (edge-lists)
  "Compute bounding box (LEFT TOP RIGHT BOTTOM) from multiple EDGE-LISTS.
Each element of EDGE-LISTS should be (L T R B)."
  (when edge-lists
    (let ((min-left (apply #'min (mapcar (lambda (e) (nth 0 e)) edge-lists)))
          (min-top (apply #'min (mapcar (lambda (e) (nth 1 e)) edge-lists)))
          (max-right (apply #'max (mapcar (lambda (e) (nth 2 e)) edge-lists)))
          (max-bottom (apply #'max (mapcar (lambda (e) (nth 3 e)) edge-lists))))
      (list min-left min-top max-right max-bottom))))

(defun passages--pdf-extract-edges (region)
  "Extract a single (LEFT TOP RIGHT BOTTOM) edge list from REGION.
REGION is the return value of `pdf-view-active-region', which may be:
  - ((L T R B) ...)      — list of edge lists (standard)
  - (L T R B)            — flat edge list
  - (PAGE (L T R B) ...) — page number followed by edge lists
  - other                — unexpected format, return nil.
When multiple edge lists exist, compute the bounding box of all."
  (cond
   ;; (PAGE (L T R B) ...) — first element is page number, rest are edges
   ((and (consp region)
         (numberp (car region))
         (consp (cadr region))
         (numberp (caadr region)))
    (passages--compute-bounding-box (cdr region)))
   ;; ((L T R B) ...) — list of edge lists, compute bounding box
   ((and (consp region)
         (consp (car region))
         (numberp (caar region)))
    (passages--compute-bounding-box region))
   ;; (L T R B) — flat list of numbers, at least 4 elements
   ((and (consp region)
         (numberp (car region))
         (nthcdr 3 region))
    region)
   (t
    (when passages-debug
      (message "passages: unexpected pdf-view-active-region format: %S" region))
    nil)))

(defun passages--pdf-get-precise-location ()
  "Get precise location in PDF."
  (let ((page (pdf-view-current-page))
        v-pos h-pos)
    (if-let* ((_ (pdf-view-active-region-p))
              (edges (passages--pdf-extract-edges (pdf-view-active-region))))
        (setq v-pos (min (nth 1 edges) (nth 3 edges))
              h-pos (min (nth 0 edges) (nth 2 edges)))
      (setq v-pos (passages--conv-page-scroll-percentage
                   (window-vscroll))))
    (if h-pos
        (cons page (cons v-pos h-pos))
      (cons page v-pos))))

(defun passages--pdf-goto-location (file location)
  "Jump to LOCATION in PDF FILE."
  (let* ((resolved-file (passages--resolve-path file))
         (existing-buf (find-buffer-visiting resolved-file))
         (page (passages--get-location-page location))
         (v-pos (passages--get-location-top location)))
    (if existing-buf
        (pop-to-buffer existing-buf)
      (find-file-other-window resolved-file))
    (pdf-view-goto-page page)
    (when (and (numberp v-pos) (> v-pos 0))
      (image-scroll-up (- (passages--conv-page-percentage-scroll v-pos)
                          (window-vscroll))))))

;;; EPUB functions

(defun passages--epub-get-precise-location ()
  "Get precise location in EPUB."
  (let ((chapter nov-documents-index))
    (if (region-active-p)
        (cons chapter (cons (region-beginning) (region-end)))
      (cons chapter (point)))))

(defun passages--epub-try-anchor-search (initial-pos anchor-text)
  "Try to locate ANCHOR-TEXT near INITIAL-POS in the current chapter.
If found within 200 chars, do nothing.  Otherwise search from chapter
start and reposition point.  Falls back to INITIAL-POS if not found."
  (let* ((nearby-start (max (point-min) (- initial-pos 200)))
         (nearby-end (min (point-max) (+ initial-pos 200)))
         (found-nearby (save-excursion
                         (goto-char nearby-start)
                         (search-forward anchor-text nearby-end t))))
    (unless found-nearby
      (goto-char (point-min))
      (if (search-forward anchor-text nil t)
          (progn
            (goto-char (match-beginning 0))
            (when passages-debug
              (message "Anchor text found at %d (original pos was %d)"
                       (point) initial-pos)))
        (goto-char initial-pos)
        (when passages-debug
          (message "Anchor text not found, using original position"))))))

(defun passages--epub-goto-location (file location &optional anchor-text)
  "Jump to LOCATION in EPUB FILE using nov.el native methods.
If ANCHOR-TEXT is provided and the text at the initial position
does not match, search forward in the chapter to find it."
  (let* ((resolved-file (passages--resolve-path file))
         (existing-buf (find-buffer-visiting resolved-file))
         (chapter (passages--get-location-page location))
         (pos (passages--get-location-top location)))
    (if existing-buf
        (pop-to-buffer existing-buf)
      (find-file-other-window resolved-file))
    (unless (eq major-mode 'nov-mode)
      (nov-mode))
    (when (and (boundp 'nov-documents) nov-documents)
      (setq nov-documents-index chapter)
      (nov-render-document))
    (let ((initial-pos (min (round pos) (point-max))))
      (goto-char initial-pos)
      (when (and anchor-text (> (length anchor-text) 0))
        (passages--epub-try-anchor-search initial-pos anchor-text)))
    (recenter)))

;;; Generic functions

(defun passages--get-current-location ()
  "Get current precise location."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (list :type 'pdf
          :file (passages--make-relative-path (buffer-file-name))
          :location (passages--pdf-get-precise-location)))
   ((eq major-mode 'nov-mode)
    (list :type 'epub
          :file (passages--make-relative-path
                 (or nov-file-name (buffer-file-name)))
          :location (passages--epub-get-precise-location)))
   (t (user-error "Not in PDF or EPUB buffer"))))

(defun passages--goto-location (file location type &optional anchor-text)
  "Jump to LOCATION in FILE of TYPE.
Optional ANCHOR-TEXT is used for secondary verification in EPUB."
  (cond
   ((eq type 'pdf)
    (passages--pdf-goto-location file location))
   ((eq type 'epub)
    (passages--epub-goto-location file location anchor-text))
   (t (error "Unknown type: %s" type))))

;;; Text extraction

(defun passages--line-text (line)
  "Get text content of LINE plist."
  (plist-get line :text))

;;; PDF character layout to lines conversion

(defun passages--chars-to-line (chars)
  "Convert a list of CHARS to a line structure.
CHARS is a list of (CHAR (LEFT TOP RIGHT BOTTOM)).
Inserts spaces when there are gaps between characters.
Handles Drop Caps by not inserting space before following lowercase."
  (when chars
    ;; Build text with gap detection
    (let* ((text-parts '())
           (last-right nil)
           (last-char nil)
           (last-height nil)
           (char-index 0)
           ;; Estimate average char width for gap detection
           (widths (mapcar (lambda (c)
                            (let ((e (cadr c)))
                              (- (nth 2 e) (nth 0 e))))
                          chars))
           ;; Use median height to avoid Drop Cap skewing the average
           (heights (sort (mapcar (lambda (c)
                                   (let ((e (cadr c)))
                                     (- (nth 3 e) (nth 1 e))))
                                 chars)
                         #'<))
           (avg-width (/ (apply #'+ widths) (float (length widths))))
           (median-height (nth (/ (length heights) 2) heights))
           ;; Gap threshold: 1.5x average char width
           (gap-threshold (* avg-width 1.5))
           ;; Drop cap threshold: 1.3x median height (lower threshold)
           (drop-cap-height-threshold (* median-height 1.3)))

      (dolist (char-info chars)
        (let* ((char (car char-info))
               (edges (cadr char-info))
               (left (nth 0 edges))
               (right (nth 2 edges))
               (height (- (nth 3 edges) (nth 1 edges)))
               ;; Check if this is Drop Cap continuation:
               ;; - At position 1 (second char)
               ;; - Last char was oversized uppercase
               ;; - Current char is lowercase
               (is-drop-cap-continuation
                (and (= char-index 1)  ; Second character in line
                     last-char last-height
                     (> last-height drop-cap-height-threshold)
                     (and (>= last-char ?A) (<= last-char ?Z))
                     (and (>= char ?a) (<= char ?z)))))
          ;; Check for gap (insert space if needed)
          ;; But NOT for Drop Cap followed by lowercase
          (when (and last-right
                     (> (- left last-right) gap-threshold)
                     (not is-drop-cap-continuation))
            (push ?\s text-parts))
          (push char text-parts)
          (setq last-right right)
          (setq last-char char)
          (setq last-height height)
          (setq char-index (1+ char-index))))

      (let* ((text (apply #'string (nreverse text-parts)))
             (edges (mapcar #'cadr chars))
             (lefts (mapcar (lambda (e) (nth 0 e)) edges))
             (tops (mapcar (lambda (e) (nth 1 e)) edges))
             (rights (mapcar (lambda (e) (nth 2 e)) edges))
             (bottoms (mapcar (lambda (e) (nth 3 e)) edges))
             (min-left (apply #'min lefts))
             (min-top (apply #'min tops))
             (max-right (apply #'max rights))
             (max-bottom (apply #'max bottoms)))
        (list :text text
              :x min-left
              :y min-top
              :width (- max-right min-left)
              :height (- max-bottom min-top))))))

(defun passages--group-chars-into-lines (chars &optional _threshold)
  "Group CHARS into lines by clustering Y-center values.
CHARS is output from `pdf-info-charlayout'.
Uses Y-center clustering to handle mixed font sizes on same visual line.
Handles Drop Caps by using Y-top for oversized characters."
  (when chars
    ;; First pass: collect Y-center and height info
    (let* ((char-data
            (mapcar (lambda (char-info)
                      (let* ((edges (cadr char-info))
                             (y-top (nth 1 edges))
                             (y-bottom (nth 3 edges))
                             (height (- y-bottom y-top))
                             (y-center (/ (+ y-top y-bottom) 2.0)))
                        (list char-info y-center height y-top)))
                    chars))
           ;; Estimate typical line height (median of char heights)
           (heights (sort (mapcar #'cl-third char-data) #'<))
           (median-height (nth (/ (length heights) 2) heights))
           ;; Drop cap threshold: 1.5x median height
           (drop-cap-threshold (* median-height 1.5))
           ;; Bucket tolerance: half of median height
           (bucket-tolerance (/ median-height 2.0))
           ;; Group into buckets by Y-center
           (buckets (make-hash-table :test 'equal)))

      ;; Assign each char to a bucket based on Y-center
      ;; For Drop Caps (oversized chars), use Y-top instead of Y-center
      (dolist (item char-data)
        (let* ((char-info (car item))
               (y-center (cadr item))
               (height (cl-third item))
               (y-top (cl-fourth item))
               ;; Use Y-top for drop caps, Y-center for normal chars
               (y-for-bucket (if (> height drop-cap-threshold)
                                 y-top
                               y-center))
               ;; Round to bucket
               (bucket-key (round (/ y-for-bucket bucket-tolerance))))
          (puthash bucket-key
                   (cons char-info (gethash bucket-key buckets))
                   buckets)))

      ;; Convert buckets to sorted lines
      (let ((bucket-keys (sort (hash-table-keys buckets) #'<))
            (raw-lines '()))
        (dolist (key bucket-keys)
          (let ((bucket-chars (gethash key buckets)))
            ;; Sort chars in bucket by X coordinate
            (push (sort bucket-chars
                        (lambda (a b)
                          (< (nth 0 (cadr a)) (nth 0 (cadr b)))))
                  raw-lines)))
        (setq raw-lines (nreverse raw-lines))

        ;; Filter out empty lines
        (seq-filter
         (lambda (line-chars)
           (let* ((text (apply #'string (mapcar #'car line-chars)))
                  (trimmed (string-trim text)))
             (not (string-empty-p trimmed))))
         raw-lines)))))

(defun passages--filter-line-by-x-contiguous (line-chars x-min x-max)
  "Filter LINE-CHARS to keep only characters contiguous with X selection.
Start with characters in X range (X-MIN to X-MAX), then expand
left and right to include adjacent characters without large gaps.
This handles multi-column (stops at column gap) and single-column
(expands to full line) correctly."
  (when line-chars
    ;; Sort by X position
    (let* ((sorted-chars (sort (copy-sequence line-chars)
                               (lambda (a b)
                                 (< (nth 0 (cadr a)) (nth 0 (cadr b))))))
           ;; Find characters in X selection range (seeds)
           (in-selection
            (seq-filter
             (lambda (c)
               (let ((left (nth 0 (cadr c)))
                     (right (nth 2 (cadr c))))
                 (and (< left x-max) (> right x-min))))
             sorted-chars)))
      (if (null in-selection)
          nil  ; No chars in selection, return empty
        ;; Calculate gap threshold based on average char width
        (let* ((widths (mapcar (lambda (c)
                                 (- (nth 2 (cadr c)) (nth 0 (cadr c))))
                               sorted-chars))
               (avg-width (/ (apply #'+ widths) (float (length widths))))
               (gap-threshold (* avg-width 3.0))  ; 3x char width = column break
               ;; Find leftmost and rightmost of selected chars
               (sel-indices
                (mapcar (lambda (c) (seq-position sorted-chars c #'eq)) in-selection))
               (min-idx (apply #'min sel-indices))
               (max-idx (apply #'max sel-indices))
               (result-start min-idx)
               (result-end max-idx))
          ;; Expand left
          (let ((i (1- min-idx)))
            (while (>= i 0)
              (let* ((curr (nth i sorted-chars))
                     (next (nth (1+ i) sorted-chars))
                     (curr-right (nth 2 (cadr curr)))
                     (next-left (nth 0 (cadr next)))
                     (gap (- next-left curr-right)))
                (if (< gap gap-threshold)
                    (progn
                      (setq result-start i)
                      (setq i (1- i)))
                  (setq i -1)))))  ; Stop expanding
          ;; Expand right
          (let ((i (1+ max-idx))
                (len (length sorted-chars)))
            (while (< i len)
              (let* ((prev (nth (1- i) sorted-chars))
                     (curr (nth i sorted-chars))
                     (prev-right (nth 2 (cadr prev)))
                     (curr-left (nth 0 (cadr curr)))
                     (gap (- curr-left prev-right)))
                (if (< gap gap-threshold)
                    (progn
                      (setq result-end i)
                      (setq i (1+ i)))
                  (setq i len)))))  ; Stop expanding
          ;; Return the contiguous range
          (seq-subseq sorted-chars result-start (1+ result-end)))))))

(defun passages--get-pdf-lines-in-region (page edges)
  "Get lines with position info from PAGE within EDGES.
EDGES is (LEFT TOP RIGHT BOTTOM) in relative coordinates.
Returns a list of line plists.

Strategy:
1. Filter by Y range to get all characters on lines within selection
2. Group into lines by Y-center
3. For each line with chars in X range, keep only contiguous chars
   (handles multi-column by stopping at column gaps)"
  (when (and (fboundp 'pdf-info-charlayout) edges)
    (let* ((edge-left (nth 0 edges))
           (edge-top (nth 1 edges))
           (edge-right (nth 2 edges))
           (edge-bottom (nth 3 edges))
           (y-margin 0.005)
           (x-margin 0.01)
           (x-min (- (min edge-left edge-right) x-margin))
           (x-max (+ (max edge-left edge-right) x-margin))
           (y-min (- (min edge-top edge-bottom) y-margin))
           (y-max (+ (max edge-top edge-bottom) y-margin))
           (all-chars (pdf-info-charlayout page))
           ;; Step 1: Filter by Y range only
           (y-filtered-chars
            (seq-filter
             (lambda (char-info)
               (let* ((char-edges (cadr char-info))
                      (char-top (nth 1 char-edges))
                      (char-bottom (nth 3 char-edges)))
                 (and (< char-top y-max)
                      (> char-bottom y-min))))
             all-chars))
           ;; Step 2: Group into lines by Y-center
           (char-groups (passages--group-chars-into-lines y-filtered-chars))
           ;; Step 3: For each line, filter to contiguous chars around X selection
           (filtered-groups
            (delq nil
                  (mapcar
                   (lambda (line-chars)
                     (passages--filter-line-by-x-contiguous
                      line-chars x-min x-max))
                   char-groups)))
           ;; Step 4: Convert to lines
           (lines (mapcar #'passages--chars-to-line filtered-groups)))
      ;; Filter out empty lines
      (seq-filter (lambda (line)
                    (and line
                         (not (string-empty-p
                               (string-trim (passages--line-text line))))))
                  lines))))

;;; Main text extraction function

(defun passages--pdf-extract-text (page edges)
  "Extract text from PAGE within EDGES and merge into single paragraph.
No paragraph detection - just merge all lines with proper spacing.
Handles Drop Caps and hyphenated words correctly."
  (let ((lines (passages--get-pdf-lines-in-region page edges)))
    (when (and lines (> (length lines) 0))
      (let ((result ""))
        (dolist (line lines)
          ;; Get line text, remove any newlines, then trim whitespace
          (let* ((raw-text (passages--line-text line))
                 (text (string-trim (replace-regexp-in-string "[\n\r]+" " " raw-text))))
            (cond
             ;; First line
             ((string-empty-p result)
              (setq result text))
             ;; Hyphenated word at end: remove hyphen and join directly
             ((string-suffix-p "-" result)
              (setq result (concat (substring result 0 -1) text)))
             ;; Check if we need space
             (t
              (let ((last-char (aref result (1- (length result))))
                    (first-char (and (> (length text) 0) (aref text 0)))
                    ;; Drop Cap: result is 1-2 uppercase letters, text starts lowercase
                    (is-drop-cap-join
                     (and (<= (length result) 2)
                          (string-match-p "^[A-Z]+$" result)
                          (> (length text) 0)
                          (let ((fc (aref text 0)))
                            (and (>= fc ?a) (<= fc ?z))))))
                (cond
                 ;; Drop Cap: join directly without space
                 (is-drop-cap-join
                  (setq result (concat result text)))
                 ;; CJK: no space
                 ((and first-char
                       (or (and (>= last-char #x4e00) (<= last-char #x9fff))
                           (and (>= first-char #x4e00) (<= first-char #x9fff))))
                  (setq result (concat result text)))
                 ;; Non-CJK: add space
                 (t
                  (setq result (concat result " " text)))))))))
        (string-trim result)))))

(defun passages--normalize-text (text)
  "Normalize TEXT extracted from EPUB (simple regex-based).
Used as fallback for PDF when smart detection fails.
- Preserve paragraph boundaries (double+ newlines)
- Handle hyphenated word breaks (word-\\n -> word)
- Merge Chinese characters without adding space
- Replace remaining single newlines with space
- Clean up excessive whitespace"
  (if (or (null text) (string-empty-p text))
      text
    (let ((result text))
      ;; 1. Protect paragraph boundaries: \n\n+ → placeholder
      (setq result (replace-regexp-in-string "\n\n+" "@@@PARA@@@" result))
      ;; 2. Handle hyphenated line breaks: word-\n → word
      (setq result (replace-regexp-in-string "-\n" "" result))
      ;; 3. Merge Chinese characters without space
      (setq result (replace-regexp-in-string "\\(\\cC\\)\n\\(\\cC\\)" "\\1\\2" result))
      ;; 4. Replace remaining newlines with space
      (setq result (replace-regexp-in-string "\n" " " result))
      ;; 5. Restore paragraph boundaries
      (setq result (replace-regexp-in-string "@@@PARA@@@" "\n\n" result))
      ;; 6. Collapse multiple horizontal whitespace
      (setq result (replace-regexp-in-string "[ \t]+" " " result))
      ;; 7. Trim leading/trailing whitespace
      (string-trim result))))

(defun passages--get-selected-text ()
  "Get selected text from PDF/EPUB, normalized.
For PDFs, uses position-based extraction if enabled."
  (pcase major-mode
    ('pdf-view-mode
     (unless (pdf-view-active-region-p)
       (user-error "No text selected in PDF"))
     (or (when-let* ((edges (passages--pdf-extract-edges (pdf-view-active-region)))
                     (_ passages-smart-text-extraction))
           (passages--pdf-extract-text (pdf-view-current-page) edges))
         (passages--normalize-text (car (pdf-view-active-region-text)))))

    ('nov-mode
     (unless (use-region-p)
       (user-error "No text selected in EPUB"))
     (passages--normalize-text
      (buffer-substring-no-properties (region-beginning) (region-end))))

    (_ (user-error "Not in PDF or EPUB buffer"))))

(defun passages--format-location (location)
  "Format LOCATION for storage."
  (let ((page (passages--get-location-page location))
        (v-pos (passages--get-location-top location))
        (h-pos (passages--get-location-left location)))
    (cond
     ((and h-pos (> h-pos 0))
      (format "(%d %.2f . %.2f)" page v-pos h-pos))
     ((and (numberp v-pos) (> v-pos 0))
      (if (and (or (integerp v-pos)
                   (= v-pos (floor v-pos)))
               (> v-pos 10))
          (format "(%d . %d)" page (floor v-pos))
        (format "(%d . %.2f)" page v-pos)))
     (t (format "%d" page)))))

;;; Find excerpts with org-mode separator

(defun passages--find-excerpts-in-region (start end)
  "Find excerpts in region from START to END.
Format: ● Pxx [content - may span multiple lines] ⟦file|location⟧
Content between marker and ⟦...⟧ is the excerpt body."
  (let (excerpts)
    (save-excursion
      (goto-char start)
      ;; Find each ● Pxx marker, then locate its ⟦...⟧ ending
      (while (re-search-forward "^[ \t]*\\(● P\\([0-9]+\\) \\)" end t)
        (let* ((marker-start (match-beginning 1))
               (marker-end (match-end 1))
               (page (string-to-number (match-string 2)))
               (body-start (point))
               ;; Limit search: don't cross into next excerpt
               (search-limit (save-excursion
                               (if (re-search-forward "^[ \t]*● P[0-9]+ " end t)
                                   (line-beginning-position)
                                 end))))
          ;; Find the closing ⟦file|location⟧ within limit
          (when (re-search-forward " ?⟦\\([^|]+\\)|\\([^⟧]+\\)⟧" search-limit t)
            (let* ((file (match-string 1))
                   (location-str (match-string 2))
                   (meta-start (match-beginning 0))
                   (meta-end (match-end 0))
                   ;; Body ends where metadata starts (minus optional space)
                   (body-end (if (eq (char-before meta-start) ?\s)
                                 (1- meta-start)
                               meta-start)))
              (when passages-debug
                (message "Found excerpt: page=%d file=%s loc=%s" page file location-str))
              (push (list :marker-start marker-start
                          :marker-end marker-end
                          :page page
                          :file file
                          :location location-str
                          :body-start body-start
                          :body-end body-end
                          :meta-start meta-start
                          :meta-end meta-end)
                    excerpts))))))
    (nreverse excerpts)))

(defun passages--find-all-excerpts ()
  "Find all excerpts in buffer."
  (passages--find-excerpts-in-region (point-min) (point-max)))

(defun passages--find-excerpts-for-page (page file)
  "Find all excerpts for PAGE in FILE in current buffer.
Supports multi-line excerpts."
  (let ((positions '()))
    (save-excursion
      (goto-char (point-min))
      ;; Find ● Pxx marker, then locate its ⟦file|...⟧
      (while (re-search-forward (format "^[ \t]*● P%d " page) nil t)
        (let* ((marker-pos (line-beginning-position))
               ;; Don't search past next excerpt marker
               (search-limit (save-excursion
                               (if (re-search-forward "^[ \t]*● P[0-9]+ " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
          ;; Find the closing ⟦file|location⟧ within limit
          (when (re-search-forward "⟦\\([^|]+\\)|[^⟧]+⟧" search-limit t)
            (let ((excerpt-file (match-string 1)))
              (when (or (null file)
                        (null excerpt-file)
                        (string-match-p (regexp-quote (file-name-nondirectory file))
                                        excerpt-file))
                (push marker-pos positions)))))))
    (nreverse positions)))

;;; Optimized overlay system with org separator

(defun passages--jit-lock-function (start end)
  "JIT lock function to add overlays incrementally."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (goto-char end)
      (end-of-line)
      (setq end (point))

      (remove-overlays start end 'passages-overlay t)

      (let ((excerpts (passages--find-excerpts-in-region start end)))
        (dolist (excerpt excerpts)
          (passages--add-overlay-for-excerpt excerpt)))

      ;; Style org-mode separators (5+ dashes)
      (save-excursion
        (goto-char start)
        (while (re-search-forward "^[ \t]*-----+[ \t]*$" end t)
          (let ((sep-ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put sep-ov 'face 'passages-separator-face)
            (overlay-put sep-ov 'passages-overlay t)
            (overlay-put sep-ov 'evaporate t)))))))

(defun passages--add-overlay-for-excerpt (excerpt)
  "Add overlay for a single EXCERPT.
Format: ● Pxx [content - may span multiple lines] ⟦file|location⟧
Content between marker and ⟦...⟧ is styled gray, metadata is hidden."
  (let* ((marker-start (plist-get excerpt :marker-start))
         (marker-end (plist-get excerpt :marker-end))
         (body-start (plist-get excerpt :body-start))
         (body-end (plist-get excerpt :body-end))
         (meta-start (plist-get excerpt :meta-start))
         (meta-end (plist-get excerpt :meta-end))
         (page (plist-get excerpt :page))
         (file (plist-get excerpt :file)))

    ;; Style the marker part (● Pxx)
    (let ((marker-ov (make-overlay marker-start marker-end)))
      (overlay-put marker-ov 'face 'passages-marker-face)
      (overlay-put marker-ov 'mouse-face 'highlight)
      (overlay-put marker-ov 'help-echo "Click or C-c e j to jump to source")
      (overlay-put marker-ov 'keymap
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1]
                       (lambda (event)
                         (interactive "e")
                         (passages-jump-to-source)))
                     map))
      (overlay-put marker-ov 'passages-overlay t)
      (overlay-put marker-ov 'passages-file file)
      (overlay-put marker-ov 'evaporate t))

    ;; Style the content part (gray)
    (when (and body-start body-end (< body-start body-end))
      (let ((body-ov (make-overlay body-start body-end)))
        (overlay-put body-ov 'face 'passages-content-face)
        (overlay-put body-ov 'passages-overlay t)
        (overlay-put body-ov 'passages-file file)
        (overlay-put body-ov 'evaporate t)
        (overlay-put body-ov 'help-echo
                     (format "Passage from page %d" page))))

    ;; Hide the metadata part ⟦file|location⟧
    (when (and meta-start meta-end)
      (let ((meta-ov (make-overlay (1- meta-start) meta-end)))
        (overlay-put meta-ov 'invisible t)
        (overlay-put meta-ov 'passages-overlay t)
        (overlay-put meta-ov 'evaporate t)))))

(defun passages--add-overlays ()
  "Add overlays to all excerpts."
  (interactive)
  (passages--remove-overlays)
  (let ((excerpts (passages--find-all-excerpts))
        (count 0))

    (dolist (excerpt excerpts)
      (passages--add-overlay-for-excerpt excerpt)
      (setq count (1+ count)))

    ;; Style org-mode separators (5+ dashes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*-----+[ \t]*$" nil t)
        (let ((sep-ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put sep-ov 'face 'passages-separator-face)
          (overlay-put sep-ov 'passages-overlay t)
          (overlay-put sep-ov 'evaporate t))))

    (when passages-debug
      (message "Added %d styled passage cards" count))
    count))

(defun passages--remove-overlays ()
  "Remove all excerpt overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'passages-overlay t))

(defun passages--refresh-overlays ()
  "Refresh all overlays."
  (interactive)
  (when (and (buffer-live-p (current-buffer))
             (derived-mode-p 'org-mode))
    (passages--remove-overlays)
    (passages--add-overlays)))

(defun passages--ensure-overlays-at-point ()
  "Ensure overlays are rendered around point."
  (when (and (fboundp 'jit-lock-fontify-now)
             (get-buffer-window))
    (let* ((win (get-buffer-window))
           (start (window-start win))
           (end (window-end win)))
      (save-excursion
        (goto-char start)
        (beginning-of-line)
        (setq start (point))
        (goto-char end)
        (end-of-line)
        (setq end (point)))
      (jit-lock-fontify-now start end))))

;;; Note file management

(defun passages--get-note-file-path (source-file)
  "Determine the path for note file of SOURCE-FILE."
  (let ((normalized-source (passages--normalize-path source-file)))
    (or
     (passages--find-citar-note normalized-source)

     (when (passages--denote-available-p)
       (or (passages--find-denote-note normalized-source)
           (passages--create-denote-note normalized-source)))

     (when passages-directory
       (unless (file-directory-p passages-directory)
         (make-directory passages-directory t))
       (passages--normalize-path
        (expand-file-name
         (concat (file-name-base normalized-source) ".notes.org")
         passages-directory)))

     (passages--normalize-path
      (concat normalized-source ".notes.org")))))

(defun passages--ensure-file-header (buffer source-file)
  "Ensure BUFFER has proper header."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "^#\\+TITLE:")
        (insert "#+TITLE: Notes for " (file-name-nondirectory source-file) "\n")
        (insert "#+SOURCE_FILE: " source-file "\n\n")))))

(defun passages--get-note-buffer (source-file)
  "Get or create note buffer for SOURCE-FILE."
  (let* ((note-file (passages--get-note-file-path source-file))
         (buf (or (find-buffer-visiting note-file)
                  (find-file-noselect note-file))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless (and (passages--denote-available-p)
                   (string-match-p (regexp-quote (expand-file-name denote-directory))
                                  (expand-file-name note-file)))
        (passages--ensure-file-header buf source-file))
      (when passages-auto-parse
        (passages-mode 1)))
    buf))

;;; Ordered insertion support

(defun passages--compare-locations-pdf (loc-a loc-b)
  "Compare two PDF locations LOC-A and LOC-B.
Returns negative if A comes before B, zero if same position,
positive if A comes after B.  Compares page first, then vertical
position with a tolerance of 0.001."
  (let ((page-a (passages--get-location-page loc-a))
        (page-b (passages--get-location-page loc-b))
        (vpos-a (float (passages--get-location-top loc-a)))
        (vpos-b (float (passages--get-location-top loc-b))))
    (cond
     ((/= page-a page-b) (- page-a page-b))
     ((< (abs (- vpos-a vpos-b)) 0.001) 0)
     (t (if (< vpos-a vpos-b) -1 1)))))

(defun passages--extract-anchor-from-excerpt (excerpt)
  "Extract first 30 characters of body text from EXCERPT plist.
EXCERPT is a plist as returned by `passages--find-excerpts-in-region'."
  (let* ((body-start (plist-get excerpt :body-start))
         (body-end (plist-get excerpt :body-end))
         (raw (buffer-substring-no-properties
               body-start (min (+ body-start 60) body-end)))
         (body-text (string-trim raw)))
    (when (> (length body-text) 0)
      (substring body-text 0 (min 30 (length body-text))))))

(defun passages--insertion-pos-before-excerpt (excerpt)
  "Find the start of the note area before EXCERPT.
Searches backward from the marker for the nearest boundary
\(separator, heading, or header keyword) and returns the position
just after that boundary and any trailing blank lines."
  (let ((marker-start (plist-get excerpt :marker-start)))
    (save-excursion
      (goto-char marker-start)
      (if (re-search-backward
           "^\\([ \t]*-----+[ \t]*$\\|\\*+ \\|#\\+\\)" nil t)
          (progn
            (forward-line 1)
            ;; Skip blank lines after boundary
            (while (and (< (point) marker-start)
                        (looking-at "^[ \t]*$"))
              (forward-line 1))
            (point))
        marker-start))))

(defun passages--find-epub-buffer (file-name)
  "Find an open EPUB buffer for FILE-NAME."
  (catch 'found-buf
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'nov-mode)
                   (boundp 'nov-file-name)
                   nov-file-name
                   (string= (file-name-nondirectory nov-file-name) file-name))
          (throw 'found-buf buf))))
    nil))

(defun passages--compare-locations-epub (new-loc exc-loc new-anchor exc epub-buffer)
  "Compare EPUB locations NEW-LOC and EXC-LOC.
NEW-ANCHOR is the anchor text for new excerpt, EXC is the existing excerpt plist.
EPUB-BUFFER is the buffer to search for anchor text comparison."
  (let ((new-ch (passages--get-location-page new-loc))
        (exc-ch (passages--get-location-page exc-loc)))
    (if (/= new-ch exc-ch)
        (- new-ch exc-ch)
      ;; Same chapter — compare by stored buffer position
      (let ((new-pos (passages--get-location-top new-loc))
            (exc-pos (passages--get-location-top exc-loc)))
        (cond
         ((and (numberp new-pos) (numberp exc-pos))
          (- new-pos exc-pos))
         ;; Fall back to anchor text comparison
         ((and epub-buffer
               (buffer-live-p epub-buffer)
               (with-current-buffer epub-buffer
                 (and (eq major-mode 'nov-mode)
                      (boundp 'nov-documents-index)
                      (= nov-documents-index new-ch))))
          (with-current-buffer epub-buffer
            (let ((exc-anchor (passages--extract-anchor-from-excerpt exc))
                  (pos-a (and new-anchor
                              (save-excursion
                                (goto-char (point-min))
                                (when (search-forward new-anchor nil t)
                                  (match-beginning 0)))))
                  (pos-b nil))
              (when exc-anchor
                (setq pos-b (save-excursion
                              (goto-char (point-min))
                              (when (search-forward exc-anchor nil t)
                                (match-beginning 0)))))
              (if (and pos-a pos-b) (- pos-a pos-b) 0))))
         (t 0))))))

(defun passages--find-insertion-point (loc-info new-text)
  "Find the buffer position where a new excerpt should be inserted.
LOC-INFO is the plist (:type :file :location) for the new excerpt.
NEW-TEXT is the excerpt body text (used for EPUB anchor comparison).
Returns a buffer position; falls back to `point-max' when no
existing excerpt sorts after the new one."
  (let* ((new-type (plist-get loc-info :type))
         (new-file (plist-get loc-info :file))
         (new-location (plist-get loc-info :location))
         (new-file-name (file-name-nondirectory new-file))
         (excerpts (passages--find-all-excerpts))
         (same-file-excerpts
          (seq-filter
           (lambda (exc)
             (string= (file-name-nondirectory (plist-get exc :file))
                      new-file-name))
           excerpts)))
    (if (null same-file-excerpts)
        (point-max)
      (let* ((new-anchor (when (and new-text (> (length new-text) 0))
                           (substring new-text 0 (min 30 (length new-text)))))
             (epub-buffer (when (eq new-type 'epub)
                            (passages--find-epub-buffer new-file-name)))
             (insertion-excerpt
              (catch 'found
                (dolist (exc same-file-excerpts)
                  (let* ((exc-loc-str (plist-get exc :location))
                         (exc-location (passages--parse-location-string exc-loc-str))
                         (cmp (cond
                               ((eq new-type 'pdf)
                                (passages--compare-locations-pdf new-location exc-location))
                               ((eq new-type 'epub)
                                (passages--compare-locations-epub
                                 new-location exc-location new-anchor exc epub-buffer))
                               (t 0))))
                    (when (< cmp 0)
                      (throw 'found exc))))
                nil)))
        (if insertion-excerpt
            (passages--insertion-pos-before-excerpt insertion-excerpt)
          (point-max))))))

;;; Duplicate detection

(defcustom passages-duplicate-anchor-length 40
  "Number of characters to use as anchor for duplicate detection."
  :type 'integer
  :group 'passages)

(defun passages--find-duplicate (text note-buffer)
  "Check if TEXT already exists in NOTE-BUFFER.
Returns (PAGE . PREVIEW) if duplicate found, nil otherwise."
  (when (and text (> (length text) 10))
    (let* ((anchor (substring (string-trim text)
                              0 (min passages-duplicate-anchor-length
                                     (length (string-trim text))))))
      (with-current-buffer note-buffer
        (save-excursion
          (goto-char (point-min))
          (when-let* ((_ (search-forward anchor nil t))
                      (_ (re-search-backward "^[ \t]*● P\\([0-9]+\\)" nil t))
                      (page (string-to-number (match-string 1)))
                      (preview-start (save-excursion (forward-line 1) (point)))
                      (preview-end (min (+ preview-start 80)
                                        (save-excursion
                                          (goto-char preview-start)
                                          (line-end-position))))
                      (preview (string-trim
                                (buffer-substring-no-properties
                                 preview-start preview-end))))
            (cons page preview)))))))

;;; Excerpt insertion with org separator

(defun passages--do-insert (text loc-info)
  "Internal function to insert excerpt with TEXT and LOC-INFO.
Structure:
  ● Pxx [excerpt content] ⟦file|location⟧
  [user notes below]
The ⟦...⟧ metadata is hidden by overlay but persists in the file."
  (let* ((location (plist-get loc-info :location))
         (file (plist-get loc-info :file))
         (type (plist-get loc-info :type))
         (page (passages--get-location-page location))
         (formatted-location (passages--format-location location))
         (source-window (selected-window))
         (source-buffer (current-buffer))
         (note-buffer (passages--get-note-buffer file))
         note-position)

    ;; Restore source buffer to source window in case it was changed
    (when (and (window-live-p source-window)
               (not (eq (window-buffer source-window) source-buffer)))
      (set-window-buffer source-window source-buffer))

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))
    (goto-char (passages--find-insertion-point loc-info text))

    (unless (looking-back "\n\n" 2)
      (if (looking-back "\n" 1)
          (insert "\n")
        (insert "\n\n")))

    ;; 1. Marker: ● Pxx
    (let ((marker-start (point)))
      (insert (format "● P%d " page)))

    ;; 2. Excerpt content
    (when text
      (insert text))

    ;; 3. Metadata suffix: ⟦file|location⟧ (will be hidden by overlay)
    (insert (format " ⟦%s|%s⟧\n" file formatted-location))

    ;; 4. User notes area (empty line below for user to fill)
    (setq note-position (point))
    (insert "\n")

    ;; Position cursor at notes area for immediate editing
    (goto-char note-position)

    (when passages-mode
      (run-with-idle-timer 0.1 nil 'passages--refresh-overlays))

    (if text
        (message "✓ Passage from p.%d inserted. Add notes below." page)
      (message "✓ Empty anchor at p.%d created. Add notes below." page))))

;;;###autoload
(defun passages-insert ()
  "Insert excerpt, checking for duplicates first."
  (interactive)
  (let* ((has-selection (cond
                         ((eq major-mode 'pdf-view-mode)
                          (pdf-view-active-region-p))
                         ((eq major-mode 'nov-mode)
                          (use-region-p))
                         (t nil)))
         (text (when has-selection
                 (passages--get-selected-text)))
         (loc-info (passages--get-current-location))
         (file (plist-get loc-info :file))
         (note-buffer (passages--get-note-buffer file)))

    (cond
     (has-selection
      ;; Check for duplicate
      (let ((duplicate (passages--find-duplicate text note-buffer)))
        (if duplicate
            (if (y-or-n-p
                 (format "Similar passage exists at p.%d: \"%.40s...\" Insert anyway? "
                         (car duplicate)
                         (cdr duplicate)))
                (passages--do-insert text loc-info)
              (message "Insertion cancelled"))
          (passages--do-insert text loc-info))))
     ((y-or-n-p "No text selected. Create an empty anchor for this page? ")
      (passages--do-insert nil loc-info))
     (t
      (message "Action cancelled")))))

;;; Find notes for current location

(defun passages--format-excerpt-choice (pos page)
  "Format excerpt at POS as a (LABEL . POS) cons for `completing-read'.
PAGE is the page number used in the label."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (let* ((excerpt-start (point))
           (excerpt-end (save-excursion
                          (if (re-search-forward
                               "^[ \t]*\\(-----+\\|● P\\)" nil t)
                              (line-beginning-position)
                            (point-max))))
           (preview (truncate-string-to-width
                     (string-trim
                      (buffer-substring-no-properties
                       excerpt-start
                       (min (+ excerpt-start 80) excerpt-end)))
                     60 nil nil "...")))
      (cons (format "p.%d: %s" page preview) pos))))

;;;###autoload
(defun passages-find-this-location-in-notes ()
  "Find and jump to notes for current location in PDF/EPUB."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))

  (let* ((loc-info (passages--get-current-location))
         (file (plist-get loc-info :file))
         (location (plist-get loc-info :location))
         (page (passages--get-location-page location))
         (note-buffer (passages--get-note-buffer file)))

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))

    (unless passages-mode
      (passages-mode 1))

    (let ((positions (passages--find-excerpts-for-page page file)))
      (cond
       ((null positions)
        (message "No passages found for page %d. Press 'e' to create one" page))

       ((= (length positions) 1)
        (goto-char (car positions))
        (passages--ensure-overlays-at-point)
        (recenter)
        (message "✓ Found passage for page %d" page))

       (t
        (let* ((choices (mapcar (lambda (pos)
                                 (passages--format-excerpt-choice pos page))
                               positions))
               (choice (completing-read
                       (format "Found %d passages for page %d. Choose: "
                               (length positions) page)
                       choices nil t)))
          (goto-char (cdr (assoc choice choices)))
          (passages--ensure-overlays-at-point)
          (recenter)
          (message "✓ Jumped to passage")))))))

;;;###autoload
(defun passages-find-region-in-notes ()
  "Find notes containing the selected text."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))

  (let* ((text (passages--get-selected-text))
         (loc-info (passages--get-current-location))
         (file (plist-get loc-info :file))
         (note-buffer (passages--get-note-buffer file))
         (search-text (substring text 0 (min 40 (length text)))))

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))

    (unless passages-mode
      (passages-mode 1))

    (goto-char (point-min))
    (if (search-forward search-text nil t)
        (progn
          (re-search-backward "^[ \t]*● P" nil t)
          (passages--ensure-overlays-at-point)
          (recenter)
          (message "✓ Found passage containing selected text"))
      (message "Selected text not found in notes. Press 'e' to create passage"))))

;;; Jump to source

(defun passages--find-marker-for-point ()
  "Find the ● P marker position for the excerpt unit containing point.
Returns the position of the marker line, or nil if not found."
  (save-excursion
    (let ((orig (point)))
      (beginning-of-line)
      (cond
       ;; Already on marker line
       ((looking-at "^[ \t]*● P")
        (point))
       ;; Search backward for ● P or -----
       ((re-search-backward "^[ \t]*\\(● P\\|-----+[ \t]*$\\)" nil t)
        (beginning-of-line)
        (if (looking-at "^[ \t]*● P")
            (point)
          ;; Hit separator - cursor is in notes area, search forward
          (goto-char orig)
          (when (re-search-forward "^[ \t]*● P" nil t)
            (beginning-of-line)
            (point))))
       ;; Nothing found backward, try forward (at beginning of buffer)
       (t
        (goto-char orig)
        (when (re-search-forward "^[ \t]*● P" nil t)
          (beginning-of-line)
          (point)))))))

(defun passages--parse-marker-line-at-point ()
  "Parse excerpt at or near point.
Format: ● Pxx [content - may span lines] ⟦file|location⟧"
  (save-excursion
    (when-let* ((marker-pos (passages--find-marker-for-point)))
      (goto-char marker-pos)
      ;; Match the marker and get page number
      (when (looking-at "^[ \t]*● P\\([0-9]+\\) ")
        (let* ((page (string-to-number (match-string 1)))
               ;; Don't search past next excerpt marker
               (search-limit (save-excursion
                               (forward-line 1)
                               (if (re-search-forward "^[ \t]*● P[0-9]+ " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
          ;; Search forward for ⟦file|location⟧ within limit
          (when (re-search-forward "⟦\\([^|]+\\)|\\([^⟧]+\\)⟧" search-limit t)
            (let* ((file (match-string 1))
                   (location-str (match-string 2))
                   (type (if (and file (string-match-p "\\.epub$" file)) 'epub 'pdf)))
              (list :page page
                    :file file
                    :location location-str
                    :type type))))))))

(defun passages--find-file-property-nearby (pos)
  "Find passages-file text property at or near POS on the same line.
Returns (FILE . FOUND-POS) or nil."
  (or
   ;; Direct hit at pos
   (when-let* ((file (get-text-property pos 'passages-file)))
     (cons file pos))
   ;; Previous property boundary on the same line
   (save-excursion
     (goto-char pos)
     (when-let* ((prev-pos (previous-single-property-change
                            pos 'passages-file nil (line-beginning-position)))
                 (file (get-text-property prev-pos 'passages-file)))
       (cons file prev-pos)))
   ;; Next property boundary on the same line
   (save-excursion
     (goto-char pos)
     (when-let* ((next-pos (next-single-property-change
                            pos 'passages-file nil (line-end-position)))
                 (file (get-text-property next-pos 'passages-file)))
       (cons file next-pos)))
   ;; Overlays
   (let ((result nil))
     (dolist (ov (overlays-at pos) result)
       (when-let* ((file (overlay-get ov 'passages-file)))
         (setq result (cons file pos)))))))

(defun passages--get-excerpt-properties-at-point (&optional pos)
  "Get excerpt properties at POS."
  (let* ((pos (or pos (point)))
         (found (passages--find-file-property-nearby pos))
         (props (when-let* ((file (car found))
                            (fpos (cdr found)))
                  (list :file file
                        :location (get-text-property fpos 'passages-location)
                        :page (get-text-property fpos 'passages-page)
                        :type (get-text-property fpos 'passages-type)))))
    (if (and props (plist-get props :page))
        props
      (passages--parse-marker-line-at-point))))

(defun passages--extract-anchor-text-at-point ()
  "Extract the first 30 characters of the excerpt body at point.
Used as anchor text for EPUB secondary location verification.
Content may span multiple lines, from ● Pxx to ⟦file|location⟧."
  (save-excursion
    (when-let* ((marker-pos (passages--find-marker-for-point)))
      (goto-char marker-pos)
      (when (looking-at "^[ \t]*● P[0-9]+ ")
        (goto-char (match-end 0))
        (let* ((body-start (point))
               ;; Don't search past next excerpt marker
               (search-limit (save-excursion
                               (if (re-search-forward "^[ \t]*● P[0-9]+ " nil t)
                                   (line-beginning-position)
                                 (point-max)))))
          (when (re-search-forward " ?⟦[^⟧]+⟧" search-limit t)
            (let* ((body-end (match-beginning 0))
                   (body-text (string-trim
                               (buffer-substring-no-properties body-start body-end))))
              (when (> (length body-text) 0)
                (substring body-text 0 (min 30 (length body-text)))))))))))

(defun passages--parse-location-string (location-str)
  "Parse LOCATION-STR into a Lisp value (number or cons cell).
Returns the parsed value, or LOCATION-STR unchanged on failure."
  (condition-case nil
      (let ((parsed (car (read-from-string location-str))))
        (if (or (numberp parsed) (consp parsed))
            parsed
          location-str))
    (error location-str)))

(defun passages-jump-to-source ()
  "Jump to source location of excerpt at point."
  (interactive)
  (when (fboundp 'jit-lock-fontify-now)
    (jit-lock-fontify-now (line-beginning-position) (line-end-position)))
  (let ((props (passages--get-excerpt-properties-at-point)))
    (unless props
      (user-error "Not in a passage. Move cursor to gray text or marker line"))
    (let* ((file (or (plist-get props :file)
                     (user-error "Could not determine source file")))
           (page (or (plist-get props :page)
                     (user-error "Could not determine page number")))
           (location-str (or (plist-get props :location)
                             (user-error "Could not determine location")))
           (type (or (plist-get props :type)
                     (if (string-match-p "\\.epub$" file) 'epub 'pdf)))
           (location (passages--parse-location-string location-str))
           (anchor-text (passages--extract-anchor-text-at-point)))
      (when passages-debug
        (message "Jumping to: %s" file)
        (message "  Page: %d" page)
        (message "  Type: %s" type)
        (message "  Location: %s" location-str)
        (when anchor-text
          (message "  Anchor: %s" anchor-text)))
      (let ((resolved-file (passages--resolve-path file)))
        (unless (file-exists-p resolved-file)
          (user-error "Source file not found: %s" resolved-file)))
      (condition-case err
          (progn
            (passages--goto-location file location type anchor-text)
            (message "✓ Jumped to %s p.%d" (file-name-nondirectory file) page))
        (error
         (message "Error jumping to source: %s" (error-message-string err))
         (user-error "Failed to jump to source"))))))

;;; Excerpt management

(defun passages--excerpt-region-at-point ()
  "Return (BEG END MARKER-POS) of the full excerpt unit at point, or nil.
An excerpt unit is: ● Pxx [content] + [user notes below]."
  (save-excursion
    (let ((orig (point))
          marker-pos)
      ;; Step 1: Find the ● P marker that owns point.
      ;; Search backward for marker or separator.
      (beginning-of-line)
      (cond
       ((looking-at "^[ \t]*● P")
        (setq marker-pos (point)))
       ((re-search-backward "^[ \t]*\\(● P\\|-----+\\)" nil t)
        (beginning-of-line)
        (if (looking-at "^[ \t]*● P")
            (setq marker-pos (point))
          (goto-char orig)
          (when (re-search-forward "^[ \t]*● P" nil t)
            (beginning-of-line)
            (setq marker-pos (point)))))
       (t
        (goto-char orig)
        (when (re-search-forward "^[ \t]*● P" nil t)
          (beginning-of-line)
          (setq marker-pos (point)))))

      ;; Step 2: Compute deletion region from marker.
      (when marker-pos
        (goto-char marker-pos)
        (let* ((region-start
                (passages--insertion-pos-before-excerpt
                 (list :marker-start marker-pos)))
               (region-end
                (save-excursion
                  (goto-char marker-pos)
                  (forward-line 1)
                  (if (re-search-forward "^[ \t]*-----+[ \t]*$" nil t)
                      (progn
                        (goto-char (match-end 0))
                        (while (and (not (eobp)) (looking-at "^[ \t]*$"))
                          (forward-line 1))
                        (point))
                    (point-max)))))
          (list region-start region-end marker-pos))))))

;;;###autoload
(defun passages-delete-at-point ()
  "Delete the excerpt at point, including user notes, marker, body, and separator."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in org-mode buffer"))
  (let ((region (passages--excerpt-region-at-point)))
    (unless region
      (user-error "No passage found at point"))
    (let* ((region-start (nth 0 region))
           (region-end (nth 1 region))
           (marker-pos (nth 2 region))
           (preview (save-excursion
                      (goto-char marker-pos)
                      (forward-line 1)
                      (string-trim
                       (buffer-substring-no-properties
                        (point)
                        (min (+ (point) 60) (line-end-position)))))))
      (when (y-or-n-p (format "Delete passage: \"%s...\"? " preview))
        ;; Expand region-start to include blank lines before notes area
        (save-excursion
          (goto-char region-start)
          (while (and (> (point) (point-min))
                      (save-excursion
                        (forward-line -1)
                        (looking-at "^[ \t]*$")))
            (forward-line -1)
            (setq region-start (point))))
        (remove-overlays region-start region-end 'passages-overlay t)
        (let ((inhibit-read-only t))
          (delete-region region-start region-end))
        ;; Clean up multiple consecutive blank lines at deletion point
        (let ((inhibit-read-only t))
          (while (and (not (eobp))
                      (looking-at "^[ \t]*$")
                      (save-excursion
                        (forward-line -1)
                        (looking-at "^[ \t]*$")))
            (delete-region (point) (progn (forward-line 1) (point)))))
        (passages--refresh-overlays)
        (message "Passage deleted")))))

;;;###autoload
(defun passages-list-excerpts ()
  "List all excerpts in the current buffer and jump to the selected one."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in org-mode buffer"))
  (let ((excerpts (passages--find-all-excerpts)))
    (unless excerpts
      (user-error "No passages found in this buffer"))
    (let* ((choices (mapcar (lambda (excerpt)
                              (passages--format-excerpt-choice
                               (plist-get excerpt :marker-start)
                               (plist-get excerpt :page)))
                            excerpts))
           (choice (completing-read "Jump to passage: " choices nil t)))
      (when-let* ((pos (cdr (assoc choice choices))))
        (goto-char pos)
        (passages--ensure-overlays-at-point)
        (recenter)
        (message "Jumped to passage")))))

;;; Path fix utility

;; Note: passages-fix-file-paths, passages-update-source-file, and
;; passages--try-find-path-for-file are removed with the new compact format
;; are deprecated with the new compact format (file info in text properties)

;;; Debug utilities

;;;###autoload
(defun passages-show-selected-text ()
  "Show what text will be extracted (for debugging)."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (if (pdf-view-active-region-p)
        (let ((text (car (pdf-view-active-region-text))))
          (with-current-buffer (get-buffer-create "*Passages Preview*")
            (erase-buffer)
            (insert "=== TEXT TO BE INSERTED ===\n\n")
            (insert text)
            (insert "\n\n=== END ===\n")
            (insert (format "\nLength: %d characters\n" (length text)))
            (insert (format "Lines: %d\n" (length (split-string text "\n"))))
            (goto-char (point-min))
            (pop-to-buffer (current-buffer))))
      (message "No region selected"))))

;;;###autoload
(defun passages-preview-extraction ()
  "Preview text extraction results for current PDF selection."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "Only works in PDF buffers"))
  (unless (pdf-view-active-region-p)
    (user-error "No region selected"))

  (let* ((page (pdf-view-current-page))
         (edges (passages--pdf-extract-edges (pdf-view-active-region)))
         (lines (passages--get-pdf-lines-in-region page edges))
         (extracted-text (when lines (passages--pdf-extract-text page edges))))

    (with-current-buffer (get-buffer-create "*Extraction Preview*")
      (erase-buffer)
      (insert (format "Page %d | %d lines\n\n" page (length lines)))

      ;; Show lines
      (when lines
        (dolist (line lines)
          (insert (format "  %s\n"
                          (truncate-string-to-width
                           (passages--line-text line) 70 nil nil "...")))))

      (insert "\n--- RESULT ---\n")
      (insert (or extracted-text "[extraction failed]"))
      (insert "\n")

      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun passages-debug-char-positions ()
  "Debug character positions to see Y coordinate differences."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "Only works in PDF buffers"))
  (unless (pdf-view-active-region-p)
    (user-error "No region selected"))

  (let* ((page (pdf-view-current-page))
         (edges (passages--pdf-extract-edges (pdf-view-active-region)))
         (edge-top (nth 1 edges))
         (edge-bottom (nth 3 edges))
         (y-margin 0.005)
         (y-min (- (min edge-top edge-bottom) y-margin))
         (y-max (+ (max edge-top edge-bottom) y-margin))
         (all-chars (pdf-info-charlayout page))
         (filtered-chars
          (seq-filter
           (lambda (char-info)
             (let* ((char-edges (cadr char-info))
                    (char-top (nth 1 char-edges))
                    (char-bottom (nth 3 char-edges)))
               (and (< char-top y-max)
                    (> char-bottom y-min))))
           all-chars)))

    (with-current-buffer (get-buffer-create "*Char Positions Debug*")
      (erase-buffer)
      (insert (format "Page %d | %d characters in region\n\n" page (length filtered-chars)))
      (insert "Char | Y-top    | Y-bottom | X-left   | Text so far...\n")
      (insert "-----+----------+----------+----------+---------------\n")

      (let ((last-y nil)
            (current-text ""))
        (dolist (char-info filtered-chars)
          (let* ((char (car char-info))
                 (char-edges (cadr char-info))
                 (y-top (nth 1 char-edges))
                 (y-bottom (nth 3 char-edges))
                 (x-left (nth 0 char-edges))
                 (y-diff (when last-y (- y-top last-y))))
            ;; Mark line breaks
            (when (and last-y (> (abs (- y-top last-y)) 0.008))
              (insert (format "---- Y jump: %.5f (threshold: 0.008) ----\n" y-diff))
              (insert (format "     Line so far: %s\n\n" (string-trim current-text)))
              (setq current-text ""))
            (setq current-text (concat current-text (string char)))
            (setq last-y y-top))))

      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun passages-debug-region ()
  "Debug: show raw region data and computed edges."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "Only works in PDF buffers"))
  (unless (pdf-view-active-region-p)
    (user-error "No region selected"))
  (let* ((raw-region (pdf-view-active-region))
         (computed-edges (passages--pdf-extract-edges raw-region)))
    (message "Raw region: %S\nComputed edges: %S" raw-region computed-edges)))

;;;###autoload
(defun passages-debug-at-point ()
  "Debug excerpt detection at current position."
  (interactive)
  (let ((excerpts (passages--find-all-excerpts)))
    (if excerpts
        (progn
          (message "Found %d passages" (length excerpts))
          (with-current-buffer (get-buffer-create "*Passages Debug*")
            (erase-buffer)
            (insert (format "Total found: %d\n\n" (length excerpts)))
            (dolist (excerpt excerpts)
              (insert (format "● P%d: %s\n"
                              (plist-get excerpt :page)
                              (truncate-string-to-width
                               (buffer-substring-no-properties
                                (plist-get excerpt :body-start)
                                (min (+ (plist-get excerpt :body-start) 50)
                                     (plist-get excerpt :body-end)))
                               50 nil nil "..."))))
            (goto-char (point-min))
            (pop-to-buffer (current-buffer))))
      (message "No passages found!"))))

;;;###autoload
(defun passages-force-refresh ()
  "Force refresh all overlays with detailed logging."
  (interactive)
  (let ((passages-debug t))
    (passages--remove-overlays)
    (let ((excerpts (passages--find-all-excerpts)))
      (message "========================================")
      (message "Found %d passages to render" (length excerpts))
      (dolist (excerpt excerpts)
        (message "Rendering: page=%d file='%s' body=%d-%d"
                 (plist-get excerpt :page)
                 (plist-get excerpt :file)
                 (plist-get excerpt :body-start)
                 (plist-get excerpt :body-end))
        (passages--add-overlay-for-excerpt excerpt))
      (message "========================================")
      (message "Refresh complete: %d passages rendered" (length excerpts)))))

;;;###autoload
(defun passages-check-conflicts ()
  "Check for conflicting modes or settings."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((info '()))
      (push (format "Major mode: %s" major-mode) info)
      (push (format "Buffer size: %d bytes" (buffer-size)) info)
      (push (format "Passages-mode: %s" passages-mode) info)
      (push (format "Passages-doc-mode: %s"
                    (if (boundp 'passages-doc-mode) passages-doc-mode "N/A")) info)
      (push (format "Total overlays: %d"
                   (length (overlays-in (point-min) (point-max)))) info)
      (push (format "Passage overlays: %d"
                   (length (seq-filter
                           (lambda (ov) (overlay-get ov 'passages-overlay))
                           (overlays-in (point-min) (point-max))))) info)
      (push (format "JIT lock registered: %s"
                   (memq 'passages--jit-lock-function
                         (bound-and-true-p jit-lock-functions))) info)

      (with-current-buffer (get-buffer-create "*Passages Conflicts*")
        (erase-buffer)
        (insert "System Check Results\n")
        (insert "====================\n\n")
        (dolist (item (nreverse info))
          (insert item "\n"))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))

      (message "Check complete. See *Passages Conflicts* buffer."))))

(defun passages-debug ()
  "Show debug info."
  (interactive)
  (let ((excerpts (passages--find-all-excerpts)))
    (with-current-buffer (get-buffer-create "*Passages Debug*")
      (erase-buffer)
      (insert (format "Found %d passages:\n\n" (length excerpts)))
      (dolist (excerpt excerpts)
        (insert (format "Page: %d\n" (plist-get excerpt :page)))
        (insert (format "  File: %s\n" (plist-get excerpt :file)))
        (insert (format "  Location: %s\n" (plist-get excerpt :location)))
        (insert (format "  Body: %d-%d\n\n"
                       (plist-get excerpt :body-start)
                       (plist-get excerpt :body-end))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Auto-refresh

(defun passages--setup-auto-refresh ()
  "Setup auto-refresh on save."
  (add-hook 'after-save-hook #'passages--refresh-overlays nil t))

(defun passages--setup-jit-lock ()
  "Setup JIT lock."
  (jit-lock-register #'passages--jit-lock-function))

(defun passages--teardown-jit-lock ()
  "Remove JIT lock."
  (jit-lock-unregister #'passages--jit-lock-function))

;;; Utility

;;;###autoload
(defun passages-open-notes ()
  "Open or create notes file."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))
  (let* ((file (if (eq major-mode 'pdf-view-mode)
                  (buffer-file-name)
                (or nov-file-name (buffer-file-name))))
         (note-buffer (passages--get-note-buffer file)))
    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))))

;;; Minor mode for Org buffers (rendering, overlays, JIT-lock)

;;;###autoload
(define-minor-mode passages-mode
  "Minor mode for inline excerpts in Org buffers.
Handles rendering, overlays, and JIT-lock fontification."
  :lighter " §"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c e j") 'passages-jump-to-source)
            (define-key map (kbd "C-c e r") 'passages--refresh-overlays)
            (define-key map (kbd "C-c e d") 'passages-debug)
            (define-key map (kbd "C-c e k") 'passages-delete-at-point)
            (define-key map (kbd "C-c e l") 'passages-list-excerpts)
            map)
  (if passages-mode
      (progn
        (passages--setup-auto-refresh)
        (passages--setup-jit-lock)

        (let ((count (passages--add-overlays)))
          (if (> count 0)
              (message "Passages mode enabled with %d passages" count)
            (message "Passages mode enabled (no passages found)")))

        (when (and (fboundp 'jit-lock-fontify-now)
                   (get-buffer-window))
          (let ((win (get-buffer-window)))
            (jit-lock-fontify-now (window-start win) (window-end win))))

        (run-with-idle-timer 0.1 nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (when passages-mode
                  (passages--ensure-overlays-at-point)))))
          (current-buffer)))

    (passages--teardown-jit-lock)
    (passages--remove-overlays)
    (remove-hook 'after-save-hook #'passages--refresh-overlays t)
    (message "Passages mode disabled")))

;;; Minor mode for Document buffers (PDF/EPUB) - keybinding management

(defvar passages-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'passages-insert)
    (define-key map (kbd "i") #'passages-open-notes)
    (define-key map (kbd "I") #'passages-find-this-location-in-notes)
    map)
  "Keymap for `passages-doc-mode'.
Bindings for interaction in PDF and EPUB buffers.")

;;;###autoload
(define-minor-mode passages-doc-mode
  "Minor mode for passages keybindings in document buffers (PDF/EPUB).
This mode provides reliable keybindings that won't be overridden by
major mode maps or other minor modes.

\\{passages-doc-mode-map}"
  :lighter " §-Doc"
  :keymap passages-doc-mode-map
  (if passages-doc-mode
      (message "Passages-doc-mode enabled: e=insert, i=notes, I=find-location")
    (message "Passages-doc-mode disabled")))

;;; Setup functions - now using minor mode instead of local-set-key

;;;###autoload
(defun passages-setup-pdf ()
  "Setup passages for pdf-view-mode.
Enables `passages-doc-mode' for reliable keybindings."
  (passages-doc-mode 1))

;;;###autoload
(defun passages-setup-epub ()
  "Setup passages for nov-mode.
Enables `passages-doc-mode' for reliable keybindings."
  (passages-doc-mode 1))

(defun passages--org-mode-hook-fn ()
  "Hook function for `org-mode-hook' to auto-enable `passages-mode'."
  (when-let* ((file-name (buffer-file-name)))
    (when (or (string-match-p "\\.notes\\.org$" file-name)
              (and passages-directory
                   (string-prefix-p
                    (expand-file-name passages-directory)
                    (expand-file-name file-name)))
              (and (passages--denote-available-p)
                   (string-match-p
                    (regexp-quote (expand-file-name denote-directory))
                    (expand-file-name file-name))
                   (string-match-p
                    (concat "__" passages-denote-keyword)
                    file-name)))
      (passages-mode 1))))

;;;###autoload
(defun passages-enable ()
  "Enable passages globally."
  (interactive)
  (add-hook 'pdf-view-mode-hook #'passages-setup-pdf)
  (add-hook 'nov-mode-hook #'passages-setup-epub)
  (add-hook 'org-mode-hook #'passages--org-mode-hook-fn)
  ;; Enable in already open PDF/EPUB buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (memq major-mode '(pdf-view-mode nov-mode))
                 (not (bound-and-true-p passages-doc-mode)))
        (passages-doc-mode 1))))
  (message "Passages enabled globally"))

;;;###autoload
(defun passages-disable ()
  "Disable passages globally."
  (interactive)
  (remove-hook 'pdf-view-mode-hook #'passages-setup-pdf)
  (remove-hook 'nov-mode-hook #'passages-setup-epub)
  (remove-hook 'org-mode-hook #'passages--org-mode-hook-fn)
  ;; Disable in all currently active buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p passages-doc-mode)
        (passages-doc-mode -1))
      (when (bound-and-true-p passages-mode)
        (passages-mode -1))))
  (message "Passages disabled globally"))

;;; Utilities

(defun passages--conv-page-scroll-percentage (scroll &optional hscroll)
  "Convert SCROLL to percentage."
  (let* ((size (pdf-view-image-size))
         (height (cdr size)))
    (/ (float scroll) height)))

(defun passages--conv-page-percentage-scroll (percentage)
  "Convert PERCENTAGE to scroll position."
  (let* ((size (pdf-view-image-size))
         (height (cdr size)))
    (floor (* percentage height))))

(provide 'passages)
;;; passages.el ends here
