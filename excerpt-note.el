;;; excerpt-note.el --- Seamless inline excerpts -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Your Name
;; Version: 2.7.0
;; Package-Requires: ((emacs "27.1") (pdf-tools "1.0") (nov "0.4.0"))
;; Keywords: pdf, epub, notes, annotations, denote, citar

;;; Commentary:

;; Excerpt-note provides a lightweight note-taking system for PDFs and EPUBs.
;; Features:
;; - Seamless inline excerpts with precise anchors
;; - Preserves original text formatting from PDFs
;; - Uses org-mode standard separators (-----)
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
;; - `excerpt-note-mode': Minor mode for Org buffers (overlays, JIT-lock)
;; - `excerpt-note-doc-mode': Minor mode for PDF/EPUB buffers (keybindings)

;;; Code:

(require 'pdf-view)
(require 'nov)
(require 'org)
(require 'jit-lock)

;;; Customization

(defgroup excerpt-note nil
  "Settings for excerpt-note."
  :group 'text
  :prefix "excerpt-note-")

(defcustom excerpt-note-auto-parse t
  "Automatically parse excerpts when opening a file."
  :type 'boolean
  :group 'excerpt-note)

(defcustom excerpt-note-directory nil
  "Directory for storing note files."
  :type '(choice (const :tag "Next to source files" nil)
                 (directory :tag "Custom directory"))
  :group 'excerpt-note)

(defcustom excerpt-note-use-citar t
  "Try to use existing citar-denote notes if available."
  :type 'boolean
  :group 'excerpt-note)

(defcustom excerpt-note-use-denote t
  "Use denote for note management when citar-denote is not available."
  :type 'boolean
  :group 'excerpt-note)

(defcustom excerpt-note-denote-keyword "excerpt"
  "Denote keyword (file tag) for excerpt notes."
  :type 'string
  :group 'excerpt-note)

(defcustom excerpt-note-use-relative-paths t
  "Store relative paths instead of absolute paths for portability."
  :type 'boolean
  :group 'excerpt-note)

(defcustom excerpt-note-search-paths
  '("~/Library/CloudStorage/Dropbox/org/bib/files/"
    "~/Library/CloudStorage/Dropbox/org/books/"
    "~/Library/CloudStorage/Dropbox/org/")
  "Directories to search for source files when resolving relative paths.
Paths are tried in order until file is found."
  :type '(repeat directory)
  :group 'excerpt-note)

(defcustom excerpt-note-debug nil
  "Enable debug logging for troubleshooting."
  :type 'boolean
  :group 'excerpt-note)

;;; Faces

(defface excerpt-note-content-face
  '((t :foreground "#666666" :extend t))
  "Face for excerpt content (gray text)."
  :group 'excerpt-note)

(defface excerpt-note-marker-face
  '((t :foreground "#999999" :height 0.85 :weight bold))
  "Face for page marker prefix like 'p.42 ➜'."
  :group 'excerpt-note)

(defface excerpt-note-separator-face
  '((t :foreground "#cccccc" :strike-through t))
  "Face for separator line -----."
  :group 'excerpt-note)

;;; Citar-Denote integration

(defun excerpt-note--citar-denote-available-p ()
  "Check if citar-denote is available and active."
  (and excerpt-note-use-citar
       (require 'citar nil t)
       (require 'citar-denote nil t)
       (bound-and-true-p citar-denote-mode)
       (fboundp 'citar-denote--get-notes)
       (fboundp 'denote-directory-files)))

(defun excerpt-note--expand-bib-file (filename)
  "Expand FILENAME relative to bibliography or library paths."
  (let ((possible-paths '()))

    (when (file-name-absolute-p filename)
      (push (expand-file-name filename) possible-paths))

    (when-let ((lib-paths (bound-and-true-p citar-library-paths)))
      (dolist (lib-path lib-paths)
        (let ((full-path (expand-file-name filename lib-path)))
          (when (file-exists-p full-path)
            (push full-path possible-paths)))))

    (when-let ((bib-files (bound-and-true-p citar-bibliography)))
      (dolist (bib-file bib-files)
        (when (file-exists-p bib-file)
          (let* ((bib-dir (file-name-directory bib-file))
                 (full-path (expand-file-name filename bib-dir)))
            (when (file-exists-p full-path)
              (push full-path possible-paths))))))

    (delete-dups possible-paths)))

(defun excerpt-note--find-citar-key-for-file (file)
  "Find citar citation key for FILE by searching bibliography entries."
  (when (require 'citar nil t)
    (let ((file-normalized (expand-file-name file))
          (file-name (file-name-nondirectory file)))

      (when excerpt-note-debug
        (message "=== Searching for citekey ===")
        (message "Looking for file: %s" file-normalized)
        (message "Filename: %s" file-name))

      (catch 'found
        (condition-case err
            (maphash
             (lambda (key entry-data)
               (let ((file-field (or (cdr (assoc "file" entry-data))
                                    (cdr (assoc "_file" entry-data)))))

                 (when (and file-field excerpt-note-debug)
                   (message "Entry %s has file field: %s" key file-field))

                 (when file-field
                   (let* ((bib-files (mapcar #'string-trim
                                            (split-string file-field ";" t "[ \t\n]+")))
                          (expanded-files '()))

                     (dolist (bib-file bib-files)
                       (let ((expanded (excerpt-note--expand-bib-file bib-file)))
                         (setq expanded-files (append expanded-files expanded))))

                     (when excerpt-note-debug
                       (message "  Expanded to: %s" expanded-files))

                     (when (or
                            (member file-normalized expanded-files)
                            (member file-name
                                   (mapcar #'file-name-nondirectory
                                          (append bib-files expanded-files))))
                       (when excerpt-note-debug
                         (message "  ✓ MATCH! Citekey: %s" key))
                       (throw 'found key))))))
             (citar-get-entries))
          (error
           (message "Error searching citar entries: %s" (error-message-string err))
           nil))

        (when excerpt-note-debug
          (message "  ✗ No match found"))
        nil))))

(defun excerpt-note--find-citar-note (source-file)
  "Try to find existing citar-denote note for SOURCE-FILE."
  (when (excerpt-note--citar-denote-available-p)
    (let ((citekey (excerpt-note--find-citar-key-for-file source-file)))
      (if citekey
          (condition-case nil
              (let ((notes-hash (citar-denote--get-notes (list citekey))))
                (when notes-hash
                  (let ((note-files (gethash citekey notes-hash)))
                    (when note-files
                      (let ((note-file (car note-files)))
                        (message "Using existing citar-denote note: %s"
                                (file-name-nondirectory note-file))
                        (excerpt-note--normalize-path note-file))))))
            (error nil))
        (when excerpt-note-debug
          (message "No citation key found in bibliography for: %s"
                  (file-name-nondirectory source-file)))
        nil))))

;;; Denote integration

(defun excerpt-note--denote-available-p ()
  "Check if denote is available."
  (and excerpt-note-use-denote
       (require 'denote nil t)
       (fboundp 'denote)
       (fboundp 'denote-directory-files)
       (bound-and-true-p denote-directory)))

(defun excerpt-note--find-denote-note (source-file)
  "Find existing denote note for SOURCE-FILE."
  (when (excerpt-note--denote-available-p)
    (let* ((source-normalized (expand-file-name source-file))
           (source-name (file-name-nondirectory source-file))
           (excerpt-files (denote-directory-files
                          (concat "_" excerpt-note-denote-keyword))))

      (catch 'found
        (dolist (note-file excerpt-files)
          (with-temp-buffer
            (insert-file-contents note-file)
            (goto-char (point-min))
            (when (re-search-forward
                   "^#\\+SOURCE_FILE:[ \t]*\\(.+\\)$"
                   nil t)
              (let* ((stored-path (string-trim (match-string 1)))
                     (resolved-path (excerpt-note--resolve-path stored-path)))
                (when (or (string= resolved-path source-normalized)
                          (string= (file-name-nondirectory stored-path)
                                  source-name))
                  (message "Using existing denote note: %s"
                          (file-name-nondirectory note-file))
                  (throw 'found (excerpt-note--normalize-path note-file)))))))
        nil))))

(defun excerpt-note--create-denote-note (source-file)
  "Create a new denote note for SOURCE-FILE."
  (when (excerpt-note--denote-available-p)
    (let* ((title (concat "Notes: " (file-name-nondirectory source-file)))
           (keywords (list excerpt-note-denote-keyword))
           (subdirectory nil)
           ;; Temporarily prevent denote from opening the new file
           ;; This avoids disrupting the current window/buffer
           (denote-open-after-create nil)
           (note-file (denote
                       title
                       keywords
                       'org
                       subdirectory
                       nil
                       nil
                       nil)))

      (with-current-buffer (find-file-noselect note-file)
        (save-excursion
          (goto-char (point-min))
          (while (and (not (eobp))
                     (looking-at "^#\\+"))
            (forward-line 1))
          (insert "#+SOURCE_FILE: "
                 (excerpt-note--make-relative-path source-file)
                 "\n\n")
          (save-buffer)))

      (message "Created denote note: %s" (file-name-nondirectory note-file))
      (excerpt-note--normalize-path note-file))))

;;; Anchor utilities

(defun excerpt-note--get-location-page (location)
  "Get page/chapter from LOCATION."
  (cond
   ((numberp location) location)
   ((consp location) (car location))
   ((stringp location)
    (when (string-match "\\([0-9]+\\)" location)
      (string-to-number (match-string 1 location))))
   (t 0)))

(defun excerpt-note--get-location-top (location)
  "Get vertical position from LOCATION."
  (cond
   ((numberp location) 0)
   ((consp location)
    (if (consp (cdr location))
        (cadr location)
      (cdr location)))
   (t 0)))

(defun excerpt-note--get-location-left (location)
  "Get horizontal position from LOCATION."
  (when (and (consp location) (consp (cdr location)))
    (cddr location)))

;;; Path utilities

(defun excerpt-note--get-project-root ()
  "Get project root directory if available."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (and (fboundp 'project-current)
           (when-let ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj)))))
      default-directory))

(defun excerpt-note--normalize-path (path)
  "Normalize PATH to canonical absolute path."
  (expand-file-name path))

(defun excerpt-note--make-relative-path (absolute-path)
  "Convert ABSOLUTE-PATH to relative path if possible."
  (if excerpt-note-use-relative-paths
      (let ((project-root (excerpt-note--get-project-root)))
        (if (and project-root
                 (string-prefix-p project-root absolute-path))
            (file-relative-name absolute-path project-root)
          absolute-path))
    absolute-path))

(defun excerpt-note--find-file-in-search-paths (filename)
  "Search for FILENAME in configured search paths.
Returns absolute path if found, nil otherwise."
  (catch 'found
    ;; Try configured search paths
    (dolist (dir excerpt-note-search-paths)
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
    (let ((project-root (excerpt-note--get-project-root)))
      (when (and project-root (file-directory-p project-root))
        (condition-case nil
            (when-let ((matches (directory-files-recursively
                                project-root
                                (concat "^" (regexp-quote filename) "$")
                                nil)))
              (when matches
                (throw 'found (car matches))))
          (error nil))))

    nil))

(defun excerpt-note--resolve-path (path)
  "Resolve PATH to absolute path with intelligent search.
Handles:
1. Absolute paths - returned as-is
2. Relative paths with directory - resolved relative to current buffer/project
3. Bare filenames - searched in configured directories"
  (cond
   ;; Already absolute
   ((file-name-absolute-p path)
    (excerpt-note--normalize-path path))

   ;; Has directory component (e.g., "../books/file.pdf")
   ((string-match-p "/" path)
    (let ((base-dir (if (buffer-file-name)
                        (file-name-directory (buffer-file-name))
                      (excerpt-note--get-project-root))))
      (excerpt-note--normalize-path (expand-file-name path base-dir))))

   ;; Bare filename (e.g., "book.pdf") - search in configured paths
   (t
    (or (excerpt-note--find-file-in-search-paths path)
        ;; Fallback: relative to current buffer
        (let ((base-dir (if (buffer-file-name)
                            (file-name-directory (buffer-file-name))
                          (excerpt-note--get-project-root))))
          (excerpt-note--normalize-path (expand-file-name path base-dir)))))))

;;; PDF functions

(defun excerpt-note--pdf-get-precise-location ()
  "Get precise location in PDF."
  (let ((page (pdf-view-current-page))
        v-pos h-pos)
    (if (pdf-view-active-region-p)
        (let ((edges (car (pdf-view-active-region))))
          (setq v-pos (min (nth 1 edges) (nth 3 edges))
                h-pos (min (nth 0 edges) (nth 2 edges))))
      (setq v-pos (excerpt-note--conv-page-scroll-percentage
                   (window-vscroll))))
    (if h-pos
        (cons page (cons v-pos h-pos))
      (cons page v-pos))))

(defun excerpt-note--pdf-goto-location (file location)
  "Jump to LOCATION in PDF FILE."
  (let* ((resolved-file (excerpt-note--resolve-path file))
         (existing-buf (find-buffer-visiting resolved-file)))

    (if existing-buf
        (pop-to-buffer existing-buf)
      (find-file-other-window resolved-file))

    (let ((page (excerpt-note--get-location-page location))
          (v-pos (excerpt-note--get-location-top location)))
      (pdf-view-goto-page page)
      (when (and (numberp v-pos) (> v-pos 0))
        (let ((scroll-pos (excerpt-note--conv-page-percentage-scroll v-pos)))
          (image-scroll-up (- scroll-pos (window-vscroll))))))))

;;; EPUB functions

(defun excerpt-note--epub-get-precise-location ()
  "Get precise location in EPUB."
  (let ((chapter nov-documents-index))
    (if (region-active-p)
        (cons chapter (cons (region-beginning) (region-end)))
      (cons chapter (point)))))

(defun excerpt-note--epub-goto-location (file location)
  "Jump to LOCATION in EPUB FILE using nov.el native methods."
  (let* ((resolved-file (excerpt-note--resolve-path file))
         (existing-buf (find-buffer-visiting resolved-file))
         (chapter (excerpt-note--get-location-page location))
         (pos (excerpt-note--get-location-top location)))

    (if existing-buf
        (pop-to-buffer existing-buf)
      (find-file-other-window resolved-file))

    (with-current-buffer (current-buffer)
      (unless (eq major-mode 'nov-mode)
        (nov-mode))

      (when (and (boundp 'nov-documents) nov-documents)
        (setq nov-documents-index chapter)
        (nov-render-document))

      (goto-char (min (round pos) (point-max)))
      (recenter))))

;;; Generic functions

(defun excerpt-note--get-current-location ()
  "Get current precise location."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (list :type 'pdf
          :file (excerpt-note--make-relative-path (buffer-file-name))
          :location (excerpt-note--pdf-get-precise-location)))
   ((eq major-mode 'nov-mode)
    (list :type 'epub
          :file (excerpt-note--make-relative-path
                 (or nov-file-name (buffer-file-name)))
          :location (excerpt-note--epub-get-precise-location)))
   (t (user-error "Not in PDF or EPUB buffer"))))

(defun excerpt-note--goto-location (file location type)
  "Jump to LOCATION in FILE of TYPE."
  (cond
   ((eq type 'pdf)
    (excerpt-note--pdf-goto-location file location))
   ((eq type 'epub)
    (excerpt-note--epub-goto-location file location))
   (t (error "Unknown type: %s" type))))

;;; Text extraction - Simple and direct

(defcustom excerpt-note-normalize-text t
  "Whether to normalize extracted text by merging line breaks.
When non-nil, soft line breaks from PDF extraction are merged
while preserving paragraph boundaries (double newlines)."
  :type 'boolean
  :group 'excerpt-note)

(defun excerpt-note--normalize-text (text)
  "Normalize TEXT extracted from PDF/EPUB.
- Preserve paragraph boundaries (double+ newlines)
- Handle hyphenated word breaks (word-\\n -> word)
- Merge Chinese characters without adding space
- Replace remaining single newlines with space
- Clean up excessive whitespace"
  (if (or (null text) (string-empty-p text) (not excerpt-note-normalize-text))
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

(defun excerpt-note--get-selected-text ()
  "Get selected text from PDF/EPUB, normalized."
  (let ((raw-text
         (cond
          ((eq major-mode 'pdf-view-mode)
           (if (pdf-view-active-region-p)
               (car (pdf-view-active-region-text))
             (user-error "No text selected in PDF")))

          ((eq major-mode 'nov-mode)
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (user-error "No text selected in EPUB")))

          (t (user-error "Not in PDF or EPUB buffer")))))
    (excerpt-note--normalize-text raw-text)))

(defun excerpt-note--format-location (location)
  "Format LOCATION for storage."
  (let ((page (excerpt-note--get-location-page location))
        (v-pos (excerpt-note--get-location-top location))
        (h-pos (excerpt-note--get-location-left location)))
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

(defun excerpt-note--find-excerpts-in-region (start end)
  "Find excerpts in region from START to END with org-mode separator."
  (let (excerpts)
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "^[ \t]*@p\\.\\([0-9]+\\)[ \t]*|[ \t]*\\([^|]+?\\)[ \t]*|[ \t]*\\(.+\\)$"
              end t)
        (let* ((page (string-to-number (match-string 1)))
               (file (string-trim (match-string 2)))
               (location-str (string-trim (match-string 3)))
               (marker-start (match-beginning 0))
               (marker-end (match-end 0)))

          (when excerpt-note-debug
            (message "Found excerpt: page=%d file='%s' loc='%s'"
                    page file location-str))

          (forward-line 1)
          (let ((body-start (point))
                (body-end (save-excursion
                           ;; Match org separator (5+ dashes) or next marker
                           (if (re-search-forward
                                "^[ \t]*\\(-----+[ \t]*$\\|@p\\.\\)"
                                nil t)
                               (line-beginning-position)
                             (point-max)))))

            (when excerpt-note-debug
              (message "  body-start=%d body-end=%d" body-start body-end))

            (when (< body-start body-end)
              (push (list :marker-start marker-start
                         :marker-end marker-end
                         :page page
                         :file file
                         :location location-str
                         :body-start body-start
                         :body-end body-end)
                    excerpts))))))
    (nreverse excerpts)))

(defun excerpt-note--find-all-excerpts ()
  "Find all excerpts in buffer."
  (excerpt-note--find-excerpts-in-region (point-min) (point-max)))

(defun excerpt-note--find-excerpts-for-page (page file)
  "Find all excerpts for PAGE in FILE in current buffer."
  (let ((positions '())
        (file-name (file-name-nondirectory file)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[ \t]*@p\\.%d[ \t]*|[ \t]*%s[ \t]*|"
                      page
                      (regexp-quote file-name))
              nil t)
        (push (line-beginning-position) positions)))
    (nreverse positions)))

;;; Overlay protection

(defun excerpt-note--protect-metadata (ov after-p _beg _end &optional _len)
  "Prevent modification of protected metadata regions."
  (unless after-p
    (when (overlay-get ov 'read-only)
      (user-error "This metadata is protected"))))

;;; Optimized overlay system with org separator

(defun excerpt-note--jit-lock-function (start end)
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

      (remove-overlays start end 'excerpt-note-overlay t)

      (let ((excerpts (excerpt-note--find-excerpts-in-region start end)))
        (dolist (excerpt excerpts)
          (excerpt-note--add-overlay-for-excerpt excerpt)))

      ;; Style org-mode separators (5+ dashes)
      (save-excursion
        (goto-char start)
        (while (re-search-forward "^[ \t]*-----+[ \t]*$" end t)
          (let ((sep-ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put sep-ov 'face 'excerpt-note-separator-face)
            (overlay-put sep-ov 'excerpt-note-overlay t)
            (overlay-put sep-ov 'evaporate t)))))))

(defun excerpt-note--add-overlay-for-excerpt (excerpt)
  "Add overlay for a single EXCERPT with optimized single-overlay approach."
  (let* ((marker-start (plist-get excerpt :marker-start))
         (marker-end (plist-get excerpt :marker-end))
         (body-start (plist-get excerpt :body-start))
         (body-end (plist-get excerpt :body-end))
         (page (plist-get excerpt :page))
         (file (plist-get excerpt :file)))

    ;; Single overlay for metadata line
    (let ((marker-ov (make-overlay marker-start (1+ marker-end)))
          (badge-map (make-sparse-keymap))
          (prefix (propertize (format "p.%d ➜ " page)
                            'face 'excerpt-note-marker-face
                            'mouse-face 'highlight
                            'help-echo "Click or C-c C-o to jump")))

      (define-key badge-map [mouse-1]
        (lambda (event)
          (interactive "e")
          (excerpt-note-jump-to-source)))

      (setq prefix (propertize prefix 'keymap badge-map))

      (overlay-put marker-ov 'before-string prefix)
      (overlay-put marker-ov 'invisible t)
      (overlay-put marker-ov 'read-only t)
      (overlay-put marker-ov 'modification-hooks
                   '(excerpt-note--protect-metadata))
      (overlay-put marker-ov 'insert-in-front-hooks
                   '(excerpt-note--protect-metadata))
      (overlay-put marker-ov 'excerpt-note-overlay t)
      (overlay-put marker-ov 'excerpt-note-file file)
      (overlay-put marker-ov 'evaporate t))

    ;; Body overlay for styling
    (let ((body-ov (make-overlay body-start body-end)))
      (overlay-put body-ov 'face 'excerpt-note-content-face)
      (overlay-put body-ov 'priority 100)
      (overlay-put body-ov 'excerpt-note-overlay t)
      (overlay-put body-ov 'excerpt-note-file file)
      (overlay-put body-ov 'evaporate t)
      (overlay-put body-ov 'help-echo
                  (format "Excerpt from page %d" page)))))

(defun excerpt-note--add-overlays ()
  "Add overlays to all excerpts."
  (interactive)
  (excerpt-note--remove-overlays)
  (let ((excerpts (excerpt-note--find-all-excerpts))
        (count 0))

    (dolist (excerpt excerpts)
      (excerpt-note--add-overlay-for-excerpt excerpt)
      (setq count (1+ count)))

    ;; Style org-mode separators (5+ dashes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*-----+[ \t]*$" nil t)
        (let ((sep-ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put sep-ov 'face 'excerpt-note-separator-face)
          (overlay-put sep-ov 'excerpt-note-overlay t)
          (overlay-put sep-ov 'evaporate t))))

    (when excerpt-note-debug
      (message "Added %d styled excerpt cards" count))
    count))

(defun excerpt-note--remove-overlays ()
  "Remove all excerpt overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'excerpt-note-overlay t))

(defun excerpt-note--refresh-overlays ()
  "Refresh all overlays."
  (interactive)
  (when (and (buffer-live-p (current-buffer))
             (derived-mode-p 'org-mode))
    (excerpt-note--remove-overlays)
    (excerpt-note--add-overlays)))

(defun excerpt-note--ensure-overlays-at-point ()
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

(defun excerpt-note--get-note-file-path (source-file)
  "Determine the path for note file of SOURCE-FILE."
  (let ((normalized-source (excerpt-note--normalize-path source-file)))
    (or
     (excerpt-note--find-citar-note normalized-source)

     (when (excerpt-note--denote-available-p)
       (or (excerpt-note--find-denote-note normalized-source)
           (excerpt-note--create-denote-note normalized-source)))

     (when excerpt-note-directory
       (unless (file-directory-p excerpt-note-directory)
         (make-directory excerpt-note-directory t))
       (excerpt-note--normalize-path
        (expand-file-name
         (concat (file-name-base normalized-source) ".notes.org")
         excerpt-note-directory)))

     (excerpt-note--normalize-path
      (concat normalized-source ".notes.org")))))

(defun excerpt-note--ensure-file-header (buffer source-file)
  "Ensure BUFFER has proper header."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "^#\\+TITLE:")
        (insert "#+TITLE: Notes for " (file-name-nondirectory source-file) "\n")
        (insert "#+SOURCE_FILE: " source-file "\n\n")))))

(defun excerpt-note--get-note-buffer (source-file)
  "Get or create note buffer for SOURCE-FILE."
  (let* ((note-file (excerpt-note--get-note-file-path source-file))
         (buf (or (find-buffer-visiting note-file)
                  (find-file-noselect note-file))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless (and (excerpt-note--denote-available-p)
                   (string-match-p (regexp-quote (expand-file-name denote-directory))
                                  (expand-file-name note-file)))
        (excerpt-note--ensure-file-header buf source-file))
      (when excerpt-note-auto-parse
        (excerpt-note-mode 1)))
    buf))

;;; Excerpt insertion with org separator

(defun excerpt-note--do-insert (text loc-info)
  "Internal function to insert excerpt with TEXT and LOC-INFO.
Structure:
  [user notes area]
  @p.XX | file | location
  [excerpt content]
  -----"
  (let* ((location (plist-get loc-info :location))
         (file (plist-get loc-info :file))
         (type (plist-get loc-info :type))
         (page (excerpt-note--get-location-page location))
         (formatted-location (excerpt-note--format-location location))
         (note-buffer (excerpt-note--get-note-buffer file))
         note-position)

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))
    (goto-char (point-max))

    (unless (looking-back "\n\n" 2)
      (if (looking-back "\n" 1)
          (insert "\n")
        (insert "\n\n")))

    ;; 1. User notes area (empty line for user to fill)
    (setq note-position (point))
    (insert "\n\n")

    ;; 2. Marker line
    (let ((marker-start (point)))
      (insert (format "@p.%d | %s | %s\n"
                     page
                     file
                     formatted-location))

      (add-text-properties marker-start (point)
                          `(excerpt-note-marker t
                            excerpt-note-page ,page
                            excerpt-note-file ,file
                            excerpt-note-type ,type
                            excerpt-note-location ,formatted-location)))

    ;; 3. Excerpt content
    (let ((content-start (point)))
      (when text
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n")))

      (add-text-properties content-start (point)
                          `(excerpt-note-content t
                            excerpt-note-page ,page
                            excerpt-note-file ,file
                            excerpt-note-type ,type
                            excerpt-note-location ,formatted-location)))

    ;; 4. Separator
    (let ((sep-start (point)))
      (insert "-----\n\n")
      (add-text-properties sep-start (- (point) 1)
                          '(excerpt-note-separator t)))

    ;; Position cursor at notes area for immediate editing
    (goto-char note-position)

    (when excerpt-note-mode
      (run-with-idle-timer 0.1 nil 'excerpt-note--refresh-overlays))

    (if text
        (message "✓ Excerpt from p.%d inserted. Add your notes above the marker" page)
      (message "✓ Empty anchor at p.%d created. Add your notes above the marker" page))))

;;;###autoload
(defun excerpt-note-insert ()
  "Insert excerpt at the END of buffer."
  (interactive)
  (let* ((has-selection (cond
                         ((eq major-mode 'pdf-view-mode)
                          (pdf-view-active-region-p))
                         ((eq major-mode 'nov-mode)
                          (use-region-p))
                         (t nil)))
         (text (when has-selection
                 (excerpt-note--get-selected-text)))
         (loc-info (excerpt-note--get-current-location)))

    (if has-selection
        (excerpt-note--do-insert text loc-info)

      (if (y-or-n-p "No text selected. Create an empty anchor for this page? ")
          (excerpt-note--do-insert nil loc-info)
        (message "Action cancelled")))))

;;; Find notes for current location

;;;###autoload
(defun excerpt-note-find-this-location-in-notes ()
  "Find and jump to notes for current location in PDF/EPUB."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))

  (let* ((loc-info (excerpt-note--get-current-location))
         (file (plist-get loc-info :file))
         (location (plist-get loc-info :location))
         (page (excerpt-note--get-location-page location))
         (note-buffer (excerpt-note--get-note-buffer file)))

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))

    (unless excerpt-note-mode
      (excerpt-note-mode 1))

    (let ((positions (excerpt-note--find-excerpts-for-page page file)))
      (cond
       ((null positions)
        (message "No excerpts found for page %d. Press 'e' to create one" page))

       ((= (length positions) 1)
        (goto-char (car positions))
        (excerpt-note--ensure-overlays-at-point)
        (recenter)
        (message "✓ Found excerpt for page %d" page))

       (t
        (let* ((choices
                (mapcar
                 (lambda (pos)
                   (save-excursion
                     (goto-char pos)
                     (forward-line 1)
                     (let ((excerpt-start (point))
                           (excerpt-end (save-excursion
                                         (if (re-search-forward
                                              "^[ \t]*\\(-----+\\|@p\\.\\)"
                                              nil t)
                                             (line-beginning-position)
                                           (point-max)))))
                       (cons
                        (format "p.%d: %s"
                                page
                                (truncate-string-to-width
                                 (string-trim
                                  (buffer-substring-no-properties
                                   excerpt-start
                                   (min (+ excerpt-start 80) excerpt-end)))
                                 60 nil nil "..."))
                        pos))))
                 positions))
               (choice (completing-read
                       (format "Found %d excerpts for page %d. Choose: "
                               (length positions) page)
                       choices
                       nil t)))
          (goto-char (cdr (assoc choice choices)))
          (excerpt-note--ensure-overlays-at-point)
          (recenter)
          (message "✓ Jumped to excerpt")))))))

;;;###autoload
(defun excerpt-note-find-region-in-notes ()
  "Find notes containing the selected text."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))

  (let* ((text (excerpt-note--get-selected-text))
         (loc-info (excerpt-note--get-current-location))
         (file (plist-get loc-info :file))
         (note-buffer (excerpt-note--get-note-buffer file))
         (search-text (substring text 0 (min 40 (length text)))))

    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))

    (unless excerpt-note-mode
      (excerpt-note-mode 1))

    (goto-char (point-min))
    (if (search-forward search-text nil t)
        (progn
          (re-search-backward "^[ \t]*@p\\." nil t)
          (excerpt-note--ensure-overlays-at-point)
          (recenter)
          (message "✓ Found excerpt containing selected text"))
      (message "Selected text not found in notes. Press 'e' to create excerpt"))))

;;; Jump to source

(defun excerpt-note--parse-marker-line-at-point ()
  "Parse excerpt marker line at or near point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^[ \t]*@p\\.")
      (re-search-backward "^[ \t]*@p\\." nil t))

    (when (looking-at "^[ \t]*@p\\.\\([0-9]+\\)[ \t]*|[ \t]*\\([^|]+?\\)[ \t]*|[ \t]*\\(.+\\)$")
      (let* ((page (string-to-number (match-string 1)))
             (file (string-trim (match-string 2)))
             (location-str (string-trim (match-string 3)))
             (type (if (and file (string-match-p "\\.epub$" file)) 'epub 'pdf)))

        (list :page page
              :file file
              :location location-str
              :type type)))))

(defun excerpt-note--get-excerpt-properties-at-point (&optional pos)
  "Get excerpt properties at POS."
  (let* ((pos (or pos (point)))
         (get-prop (lambda (p prop)
                    (when p (get-text-property p prop))))
         (file (funcall get-prop pos 'excerpt-note-file))
         (found-pos pos)
         props)

    ;; Try text properties
    (unless file
      (save-excursion
        (goto-char pos)
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (setq found-pos (previous-single-property-change
                          pos 'excerpt-note-file nil line-beg))
          (when found-pos
            (setq file (funcall get-prop found-pos 'excerpt-note-file)))

          (unless file
            (setq found-pos (next-single-property-change
                            pos 'excerpt-note-file nil line-end))
            (when found-pos
              (setq file (funcall get-prop found-pos 'excerpt-note-file)))))))

    ;; Try overlays
    (unless file
      (let ((overlays (overlays-at pos)))
        (dolist (ov overlays)
          (when-let ((ov-file (overlay-get ov 'excerpt-note-file)))
            (setq file ov-file)
            (setq found-pos pos)))))

    (when (and file found-pos)
      (setq props
            (list :file file
                  :location (funcall get-prop found-pos 'excerpt-note-location)
                  :page (funcall get-prop found-pos 'excerpt-note-page)
                  :type (funcall get-prop found-pos 'excerpt-note-type))))

    ;; Fallback: Parse marker line
    (unless (and props (plist-get props :page))
      (setq props (excerpt-note--parse-marker-line-at-point)))

    props))

(defun excerpt-note-jump-to-source ()
  "Jump to source location of excerpt at point."
  (interactive)

  (when (fboundp 'jit-lock-fontify-now)
    (jit-lock-fontify-now (line-beginning-position) (line-end-position)))

  (let ((props (excerpt-note--get-excerpt-properties-at-point)))
    (if props
        (let* ((file (plist-get props :file))
               (location-str (plist-get props :location))
               (page (plist-get props :page))
               (type (plist-get props :type)))

          (unless file
            (user-error "Could not determine source file"))
          (unless page
            (user-error "Could not determine page number"))
          (unless location-str
            (user-error "Could not determine location"))

          (unless type
            (setq type (if (string-match-p "\\.epub$" file) 'epub 'pdf)))

          (let ((location (condition-case nil
                             (let ((parsed (car (read-from-string location-str))))
                               (if (or (numberp parsed) (consp parsed))
                                   parsed
                                 location-str))
                           (error location-str))))

            (when excerpt-note-debug
              (message "Jumping to: %s" file)
              (message "  Page: %d" page)
              (message "  Type: %s" type)
              (message "  Location: %s" location-str))

            (let ((resolved-file (excerpt-note--resolve-path file)))
              (unless (file-exists-p resolved-file)
                (user-error "Source file not found: %s" resolved-file)))

            (condition-case err
                (progn
                  (excerpt-note--goto-location file location type)
                  (message "✓ Jumped to %s p.%d" (file-name-nondirectory file) page))
              (error
               (message "Error jumping to source: %s" (error-message-string err))
               (user-error "Failed to jump to source")))))

      (user-error "Not in an excerpt. Move cursor to gray text or marker line"))))

;;; Path fix utility

;;;###autoload
(defun excerpt-note-fix-file-paths ()
  "Fix old excerpts that only have filename without path."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in org-mode buffer"))

  (let ((fixed-count 0)
        (failed-files '()))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*@p\\.\\([0-9]+\\)[ \t]*|[ \t]*\\([^|]+?\\)[ \t]*|[ \t]*\\(.+\\)$"
              nil t)
        (let* ((page (string-to-number (match-string 1)))
               (file (string-trim (match-string 2)))
               (location-str (string-trim (match-string 3)))
               (marker-start (match-beginning 0))
               (marker-end (match-end 0)))

          (when (and file
                     (not (string-match-p "/" file))
                     (not (string-match-p "\\\\" file)))

            (let ((found-path nil))

              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^#\\+SOURCE_FILE:[ \t]*\\(.+\\)$" nil t)
                  (let ((source-file (string-trim (match-string 1))))
                    (when (and source-file (string-suffix-p file source-file))
                      (setq found-path source-file)))))

              (unless found-path
                (when-let ((absolute-path (excerpt-note--find-file-in-search-paths file)))
                  (setq found-path (excerpt-note--make-relative-path absolute-path))))

              (unless found-path
                (when (y-or-n-p (format "Could not find '%s'. Locate manually? " file))
                  (let ((user-path (read-file-name
                                   (format "Locate file '%s': " file)
                                   (car excerpt-note-search-paths)
                                   nil t)))
                    (when (and user-path (file-exists-p user-path))
                      (setq found-path (excerpt-note--make-relative-path user-path))))))

              (if found-path
                  (progn
                    (goto-char marker-start)
                    (delete-region marker-start marker-end)
                    (insert (format "@p.%d | %s | %s"
                                   page
                                   found-path
                                   location-str))
                    (setq fixed-count (1+ fixed-count))
                    (message "Fixed: %s → %s" file found-path))

                (push file failed-files)))))))

    (if (> fixed-count 0)
        (progn
          (save-buffer)
          (excerpt-note--refresh-overlays)
          (message "✓ Fixed %d excerpts" fixed-count))
      (message "No excerpts need fixing"))

    (when failed-files
      (message "⚠ Could not locate: %s" (string-join failed-files ", ")))))

;;;###autoload
(defun excerpt-note-update-source-file ()
  "Update the source file path for current excerpt."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^[ \t]*@p\\.")
      (re-search-backward "^[ \t]*@p\\." nil t))

    (when (looking-at "^[ \t]*@p\\.\\([0-9]+\\)[ \t]*|[ \t]*\\([^|]+?\\)[ \t]*|[ \t]*\\(.+\\)$")
      (let* ((page (match-string 1))
             (old-file (string-trim (match-string 2)))
             (location (string-trim (match-string 3)))
             (new-file (read-file-name
                       (format "Locate source file '%s': " old-file)
                       (car excerpt-note-search-paths)
                       nil t)))

        (when (file-exists-p new-file)
          (let ((relative-path (excerpt-note--make-relative-path new-file)))
            (delete-region (match-beginning 0) (match-end 0))
            (insert (format "@p.%s | %s | %s"
                           page relative-path location))
            (save-buffer)
            (excerpt-note--refresh-overlays)
            (message "✓ Updated path: %s" relative-path)))))))

;;; Debug utilities

;;;###autoload
(defun excerpt-note-show-selected-text ()
  "Show what text will be extracted (for debugging)."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (if (pdf-view-active-region-p)
        (let ((text (car (pdf-view-active-region-text))))
          (with-current-buffer (get-buffer-create "*Excerpt Preview*")
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
(defun excerpt-note-debug-at-point ()
  "Debug excerpt detection at current position."
  (interactive)
  (let ((excerpt-note-debug t))
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (results '()))
        (while (re-search-forward
                "^[ \t]*@p\\.\\([0-9]+\\)[ \t]*|[ \t]*\\([^|]+?\\)[ \t]*|[ \t]*\\(.+\\)$"
                nil t)
          (setq count (1+ count))
          (let ((info (format "Excerpt %d: page=%s file='%s' loc='%s'"
                            count
                            (match-string 1)
                            (match-string 2)
                            (match-string 3))))
            (push info results)
            (message "%s" info)))

        (if (> count 0)
            (progn
              (message "========================================")
              (message "Total excerpts found: %d" count)
              (with-current-buffer (get-buffer-create "*Excerpt Debug*")
                (erase-buffer)
                (insert "Excerpt Detection Results\n")
                (insert "==========================\n\n")
                (insert (format "Total found: %d\n\n" count))
                (dolist (result (nreverse results))
                  (insert result "\n"))
                (goto-char (point-min))
                (pop-to-buffer (current-buffer))))
          (message "No excerpts found! Check your marker format."))))))

;;;###autoload
(defun excerpt-note-force-refresh ()
  "Force refresh all overlays with detailed logging."
  (interactive)
  (let ((excerpt-note-debug t))
    (excerpt-note--remove-overlays)
    (let ((excerpts (excerpt-note--find-all-excerpts)))
      (message "========================================")
      (message "Found %d excerpts to render" (length excerpts))
      (dolist (excerpt excerpts)
        (message "Rendering: page=%d file='%s' body=%d-%d"
                 (plist-get excerpt :page)
                 (plist-get excerpt :file)
                 (plist-get excerpt :body-start)
                 (plist-get excerpt :body-end))
        (excerpt-note--add-overlay-for-excerpt excerpt))
      (message "========================================")
      (message "Refresh complete: %d excerpts rendered" (length excerpts)))))

;;;###autoload
(defun excerpt-note-check-conflicts ()
  "Check for conflicting modes or settings."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((info '()))
      (push (format "Major mode: %s" major-mode) info)
      (push (format "Buffer size: %d bytes" (buffer-size)) info)
      (push (format "Excerpt-note-mode: %s" excerpt-note-mode) info)
      (push (format "Excerpt-note-doc-mode: %s"
                    (if (boundp 'excerpt-note-doc-mode) excerpt-note-doc-mode "N/A")) info)
      (push (format "Total overlays: %d"
                   (length (overlays-in (point-min) (point-max)))) info)
      (push (format "Excerpt overlays: %d"
                   (length (seq-filter
                           (lambda (ov) (overlay-get ov 'excerpt-note-overlay))
                           (overlays-in (point-min) (point-max))))) info)
      (push (format "JIT lock registered: %s"
                   (memq 'excerpt-note--jit-lock-function
                         (bound-and-true-p jit-lock-functions))) info)

      (with-current-buffer (get-buffer-create "*Excerpt Conflicts*")
        (erase-buffer)
        (insert "System Check Results\n")
        (insert "====================\n\n")
        (dolist (item (nreverse info))
          (insert item "\n"))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))

      (message "Check complete. See *Excerpt Conflicts* buffer."))))

(defun excerpt-note-debug ()
  "Show debug info."
  (interactive)
  (let ((excerpts (excerpt-note--find-all-excerpts)))
    (with-current-buffer (get-buffer-create "*Excerpt Debug*")
      (erase-buffer)
      (insert (format "Found %d excerpts:\n\n" (length excerpts)))
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

(defun excerpt-note--setup-auto-refresh ()
  "Setup auto-refresh on save."
  (add-hook 'after-save-hook #'excerpt-note--refresh-overlays nil t))

(defun excerpt-note--setup-jit-lock ()
  "Setup JIT lock."
  (jit-lock-register #'excerpt-note--jit-lock-function))

(defun excerpt-note--teardown-jit-lock ()
  "Remove JIT lock."
  (jit-lock-unregister #'excerpt-note--jit-lock-function))

;;; Utility

;;;###autoload
(defun excerpt-note-open-notes ()
  "Open or create notes file."
  (interactive)
  (unless (memq major-mode '(pdf-view-mode nov-mode))
    (user-error "Not in PDF or EPUB buffer"))
  (let* ((file (if (eq major-mode 'pdf-view-mode)
                  (buffer-file-name)
                (or nov-file-name (buffer-file-name))))
         (note-buffer (excerpt-note--get-note-buffer file)))
    (pop-to-buffer note-buffer
                   '((display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t)))))

;;; Minor mode for Org buffers (rendering, overlays, JIT-lock)

;;;###autoload
(define-minor-mode excerpt-note-mode
  "Minor mode for inline excerpts in Org buffers.
Handles rendering, overlays, and JIT-lock fontification."
  :lighter " Exc"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-o") 'excerpt-note-jump-to-source)
            (define-key map (kbd "C-c e j") 'excerpt-note-jump-to-source)
            (define-key map (kbd "C-c e r") 'excerpt-note--refresh-overlays)
            (define-key map (kbd "C-c e d") 'excerpt-note-debug)
            map)
  (if excerpt-note-mode
      (progn
        (excerpt-note--setup-auto-refresh)
        (excerpt-note--setup-jit-lock)

        (let ((count (excerpt-note--add-overlays)))
          (if (> count 0)
              (message "Excerpt-note mode enabled with %d excerpts" count)
            (message "Excerpt-note mode enabled (no excerpts found)")))

        (when (and (fboundp 'jit-lock-fontify-now)
                   (get-buffer-window))
          (let ((win (get-buffer-window)))
            (jit-lock-fontify-now (window-start win) (window-end win))))

        (run-with-idle-timer 0.1 nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (when excerpt-note-mode
                  (excerpt-note--ensure-overlays-at-point)))))
          (current-buffer)))

    (excerpt-note--teardown-jit-lock)
    (excerpt-note--remove-overlays)
    (remove-hook 'after-save-hook #'excerpt-note--refresh-overlays t)
    (message "Excerpt-note mode disabled")))

;;; Minor mode for Document buffers (PDF/EPUB) - keybinding management

(defvar excerpt-note-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'excerpt-note-insert)
    (define-key map (kbd "i") #'excerpt-note-open-notes)
    (define-key map (kbd "I") #'excerpt-note-find-this-location-in-notes)
    map)
  "Keymap for `excerpt-note-doc-mode'.
Bindings for interaction in PDF and EPUB buffers.")

;;;###autoload
(define-minor-mode excerpt-note-doc-mode
  "Minor mode for excerpt-note keybindings in document buffers (PDF/EPUB).
This mode provides reliable keybindings that won't be overridden by
major mode maps or other minor modes.

\\{excerpt-note-doc-mode-map}"
  :lighter " Exc-Doc"
  :keymap excerpt-note-doc-mode-map
  (if excerpt-note-doc-mode
      (message "Excerpt-note-doc-mode enabled: e=insert, i=notes, I=find-location")
    (message "Excerpt-note-doc-mode disabled")))

;;; Setup functions - now using minor mode instead of local-set-key

;;;###autoload
(defun excerpt-note-setup-pdf ()
  "Setup excerpt-note for pdf-view-mode.
Enables `excerpt-note-doc-mode' for reliable keybindings."
  (excerpt-note-doc-mode 1))

;;;###autoload
(defun excerpt-note-setup-epub ()
  "Setup excerpt-note for nov-mode.
Enables `excerpt-note-doc-mode' for reliable keybindings."
  (excerpt-note-doc-mode 1))

;;;###autoload
(defun excerpt-note-enable ()
  "Enable excerpt-note globally."
  (interactive)
  (add-hook 'pdf-view-mode-hook #'excerpt-note-setup-pdf)
  (add-hook 'nov-mode-hook #'excerpt-note-setup-epub)
  (add-hook 'org-mode-hook
            (lambda ()
              (when-let ((file-name (buffer-file-name)))
                (when (or (string-match-p "\\.notes\\.org$" file-name)
                          (and excerpt-note-directory
                               (string-prefix-p
                                (expand-file-name excerpt-note-directory)
                                (expand-file-name file-name)))
                          (and (excerpt-note--denote-available-p)
                               (string-match-p
                                (regexp-quote (expand-file-name denote-directory))
                                (expand-file-name file-name))
                               (string-match-p
                                (concat "__" excerpt-note-denote-keyword)
                                file-name)))
                  (excerpt-note-mode 1)))))
  ;; Enable in already open PDF/EPUB buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       ((eq major-mode 'pdf-view-mode)
        (unless (bound-and-true-p excerpt-note-doc-mode)
          (excerpt-note-doc-mode 1)))
       ((eq major-mode 'nov-mode)
        (unless (bound-and-true-p excerpt-note-doc-mode)
          (excerpt-note-doc-mode 1))))))
  (message "Excerpt-note enabled globally"))

;;;###autoload
(defun excerpt-note-disable ()
  "Disable excerpt-note globally."
  (interactive)
  (remove-hook 'pdf-view-mode-hook #'excerpt-note-setup-pdf)
  (remove-hook 'nov-mode-hook #'excerpt-note-setup-epub)
  ;; Disable in all currently active buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p excerpt-note-doc-mode)
        (excerpt-note-doc-mode -1))
      (when (bound-and-true-p excerpt-note-mode)
        (excerpt-note-mode -1))))
  (message "Excerpt-note disabled globally"))

;;; Utilities

(defun excerpt-note--conv-page-scroll-percentage (scroll &optional hscroll)
  "Convert SCROLL to percentage."
  (let* ((size (pdf-view-image-size))
         (height (cdr size)))
    (/ (float scroll) height)))

(defun excerpt-note--conv-page-percentage-scroll (percentage)
  "Convert PERCENTAGE to scroll position."
  (let* ((size (pdf-view-image-size))
         (height (cdr size)))
    (floor (* percentage height))))

(provide 'excerpt-note)
;;; excerpt-note.el ends here
