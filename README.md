# excerpt-note.el

A lightweight Emacs package for seamless PDF and EPUB annotation with Org-mode.

## Philosophy

Academic reading often involves complex annotations: multi-paragraph notes, inline images, LaTeX equations, code blocks, and lengthy excerpts. Traditional annotation tools force you to think about structure—where does the note end? How do I separate my thoughts from the source material?

**excerpt-note** eliminates this friction. Each annotation unit has a clear, consistent structure:

```
[Your notes - write freely here]

@p.42 | document.pdf | (42 . 0.35)
[Extracted text from source]
-----
```

The `-----` separator cleanly delimits each unit. Write as much as you want above the marker line—multiple paragraphs, images, equations, whatever your thinking requires. The excerpt lives below the marker, preserving the original text. No need to manage boundaries manually; the structure handles it for you.

## Features

- **Bidirectional navigation**: Jump between PDF/EPUB and notes seamlessly
- **Precise location anchors**: Return to exact scroll positions, not just pages
- **Org-mode native**: Full support for images, LaTeX, tables, and all Org features
- **Smart text extraction**: Automatically merges soft line breaks from PDFs while preserving paragraph structure
- **Smart file resolution**: Works with relative paths, bibliography entries, and configurable search paths
- **Denote & Citar integration**: Automatically links to existing literature notes
- **Visual rendering**: Clean overlay system hides metadata, shows friendly "p.42 →" badges
- **JIT fontification**: Efficient rendering for large note files

## Installation

Requires Emacs 27.1+, [pdf-tools](https://github.com/vedang/pdf-tools), and [nov.el](https://depp.brause.cc/nov.el/).

```elisp
(use-package excerpt-note
  :load-path "path/to/excerpt-note"
  :commands (excerpt-note-enable excerpt-note-disable)
  :config
  (excerpt-note-enable))
```

To disable: `M-x excerpt-note-disable`

This removes all hooks and disables the minor modes in all active buffers.

## Usage

### In PDF/EPUB buffers

| Key | Command | Description |
|-----|---------|-------------|
| `e` | `excerpt-note-insert` | Extract selected text (or create empty anchor) |
| `i` | `excerpt-note-open-notes` | Open/create notes file |
| `I` | `excerpt-note-find-this-location-in-notes` | Jump to notes for current page |

### In Org notes buffer

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-o` | `excerpt-note-jump-to-source` | Jump to source location |
| `C-c e j` | `excerpt-note-jump-to-source` | Same as above |
| `C-c e r` | `excerpt-note--refresh-overlays` | Refresh visual rendering |

## Workflow

1. Open a PDF or EPUB
2. Select text and press `e` to excerpt
3. Notes file opens in another window with cursor ready for your annotations
4. Write your thoughts above the marker line
5. Press `C-c C-o` on any excerpt to jump back to the source

## Configuration

```elisp
;; Where to search for source files
(setq excerpt-note-search-paths
      '("~/Documents/papers/"
        "~/Documents/books/"))

;; Use relative paths for portability
(setq excerpt-note-use-relative-paths t)

;; Integrate with Denote
(setq excerpt-note-use-denote t)
(setq excerpt-note-denote-keyword "excerpt")

;; Integrate with Citar
(setq excerpt-note-use-citar t)

;; Text normalization (enabled by default)
;; Merges soft line breaks from PDF extraction
;; while preserving paragraph boundaries
(setq excerpt-note-normalize-text t)
```

## Note File Structure

Each excerpt unit follows this structure:

```org
Your notes go here. Write as much as you need.

Include images: [[./figures/diagram.png]]

Or LaTeX: $E = mc^2$

Multiple paragraphs are fine.

@p.15 | paper.pdf | (15 . 0.42)
The extracted text from the source document
appears here, preserving original formatting
across multiple lines if needed.
-----

Next excerpt starts after the separator...
```

## Debugging

- `M-x excerpt-note-debug` — Show detected excerpts
- `M-x excerpt-note-check-conflicts` — Check mode status
- `M-x excerpt-note-force-refresh` — Force re-render all overlays
- Set `excerpt-note-debug` to `t` for verbose logging

## License

GPL-3.0
