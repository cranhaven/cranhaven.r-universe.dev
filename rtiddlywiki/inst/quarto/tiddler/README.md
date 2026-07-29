# TiddlyWiki Quarto Extension

This is a Quarto custom format extension that automatically converts Quarto documents to TiddlyWiki format.

## Installation

This extension is bundled with the `rtiddlywiki` R package.

### Option 1: From Package Installation

```r
# Install rtiddlywiki
install.packages("rtiddlywiki")  # or remotes::install_github("byzheng/rtiddlywiki")

# Copy extension to your project
system(paste(
  "cp -r",
  system.file("quarto/tiddler", package = "rtiddlywiki"),
  "_extensions/"
))
```

### Option 2: Manual Copy

Copy the contents of `inst/quarto/tiddler` from the rtiddlywiki package to your project's `_extensions/tiddler/` directory.

## Usage

In your `.qmd` file YAML header, specify the `tiddler-gfm` format:

```yaml
---
title: "My Document"
format: tiddler-gfm
tiddler:
  remote: false
  preview: false
  tags:
    - example
    - analysis
---
```

Then render normally:

```bash
quarto render my-document.qmd
```

This will:
1. Render to GitHub-flavored markdown
2. Automatically convert to TiddlyWiki JSON format (`.json` file)
3. Optionally upload to TiddlyWiki server if `remote: true`
4. Optionally open in browser if `preview: true` (requires tw-livebridge plugin)

## Configuration

The `tiddler` section supports these options:

- `remote` (boolean, default: false): Upload to TiddlyWiki Node.js server
- `preview` (boolean, default: false): Open tiddler in browser via tw-livebridge
- `host` (string): TiddlyWiki server URL (overrides `TW_HOST` environment variable)
- `overwrite` (boolean, default: false): Overwrite existing tiddlers
- `tags` (array): Tags to add to the tiddler
- `fields` (object): Custom fields (advanced)

## Environment Variables

- `TW_HOST`: Default TiddlyWiki server URL (e.g., "http://localhost:8080")
- `TW_HTTP_X_AUTH_KEY`: Authentication key for remote servers

## Features

- Converts Quarto-rendered markdown to TiddlyWiki JSON format
- Embeds images as base64 data URIs (no external image files needed)
- Preserves TiddlyWiki syntax:
  - Links: `[[Link Text]]`
  - Macros: `<<macro>>`
- Supports uploading directly to TiddlyWiki Node.js server
- Optional live preview via tw-livebridge plugin

## Requirements

- Quarto >= 1.2.0
- R with rtiddlywiki package installed
- TiddlyWiki Node.js server (optional, for remote upload)

## Examples

### Basic Local Generation

```yaml
---
title: "Analysis Report"
format: tiddler-gfm
---
```

Renders and creates `analysis-report.json` in the same directory.

### Upload to Server

```yaml
---
title: "Analysis Report"  
format: tiddler-gfm
tiddler:
  remote: true
  tags:
    - reports
    - 2025
---
```

### With Custom Host and Preview

```yaml
---
title: "Analysis Report"
format: tiddler-gfm
tiddler:
  remote: true
  preview: true
  host: "https://my-wiki.example.com"
  overwrite: true
---
```

## Troubleshooting

If the post-processing doesn't work:

1. Ensure rtiddlywiki is installed: `Rscript -e "library(rtiddlywiki)"`
2. Check that `tiddler-render.R` is executable
3. Verify your YAML syntax (proper indentation)
4. Check the Quarto render log for errors

## See Also

- Main package documentation: `?rtiddlywiki::quarto_to_tiddler`
- TiddlyWiki: https://tiddlywiki.com
- Quarto: https://quarto.org
