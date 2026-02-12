# cardargus 0.2.0

## Breaking changes

### Function renames (tidyverse style)

All functions now follow `snake_case` naming convention. The old function names have been removed.

| Removed function | Use instead |
|----------|----------|
| `house_icon_svg()` | `icon_house()` |
| `building_icon_svg()` | `icon_building()` |
| `construction_icon_svg()` | `icon_construction()` |
| `map_pin_icon_svg()` | `icon_map_pin()` |
| `money_icon_svg()` | `icon_money()` |
| `badge_svg()` | `create_badge()` |
| `badge_row_svg()` | `create_badge_row()` |
| `setup_cardargus_fonts()` | `setup_fonts()` |
| `install_cardargus_fonts()` | `install_fonts()` |
| `cardargus_font_cache_dir()` | `font_cache_dir()` |
| `register_knitr_engine()` | `register_cardargus_knitr()` |

## New features

### Chrome-based rendering

Added headless Chrome support for superior font rendering via the `chromote` package.

* `svg_to_png_chrome()` - Convert SVG to PNG using headless Chrome
* `svg_to_pdf_chrome()` - Convert SVG to vector PDF using headless Chrome
* `chrome_available()` - Check if Chrome/Chromium is available
* `ensure_chrome()` - Check and optionally download Chrome for Testing
* `find_chrome_path()` - Find Chrome executable on the system

### Chrome auto-download

When Chrome is not installed, use `ensure_chrome(download = TRUE)` to 
automatically download "Chrome for Testing" (~150MB). This standalone
Chrome distribution is cached locally and works without system installation.

### Improved CLI output

All user-facing messages now use the `cli` package for better formatting:
- Informative error messages with suggestions
- Progress indicators for downloads
- Structured output with bullets and formatting

### Custom SVG support

Made it clearer that you can use your own SVG files for logos and icons:

```r
# Use any SVG file path for logos
svg_card(
 title = "My Card",
 logos = c("/path/to/logo1.svg", "/path/to/logo2.svg"),
 ...
)

# Or for icons in fields
svg_card(
 title = "My Card",
 fields = list(
   list(list(label = "Name", value = "Test", with_icon = "/path/to/icon.svg"))
 ),
 ...
)
```

### Enhanced knitr functions

All knitr/Quarto functions now support an `engine` parameter:

* `include_card()` - Added `engine` parameter
* `include_card_png()` - Added `engine` parameter
* `save_card_for_knitr()` - Added `engine` parameter
* `card_to_grob()` - Added `engine` parameter

Engine options:
- `"auto"` (default): Uses Chrome if available, otherwise rsvg
- `"chrome"`: Forces Chrome rendering
- `"rsvg"`: Forces rsvg/magick rendering

## Bug fixes

* Fixed duplicate `@font-face` declarations in `embed_svg_fonts()`
* Fixed double font embedding in `svg_to_formats()`
* Now uses modern CSS weight range syntax (`font-weight: 100 900`)
* Removed unused `inst/fonts/` directory

## Deprecated

* `fonts_dir()` - Use `font_cache_dir()` instead
* `custom_logo_svg()` - Use `load_svg_for_embed()` or pass file paths directly

---

# cardargus 0.1.0

## Initial release

### Card Creation
* `svg_card()` - Main function for creating SVG cards with badges, fields, and logos
* Support for Google Fonts via embedded CSS
* Shields.io-style badges with uniform height
* Top-right and bottom-left logo positioning

### Icons
* `icon_house()` - House/home icon
* `icon_building()` - Building icon
* `icon_construction()` - Construction icon
* `icon_map_pin()` - Location pin icon
* `icon_money()` - Money/currency icon

### Logos
* `load_svg_for_embed()` - Load and process external SVG files
* `create_logo_row()` - Create horizontal logo rows
* `get_svg_path()` - Get paths to bundled SVGs
* `list_bundled_svgs()` - List available bundled SVGs

### Export
* `save_svg()` - Save card as SVG file
* `svg_to_png()` - Convert card to PNG
* `svg_to_png_with_margin()` - Convert with margin
* `batch_svg_to_png()` - Batch convert multiple cards

### R Markdown / Quarto Integration
* `include_card()` - Display card inline as SVG
* `include_card_png()` - Display card inline as PNG
* `save_card_for_knitr()` - Save for knitr::include_graphics()
* `register_cardargus_knitr()` - Register custom knitr engine
* `card_to_grob()` - Convert to grid graphical object

### Font Management
* `setup_fonts()` - Quick setup for showtext
* `register_google_font()` - Register Google Fonts
* `font_available()` - Check font availability
* `install_fonts()` - Install fonts locally
