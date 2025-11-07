# thaipdf 0.1.2

-   Fix function documentation and examples.

# thaipdf 0.1.1

-   Rename R Markdown template folder from `preTeX` to `pre-tex` for recommended practice of all lower-case letter in sub-directory.

# thaipdf 0.1.0

> Self-contained Thai R Markdown settings, no need for global configuration files. (#6)

## Major changes

### Functions

-   `thaipdf_document()` (also `thaipdf_book()`) accept 2 new arguments:

    -   `thai_font`: for Thai font to use, default font is "TH Sarabun New".

    -   `line_spacing`: for line spacing, reccommended default is 1.5

-   **Defunct** `thaipdf_config_set()` and `thaipdf_config_get()`

These changes enable user to supply `thai_font` or `line_spacing` in the YAML header of R Markdown directly. Therefore, self-contained document setting would facilitate more **reproducible workflow**.

### R Markdown Templates

-   "Thai PDF Bookdown" and "Thai PDF R Markdown" template has an update in YAML header.

# thaipdf 0.0.2.9000

-   Plan to remove `thaipdf_config_set()` and `thaipdf_config_get()` in an upcoming version

-   To enable reproducible workflow, next version will let user specify document setting such as `thai_font` and `line_spacing` in the YAML header of R Markdown.

# thaipdf 0.0.2

-   `use_thai_preamble()`: `name` argument can be a relative path or absolute path.

# thaipdf 0.0.1

-   First release version to [GitHub](https://github.com/Lightbridge-KS?tab=repositories)
-   Added package logo

# thaipdf 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
