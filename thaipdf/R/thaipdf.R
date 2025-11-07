
# Wrap R Markdown ---------------------------------------------------------



#' Convert to a PDF/LaTeX document with Thai Language Compatibility
#'
#' @description **Thai language** supported conversion of R Markdown to a PDF or LaTeX document.
#' It is a wrapper around [`rmarkdown::pdf_document()`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html).
#'
#' @details To achieve Thai language compatibility for \LaTeX,
#' This function injects preamble fragment of Thai \LaTeX typesetting into the preamble of output \LaTeX via [`includes`](https://pkgs.rstudio.com/rmarkdown/reference/includes.html) argument,
#' and set `latex_engine` to "xelatex".
#'
#' @param thai_font (Character) Name of the Thai font to use. Default font is "TH Sarabun New". It can be any Thai font that installed in your system.
#' @param line_spacing (Numeric) Spacing between each line. Line spacing 1.5 is recommended for Thai language (default).
#' @param ... Arguments to pass to [`pdf_document()`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html). You may supply any valid arguments of [`pdf_document()`] except
#' for `includes` and `latex_engine`.
#'
#'
#' @return An S3 object of class "rmarkdown_output_format" to pass to [`rmarkdown::render()`]
#'
#' @seealso
#' * How to use [`rmarkdown::pdf_document`], please see [official documentation](https://bookdown.org/yihui/rmarkdown/pdf-document.html#other-features).
#' * How to use [thaipdf_book()].
#' @examples
#' \dontrun{
#' library(rmarkdown)
#'
#'  # Simple Conversion
#'  render("input.Rmd", output_format = thaipdf::thaipdf_document())
#'
#'  # Render with Thai font "Laksaman", font size 10pt, enable table of contents
#'  render("input.Rmd",
#'         output_format = thaipdf::thaipdf_document(
#'           thai_font = "Laksaman", # you must have this font in your system
#'           toc = TRUE,
#'           pandoc_args = pandoc_metadata_arg("fontsize", "10pt")
#'         ))
#' }
#' @export
thaipdf_document <- function(thai_font = "TH Sarabun New",
                             line_spacing = 1.5,
                             ...
) {

  # Render a tmp file from template-thai-preamble.tex
  tmp_preamble <- tempfile("thai-preamble-", fileext = ".tex")
  # Write pandoc template to a location with variable substitution
  tmp_preamble <- write_thai_preamble(path_abs = tmp_preamble,
                                      thai_font = thai_font,
                                      line_spacing = line_spacing)

  # Pandoc conversion will fail If I remove temp file when exit
  rmarkdown::pdf_document(
    latex_engine = "xelatex",
    includes = rmarkdown::includes(
      in_header = tmp_preamble,
      before_body = before_body()
    ),
    ...
  )
}

# Wrap Bookdown -----------------------------------------------------------



#' Convert R Markdown to a PDF book with Thai Language Compatibility
#'
#' @description **Thai language** supported conversion of R Markdown to a PDF after resolving the special tokens of **bookdown** (e.g., the tokens for references and labels) to native LaTeX commands.
#' It is a wrapper around [`bookdown::pdf_book()`](https://pkgs.rstudio.com/bookdown/reference/pdf_book.html) with argument `base_format` set to [thaipdf_document()].
#'
#' @param ... Arguments to pass to [`bookdown::pdf_book()`](https://pkgs.rstudio.com/bookdown/reference/pdf_book.html), and then to [thaipdf_document()]. You may supply argument `thai_font` and `line_spacing` in here.
#'
#' @return An S3 object of class "rmarkdown_output_format" to pass to [`rmarkdown::render()`]
#' @seealso
#' * How to use [`bookdown::pdf_book()`], please see [official documentation](https://bookdown.org/yihui/bookdown/latexpdf.html)
#' * How to use [thaipdf_document()].
#'
#' @examples
#' \dontrun{
#'  library(rmarkdown)
#'
#'  # Simple Conversion
#'  render("input.Rmd", output_format = thaipdf::thaipdf_book())
#'
#'  # Render with Thai font "Laksaman" and font size 10pt
#'  render("input.Rmd",
#'         output_format = thaipdf::thaipdf_book(
#'           thai_font = "Laksaman", # you must have this font in your system
#'           pandoc_args = pandoc_metadata_arg("fontsize", "10pt")
#'         ))
#' }
#' @export
thaipdf_book <- function(...) {

  # Check bookdown
  if (!requireNamespace("bookdown")) {
    cli::cli_alert_warning("This function require package {.pkg bookdown} installed.")
    cli::cli_li("To install run {.code install.packages('bookdown')}")
  }

  bookdown::pdf_book(
    base_format = thaipdf::thaipdf_document,
    ...
  )
}
