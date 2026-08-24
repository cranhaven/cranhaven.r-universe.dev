#' Wrapper for kableExtra for consistent `ufs` table styling
#'
#' @param x The dataframe to print
#' @param digits,format,escape,table.attr,lightable_options,html_font,full_width Defaults
#' that are passed to [knitr::kable()]
#' @param print Wther to print the table
#' @param viewer Whether to show the table in the viewer
#' @param kable_classic Whether to call `kable_classic`; otherwise,
#' `kable_styling` is called.
#' @param ... Additional arguments are passed to [knitr::kable()]
#'
#' @return The table, invisibly.
#' @export
#'
#' @examples kblXtra(mtcars);
kblXtra <- function(x,
                    digits = 2,
                    format="html",
                    escape = FALSE,
                    print=TRUE,
                    viewer = FALSE,
                    kable_classic = FALSE,
                    lightable_options = "striped",
                    html_font = "\"Arial Narrow\", \"Source Sans Pro\", sans-serif",
                    full_width = TRUE,
                    table.attr = "style='border:0px solid black !important;'",
                    ...) {

  oldKableViewOption <- getOption("kableExtra_view_html", NULL);
  options(kableExtra_view_html = viewer);
  kbl <-
    knitr::kable(
      x,
      digits = digits,
      format = format,
      escape = escape,
      table.attr = table.attr,
      ...
    );

  if (kable_classic) {
    res <-
      kableExtra::kable_classic(
        kbl,
        lightable_options = lightable_options,
        html_font = html_font,
        full_width = full_width
      );
  } else {
    res <-
      kableExtra::kable_styling(
        kbl,
        bootstrap_options = "condensed",
        html_font = html_font,
        full_width = full_width
      );
  }

  if (print || viewer) {
    print(res);
  }

  if (!is.null(oldKableViewOption)) {
    options(kableExtra_view_html = oldKableViewOption);
  }

  return(invisible(res));

}
