#' Output report from results
#'
#' This method can be used to format results in a way that can directly be
#' included in a report or manuscript.
#'
#' @param x The object to show.
#' @param headingLevel The level of the Markdown heading to provide; basically
#' the number of hashes ('`#`') to prepend to the headings.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param ... Passed to the specific method; for the default method, this is
#' passed to the print method.
#'
#' @rdname report
#' @export report
report <-
  function (x,
            headingLevel = 3,
            quiet=TRUE,
            ...) {
    UseMethod("report", x)
  }

#' @rdname report
#' @method report default
#' @export
report.default <-
  function (x,
            headingLevel = 3,
            quiet=TRUE,
            ...) {
    res <-
      paste0("\n",
             repStr("#", headingLevel),
             " Results for ", deparse(substitute(x)),
             ":\n\n",
             utils::capture.output(print(x, ...)),
             "\n\n");
    cat(res);
    return(invisible(res));
  }
