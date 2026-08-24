#' Output R Markdown partial
#'
#' This method can be used to produce an R Markdown (or, more
#' accurately, knitr) partial, that can then easily be included
#' as a child 'document' in R Markdown reports.
#'
#' @param x The object to show.
#' @param headingLevel The level of the Markdown heading to provide; basically
#' the number of hashes ('`#`') to prepend to the headings.
#' @param quiet Passed on to [knitr::knit()] whether it should b
#'  chatty (`FALSE`) or quiet (`TRUE`).
#' @param echoPartial Whether to show the executed code in the R Markdown
#' partial (`TRUE`) or not (`FALSE`).
#' @param partialFile This can be used to specify a custom partial file. The
#' file will have object `x` available.
#' @param ... Passed to the specific method; for the default method, this is
#' passed to the print method.
#'
#' @rdname partial
#' @export partial
# partial <-
#   function (x,
#             headingLevel = 3,
#             quiet=TRUE,
#             echoPartial = FALSE,
#             partialFile = NULL,
#             ...) {
#     UseMethod("partial", x)
#   }

#' @rdname partial
#' @method partial default
#' @export
# partial.default <-
#   function (x,
#             headingLevel = 3,
#             quiet=TRUE,
#             echoPartial = FALSE,
#             partialFile = NULL,
#             ...) {
#     knitr::raw_output(utils::capture.output(print(x, ...)));
#   }
