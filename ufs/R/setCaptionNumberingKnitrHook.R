#' Set a knitr hook for caption numbering
#'
#' Set a knitr hook to automatically number captions for, e.g., figures
#' and tables. `setCaptionNumberingKnitrHook()` is the general purpose
#' function; you normally use `setFigCapNumbering()` or `setTabCapNumbering()`.
#'
#' @param captionName The name of the caption; for example, `fig.cap`
#' or `tab.cap`.
#' @param prefix,suffix The prefix and suffix; any occurrences of
#' `\%s` will be replaced by the number.
#' @param optionName THe name to use for the option that keeps track
#' of the numbering.
#' @param resetCounterTo Whether to reset the counter (as stored in the
#' options), and if so, to what value (set to `FALSE` to prevent resetting).
#'
#' @return `NULL`, invisibly.
#' @rdname setCaptionNumberingKnitrHook
#' @export
#'
#' @examples ### To start automatically numbering figure captions
#' setFigCapNumbering();
#'
#' ### To start automatically numbering table captions
#' setTabCapNumbering();
setCaptionNumberingKnitrHook <- function (captionName = "fig.cap",
                                          prefix = "Figure %s: ",
                                          suffix = "",
                                          optionName = paste0("setCaptionNumbering_",
                                                              captionName),
                                          resetCounterTo = 1) {

  ### Store prefix and suffix
  do.call(options,
          stats::setNames(list(prefix, suffix),
                          nm = paste0(optionName,
                                      c("_prefix",
                                        "_suffix"))));

  ### Reset counter here; not in the knitr hook,
  ### otherwise it's reset every time that's called
  if (is.numeric(resetCounterTo)) {
    counter <- resetCounterTo;
    do.call(options,
            stats::setNames(list(counter),
                            nm = optionName));
  }

  ### Define hook function
  hookFunction <-
    function(options) {

      options[[captionName]] <-
        insertNumberedCaption(
          captionText = options[[captionName]],
          captionName  = captionName,
          prefix = prefix,
          suffix = suffix,
          optionName = optionName,
          resetCounterTo = NULL
        )

      return(options);
    }

  ### Set hook function
  do.call(knitr::opts_hooks$set,
          stats::setNames(list(hookFunction),
                          captionName));

  return(invisible(NULL));
}
