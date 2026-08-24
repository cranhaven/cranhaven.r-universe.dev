#' Insert numbered caption
#'
#' These functions can be used to manually insert a numbered caption. These
#' functions have been designed to work well with [setFigCapNumbering()] and
#' [setTabCapNumbering()]. This is useful when
#' inserting figures or tables in an RMarkdown document when you
#' use automatic caption numbering for `knitr` chunks, but are inserting a
#' table or figure that isn't produced in a `knitr` chunk while
#' still retaining the automatic numbering. `insertNumberedCaption()` is the
#' general-purpose function; you will typically only use `insertFigureCaption()`
#' and `insertTableCaption()`.
#'
#' @param captionText The text of the caption.
#' @param captionName The name of the caption; by default, for tables,
#' "`tab.cap`".
#' @param prefix,suffix The prefix and suffix texts; [base::sprintf()] is used
#' to insert the number in the position taken up by `\%s`.
#' @param optionName The name of the option to use to save the number counter.
#' @param resetCounterTo If a numeric value, the counter is reset to that value.
#'
#' @return The caption in a character vector.
#' @export
#' @rdname insertNumberedCaption
#'
#' @examples insertNumberedCaption("First caption");
#' insertNumberedCaption("Second caption");
#' sectionNumber <- 12;
#' insertNumberedCaption("Third caption",
#'                       prefix = paste0("Table ",
#'                                       sectionNumber,
#'                                       ".%s: "));
insertNumberedCaption <- function(captionText = "",
                                  captionName = "fig.cap",
                                  prefix = getOption(paste0(optionName,
                                                            "_prefix"),
                                                     "Figure %s: "),
                                  suffix = getOption(paste0(optionName,
                                                            "_suffix"),
                                                     ""),
                                  optionName = paste0("setCaptionNumbering_", captionName),
                                  resetCounterTo = NULL) {
  ### Get current counter value
  counter <- getOption(optionName,
                       FALSE);
  ### Potentially (re)set counter
  if (!is.numeric(counter)) counter <- 1;
  if (is.numeric(resetCounterTo)) counter <- resetCounterTo;
  ### Compose caption text
  if ((!is.null(prefix)) && (!(is.na(prefix))) && (nchar(prefix > 0))) {
    formattedPrefix <- sprintf(prefix, counter);
  } else {
    formattedPrefix <- "";
  }
  if ((!is.null(suffix)) && (!(is.na(suffix))) && (nchar(suffix > 0))) {
    formattedSuffix <- sprintf(suffix, counter);
  } else {
    formattedSuffix <- "";
  }
  res <-
    paste0(
      formattedPrefix,
      captionText,
      formattedSuffix
    );
  ### Update counter in options
  do.call(options,
          stats::setNames(list(counter + 1),
                          nm = optionName));
  ### Return caption text
  return(res);
}
