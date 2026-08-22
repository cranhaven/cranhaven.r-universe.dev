#' @param replacements The strings to search & replace, as a list of two-element vectors,
#' where the first element in each vector contains a regular expression to search for
#' in the source(s), and the second element contains the replacement (these are passed
#' as `perl` regular expressions; see \code{\link{regex}} for more information).
#' Instead of regular expressions, simple words or phrases can also be entered of
#' course (since those are valid regular expressions).
#' @param rlWarn Whether to let [readLines()] warn, e.g. if files do not end
#' with a newline character.
#'
#' @rdname cleaning_sources
#'
#' @examples exampleSource <-
#' "Do you like icecream?
#'
#'
#' Well, that depends\u2026 Sometimes, when it's..... Nice. Then I do,
#' but otherwise... not really, actually."
#'
#' ### Simple text replacements:
#' cat(search_and_replace_in_source(exampleSource,
#'                                  replacements=list(c("\u2026", "..."),
#'                                                    c("Nice", "Great"))));
#'
#' ### Using a regular expression to capitalize all words following
#' ### a period:
#' cat(search_and_replace_in_source(exampleSource,
#'                                  replacements=list(c("\\.(\\s*)([a-z])", ".\\1\\U\\2"))));
#'
#' @export
search_and_replace_in_source <- function(input,
                                         replacements = NULL,
                                         output = NULL,
                                         preventOverwriting = TRUE,
                                         encoding = "UTF-8",
                                         rlWarn = rock::opts$get(rlWarn),
                                         silent=FALSE) {

  if ((length(input) == 1) && file.exists(input)) {
    res <- readLines(input,
                     encoding=encoding,
                     warn = rlWarn);
  } else {
    res <- input;
  }

  if (is.null(replacements)) {
    stop("You have to specify one or more replacements. Specify them in a list ",
         "containing vectors that each have two elements. The first is the ",
         "regular expression to search for; the second is the replacement. For ",
         "example:\n\n",
         "replacements=list(c('firstStringToLookFor', 'firstReplacement'),\n",
         "                  c('secondStringToLookFor', 'secondReplacement'))\n\n",
         "Also see the examples in the manual, which you can view with:\n\n",
         "?rock::search_and_replace_in_source\n");
  } else {
    for (i in seq_along(replacements)) {
      res <- gsub(replacements[[i]][1],
                  replacements[[i]][2],
                  res,
                  perl=TRUE);
    }
  }

  if (is.null(output)) {
    attr(res, "output") <- "returned";
    return(res);
  } else {
    if (!dir.exists(dirname(output))) {
      stop("The directory specified where the output file '",
           basename(output), "' is supposed to be written ('",
           dirname(output),
           "') does not exist.");
    }
    if (file.exists(output) && preventOverwriting) {
      if (!silent) {
        message("File '",
                output, "' exists, and `preventOverwriting` was `TRUE`, so I did not ",
                "write the 'post-search-replace-source' to disk.");
      }
      attr(res, "output") <- "existed";
    } else {
      con <- file(description=output,
                  open="w",
                  encoding=encoding);
      writeLines(text=res,
                 con=con);
      close(con);
      attr(res, "output") <- "written";
    }
    if (!silent) {
      message("I just wrote a 'post-search-replace-source' to file '",
              output,
              "'. Note that this file may be overwritten if this ",
              "script is ran again (unless `preventOverwriting` is set to `TRUE`). ",
              "Therefore, make sure to copy it to ",
              "another directory, or rename it, before starting to code this source!");
    }
    return(invisible(res));
  }

}
