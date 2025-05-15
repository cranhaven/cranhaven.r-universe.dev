#' Extract comments from DAS data
#'
#' Extract comments from DAS data
#'
#' @param x an object of class \code{das_dfr} or \code{das_df},
#'  or a data frame that can be coerced to a \code{das_dfr} object
#'
#' @details This function recreates the comment strings by
#'   pasting the Data# columns back together for the C events (comments).
#'   See the examples section for how to search for comments with certain phrases
#'
#' @return \code{x}, filtered for C events and with the added column
#'   comment_str containing the concatenated comment strings
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' y.proc <- das_process(y)
#'
#' das_comments(y.proc)
#'
#' # Extract all comments containing "record" - could also use stringr pacakge
#' y.comm <- das_comments(y.proc)
#' y.comm[grepl("record", y.comm$comment_str, ignore.case = TRUE), ]
#'
#' # Join comments with processed data
#' dplyr::left_join(y.proc, y.comm[, c("file_das", "line_num", "comment_str")],
#'                  by = c("file_das", "line_num"))
#'
#' @export
das_comments <- function(x) UseMethod("das_comments")


#' @name das_comments
#' @export
das_comments.data.frame <- function(x) {
  das_comments(as_das_dfr(x))
}


#' @name das_comments
#' @export
das_comments.das_df <- function(x) {
  as_das_df(.das_comments(x))
}


#' @name das_comments
#' @export
das_comments.das_dfr <- function(x) {
  as_das_dfr(.das_comments(x))
}


.das_comments <- function(x) {
  stopifnot(inherits(x, "das_df") | inherits(x, "das_dfr"))

  x.c <- x[x$Event == "C", ]
  x.c.c <- apply(x.c[, paste0("Data", 1:9)], 1, function(i) {
    paste(na.omit(i), collapse = "")
  })

  x.c %>% mutate(comment_str = unname(x.c.c))
}
