#' Turn a nested list into a data.frame
#'
#' A recombinator attempts to convert a depth 2 nested list into
#' a \code{data.frame}.
#'
#' There are two supported formats.
#'
#' \enumerate{
#'   \item{Homogeneous lists}{ A list where the first list element
#'     is a character vector giving the names of the data.frame,
#'     and the subsequent list elements themselves lists of values.}
#'   \item{Heterogeneous lists}{ A list where each element is a named
#'     list of values. In this format, \code{plyr::rbind} will be used
#'     to take the union of all names and impute the ones missing
#'     with \code{NA} values.}
#' }
#'
#' @param dat list. The list of lists to process. It can be in
#'     homogeneous or heterogeneous format (see the description).
#' @param id character. Primary key, by default \code{"id"}.
#' @export
#' @return the converted data.frame. If not a list, no changes will
#'     be performed.
#' @note A warning will be issued if non-standard names (i.e.
#'     those containing more than alphanumeric, underscore, and period
#'     characters) are used.
recombinator <- function(dat, id = "id") {
  if (!is.list(dat) || is.data.frame(dat)) {
  dat
  } else if (is_heterogeneous(dat)) {
    heterogeneous_recombinator(dat, id)
  } else if (is_homogeneous(dat)) {
    homogeneous_recombinator(dat, id)
  } else {
    stop("Invalid recombinator format: pass either (1) ",
         "a list whose first element is a character vector of names ",
         "and the subsequent list elements are unnamed lists of values ",
         "or (2) a list each of whose elements are named lists.", call. = FALSE)
  }
}


#' Is this heterogeneous data?
#' @param dat list. The list to verify.
#' @return boolean. TRUE if the list is heterogeneous, FALSE otherwise.
is_heterogeneous <- function(dat) {
  is.list(dat[[1L]]) && (has_names(dat) || has_names(dat[[1L]]))
}

#' Is this homogeneous data?
#' @param dat list. The list to verify.
#' @return boolean. TRUE if the list is heterogeneous, FALSE otherwise.
is_homogeneous <- function(dat) {
  if (is_heterogeneous(dat)) { FALSE }
  else if (is.character(dat[[1L]]) && !("list" %in% sapply(dat, class))) { TRUE }
  else if (length(dat[[1L]]) > 1) { TRUE }
  else { FALSE }
}


#' Checks if a list has names.
#'
#' @param dat list. The list to verify.
#' @return boolean. TRUE if the list is named, FALSE otherwise.
has_names <- function(dat) {
  !is.null(names(dat)) && all(nzchar(names(dat)))
}

#' Process homogeneous batch data.
#'
#' This function turns a list of data obtained from the Avant API in
#' homogeneous format into a \code{data.frame}. Here, homogeneous refers
#' to a list of lists with the first element of the list being a character
#' vector of column names, and subsequent list elements being lists of
#' values in the correct order and of the same length as the names vector.
#'
#' For example,
#'  \code{list(c('variable_one', 'variable_two'), list(1, 'a'), list(2, 'b'))}
#' refers to a data set with two variables with two rows, the first variable
#' having \code{c(1,2)} and the latter having \code{'a', 'b'}.
#'
#' If the list of lists is not formatted in this way, the function performs
#' no error handling and will likely return a malformed \code{data.frame}.
#'
#' @param dat list. The list of lists to process. The first list element is
#'   a character vector of variable names, and subsequent elements are lists
#'   of variable values ordered by these variable names.
#' @inheritParams recombinator
#' @return the formatted \code{data.frame}
#' @export
#' @examples
#' pre_dataframe <- list(c('variable_one', 'variable_two'), list(1, 'a'), list(2, 'b'))
#' df <- homogeneous_recombinator(pre_dataframe)
#' # 2 by 2 dataframe w/ c(1,2), c('a','b') in the columns, respectively.
homogeneous_recombinator <- function(dat, id = "id") {
  warn_on_nonstandard_names(dat)
  predf <- lapply(seq_along(dat[[1]]), function(.) vector("list", length(dat) - 1))
  for (row_ix in seq_len(length(dat) - 1)) {
    for (col_ix in seq_along(dat[[1]])) {
      predf[[col_ix]][[row_ix]] <-
        if (is.null(tmp <- dat[[row_ix + 1]][[col_ix]])) NA else tmp
    }
  }

  newdat <- stats::setNames(lapply(predf, unlist), dat[[1]])
  predf  <- data.frame(newdat, stringsAsFactors = FALSE)
  if (id %in% names(predf)) {
    predf[[id]] <- as.numeric(predf[[id]])
  }
  predf
}

#' Process heterogeneous batch data.
#'
#' This function turns a list of data obtained from the Avant API in
#' heterogeneous format into a \code{data.frame}. Here, heterogeneous refers
#' to a list of lists with each element being of possibly different size,
#' but a complete named list of the data for that row.
#'
#' For example,
#'  \code{list(list(variable_one = 1, variable_two = 'a'),
#'             list(variable_one = 2, variable_three = 1))}
#' refers to a data set with three variables with two rows, the first variable
#' having \code{c(1,2)}, the second \code{c('a', NA)}, and the third \code{c(NA, 1)}.
#'
#' If the list of lists is not formatted in this way, the function performs
#' no error handling and will likely return a malformed \code{data.frame}.
#'
#' @param dat list. The list of lists to process. Each row is a named list
#'   with the names being variable names and the values being respective
#'   variable values.
#' @inheritParams recombinator
#' @return the formatted \code{data.frame}
#' @export
#' @examples
#' pre_dataframe <-
#'   list(list(variable_one = 1, variable_two = 'a'),
#'        list(variable_one = 2, variable_three = 1))
#' df <- heterogeneous_recombinator(pre_dataframe)
#' # 3 by 2 dataframe w/ c(1,2), c('a', NA), c(NA, 1) in the columns, respectively.
heterogeneous_recombinator <- function(dat, id = "id") {
  if (all(sapply(ns <- names(dat), function(x) is.character(x) && nchar(x) > 0)) &&
      length(ns) > 0) {
    dat <- lapply(dat, function(x) unlist(lapply(x, function(y)
      if (is.null(y)) NA else y)))
    warn_on_nonstandard_names(dat)
    return(data.frame(dat, stringsAsFactors = FALSE))
  }

  if (!any(vapply(dat, is.list, logical(1)))) {
    dat <- list(dat) # Really just one row
  }

  predf <- stats::setNames(
    lapply(seq_along(dat[[1]]), function(.) vector("list", length(dat)))
  , names(dat[[1]]))
  for (row_ix in seq_along(dat)) {
    for (col_name in names(dat[[row_ix]])) {
      if (!col_name %in% names(predf)) {
        predf[[col_name]] <- vector("list", length(dat))
      }

      predf[[col_name]][[row_ix]] <-
        if (is.null(tmp <- dat[[row_ix]][[col_name]])) NA
        else tmp
    }
  }

  for (i in seq_along(predf)) {
    for (j in seq_along(predf[[i]])) {
      if (is.null(predf[[i]][[j]])) {
        predf[[i]][[j]] <- NA
      }
    }
  }

  newdat <- lapply(predf, unlist)
  warn_on_nonstandard_names(newdat)
  predf <- data.frame(newdat, stringsAsFactors = FALSE)
  ids <- grepl("_id$", names(predf))
  predf[ids] <- lapply(predf[ids], as.numeric)
  predf
}

#' Warn if names will be changed when converting to a data.frame.
#'
#' @param data list. A list to convert to a data.frame.
#' @return Nothing, but a \code{warning} if the names will be
#'    mangled due to R's \code{\link{make.names}}.
warn_on_nonstandard_names <- function(data) {
  if (!identical(names(data), make.names(names(data))) && !is.null(names(data))) {
    diffs <- names(data)[names(data) != make.names(names(data))]
    diffs <- vapply(diffs, function(x) paste0(" * ", crayon::yellow(x)), character(1))
    warning(crayon::red(paste0("Some variable names will be destroyed while ",
      "converting to a data.frame, for example: ")), "\n",
      paste(diffs, collapse = "\n"), "\n\n")
  }
}
