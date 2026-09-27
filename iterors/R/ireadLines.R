#
# Copyright (c) 2008-2010 Revolution Analytics
# Translated 2013 by Peter Meilstrup
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This function makes iterator makers.  The resulting iterator makers all take
# an optional "count" argument which specifies the number of times the
# resulting iterator should fire.  The iterators are wrappers around functions
# that return different values each time they are called.  All this is done to
# avoid cutting and pasting the same code repeatedly.

#' Iterator over Lines of Text from a Connection
#'
#' Returns an iterator over the lines of text from a connection.  It is a
#' wrapper around the standard \code{readLines} function.
#'
#'
#' @param con a connection object or a character string.
#' @param n integer.  The maximum number of lines to read.  Negative values
#' indicate that one should read up to the end of the connection.  The default
#' value is 1.
#' @param \dots passed on to the \code{readLines} function.
#' @return The line reading iterator.
#' @seealso \code{\link[base]{readLines}}
#' @details Originally from the `iterators` package.
#' @examples
#'
#' # create an iterator over the lines of COPYING
#' it <- ireadLines(file.path(R.home(), "COPYING"))
#' nextOr(it)
#' nextOr(it)
#' nextOr(it)
#'
#' @aliases ireadLines
#' @export i_readLines
i_readLines <- function(con, n=1, ...) {
  if (!is.numeric(n) || length(n) != 1 || n < 1)
    stop('n must be a numeric value >= 1')

  if (is.character(con)) {
    con <- file(con, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  nextOr_ <- function(or) {
    if (is.null(con))
      return(or)

    r <- readLines(con, n=n, ...)
    if (length(r) == 0) {
      if (doClose)
        close(con)
      con <<- NULL
      return(or)
    }
    r
  }

  iteror_internal(nextOr_)
}

#' @export
ireadLines <- i_readLines

#' Iterator over Rows of a Data Frame Stored in a File
#'
#' Returns an iterator over the rows of a data frame stored in a file in table
#' format.  It is a wrapper around the standard \code{read.table} function.
#'
#'
#' @param file the name of the file to read the data from.
#' @param \dots all additional arguments are passed on to the \code{read.table}
#' function.  See the documentation for \code{read.table} for more information.
#' @param verbose logical value indicating whether or not to print the calls to
#' \code{read.table}.
#' @return The file reading iterator.
#' @note In this version of \code{iread.table}, both the \code{read.table}
#' arguments \code{header} and \code{row.names} must be specified.  This is
#' because the default values of these arguments depend on the contents of the
#' beginning of the file.  In order to make the subsequent calls to
#' \code{read.table} work consistently, the user must specify those arguments
#' explicitly.  A future version of \code{iread.table} may remove this
#' requirement.
#' @details Originally from the `iterators` package.
#' @seealso \code{\link[utils]{read.table}}
#' @export i_read.table
#' @aliases iread.table
i_read.table <- function(file, ..., verbose=FALSE) {
  args <- list(...)
  argnames <- names(args)

  # need to do this (at least for now) because the default values for
  # header and row.names depend on the first few lines of the file,
  # which could cause a different number of columns to be returned from
  # the first versus the subsequent calls to read.table
  if (!all(c('header', 'row.names') %in% argnames))
    stop('both header and row.names must be specified in this implementation')

  nrows <- if ('nrows' %in% argnames) args$nrows else 1
  row.names <- args$row.names

  # it doesn't seem to make sense to allow nrows < 1 for the "iterator"
  # version of read.table
  if (!is.numeric(nrows) || length(nrows) != 1 || nrows < 1)
    stop('nrows must be a numeric value >= 1')

  # open the file if necessary and remember to close it
  if (is.character(file)) {
    file <- file(file, open='r')
    doClose <- TRUE
  } else {
    doClose <- FALSE
  }

  # create the call object that we'll use to call read.table
  m <- as.call(c(as.name('read.table'), file='', list(...)))
  m$file <- file
  m$nrows <- nrows  # needed since we use a different default than read.table
  env <- sys.frame(sys.nframe())

  # compute these once rather than repeatedly
  rnlen <- length(row.names)
  gotrownames <- is.character(row.names) && rnlen > 1

  # initialize a few state variables
  first.time <- TRUE
  irow <- 1
  errmsg <- NULL

  nextOr_ <- function(or) {
    if (!is.null(errmsg))
      stop(paste('iterator failed previously:', errmsg), call.=FALSE)

    if (is.null(file))
      return(or)

    if (gotrownames) {
      rem <- rnlen - irow + 1  # remaining strings in row.names
      nrows <<- min(nrows, rem)  # possibly decrease nrows to match row.names

      # there is a problem if nrows is one: we would have to set row.names
      # to a character vector of length one, which is interpreted
      # incorrectly by read.table
      if (nrows > 1)
        m$row.names <<- row.names[seq(irow, length=nrows)]
      else
        m['row.names'] <<- list(NULL)  # we'll fix the row names later
      m$nrows <<- nrows
    }

    # call read.table to actually read the file
    r <- tryCatch({
      # handle the case where we've run out of row names
      if (nrows > 0) {
        if (verbose)
          print(m)
        eval(m, env)
      } else {
        NULL
      }
    },
    error=function(e) {
      # this error is thrown at the end of input sometimes
      # but other times a data frame with no rows is returned
      # (for instance when col.names is specified)
      if (!identical(conditionMessage(e), 'no lines available in input')) {
        if (doClose)
          close(file)
        file <<- NULL
        errmsg <<- conditionMessage(e)
        stop(e)
      }
      NULL
    })

    # set header to FALSE, skip to 0, and col.names to names(r)
    # after the first call to read.table
    if (first.time) {
      first.time <<- FALSE
      m$header <<- FALSE
      m$skip <<- 0
      nms <- names(r)
      if (is.numeric(row.names)) {
        nms <- if (row.names == 1)
          c('', nms)
        else if (row.names >= length(nms))
          c(nms, '')
        else
          c(nms[1:(row.names-1)], '', nms[row.names:length(nms)])
      }
      m$col.names <<- nms
    }

    # check if we're done reading
    if (is.null(r) || nrow(r) == 0) {
      if (doClose)
        close(file)
      file <<- NULL
      return(or)
    }

    if (gotrownames) {
      # fix the row names for this particular case
      if (nrows == 1)
        rownames(r) <- row.names[irow]

      # update the index into row.names
      irow <<- irow + nrows
    }

    r
  }

  iteror_internal(nextOr_)
}

#' @export
iread.table <- i_read.table
