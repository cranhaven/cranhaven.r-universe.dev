#
# Copyright (c) 2012, Stephen B. Weston
# Updated 2023 by Peter Meilstrup
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA



#' Record and replay iterators
#'
#' The \code{record} function records the values issued by a specified
#' iterator to a file or connection object.  The \code{ireplay} function
#' returns an iterator that will replay those values.  This is useful for
#' iterating concurrently over multiple, large matrices or data frames that you
#' can't keep in memory at the same time.  These large objects can be recorded
#' to files one at a time, and then be replayed concurrently using minimal
#' memory.
#'
#'
#' @aliases record ireplay i_replay
#' @param iterable The iterable to record to the file.
#' @param con A file path or open connection.
#' @param ... passed along to `iteror(iterable, ...)`
#' @return NULL, invisibly.
#' @details Originally from the `itertools` package.
#' @examples
#'
#' suppressMessages(library(foreach))
#'
#' m1 <- matrix(rnorm(70), 7, 10)
#' f1 <- tempfile()
#' record(iteror(m1, by='row', chunkSize=3), f1)
#'
#' m2 <- matrix(1:50, 10, 5)
#' f2 <- tempfile()
#' record(iteror(m2, by='column', chunkSize=3), f2)
#'
#' # Perform a simple out-of-core matrix multiply
#' p <- foreach(col=i_replay(f2), .combine='cbind') %:%
#'        foreach(row=i_replay(f1), .combine='rbind') %do% {
#'          row %*% col
#'        }
#'
#' dimnames(p) <- NULL
#' print(p)
#' all.equal(p, m1 %*% m2)
#' unlink(c(f1, f2))
#'
#' @export record ireplay
record <- function(iterable, con, ...) UseMethod("record")

#' @exportS3Method
record.default <- function(iterable, con, ...) {
  record.iteror(iteror(iterable, ...), con=con)
}

#' @exportS3Method
record.iteror <- function(iterable, con, ...) {
  stop_unused(...)
  if (is.character(con)) {
    con <- file(con, 'wb')
    on.exit(close(con))
  }
  repeat {
    serialize(iterable(or = break), con)
  }
  invisible()
}

#' @export
i_replay <- function(con) {
  # Remember if we had to open this connection
  opened <- if (is.character(con)) {
    con <- file(con, open='rb')
    TRUE
  } else {
    FALSE
  }

  nextOr_ <- function(or) {
    # Check if we've already stopped
    if (is.null(con)) {
      return(or)
    }

    tryCatch({
      unserialize(con)
    },
    error=function(e) {
      if (opened) {
        close(con)
      }
      con <<- NULL
      or
    })
  }

  iteror(nextOr_)
}

#' @export
ireplay <- i_replay
