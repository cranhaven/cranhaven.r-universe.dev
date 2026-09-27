#
# Copyright (c) 2010, Stephen B. Weston
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



#' Create an iterator to read binary data from a connection
#'
#' Create an iterator to read binary data from a connection.
#'
#'
#' @param con A connection object or a character string naming a file or a raw
#' vector.
#' @param what Either an object whose mode will give the mode of the vector to
#' be read, or a character vector of length one describing the mode: one of
#' \dQuote{numeric}, \dQuote{double}, \dQuote{integer}, \dQuote{int},
#' \dQuote{logical}, \dQuote{complex}, \dQuote{character}, \dQuote{raw}.
#' Unlike \code{readBin}, the default value is \dQuote{raw}.
#' @param n integer.  The (maximal) number of records to be read each time the
#' iterator is called.
#' @param size integer.  The number of bytes per element in the byte stream.
#' The default, \sQuote{NA_integer_}, uses the natural size.
#' @param signed logical.  Only used for integers of sizes 1 and 2, when it
#' determines if the quantity on file should be regarded as a signed or
#' unsigned integer.
#' @param endian The endian-ness ('\dQuote{big}' or '\dQuote{little}') of the
#' target system for the file.  Using '\dQuote{swap}' will force swapping
#' endian-ness.
#' @param ipos iterable.  If not \code{NULL}, values from this iterable will be
#' used to do a seek on the file before calling readBin.
#' @return An [iteror] reading binary chunks from the connection.
#' @details Originally from the `itertools` package.
#' @examples
#'
#' zz <- file("testbin", "wb")
#' writeBin(1:100, zz)
#' close(zz)
#'
#' it <- ihasNext(ireadBin("testbin", integer(), 10))
#' repeat print(nextOr(it, break))
#' unlink("testbin")
#'
#' @aliases ireadBin
#' @export i_readBin
i_readBin <- function(con, what='raw', n=1L, size=NA_integer_,
                     signed=TRUE, endian=.Platform$endian, ipos=NULL) {
  # Sanity check "n"
  if (!is.numeric(n) || length(n) != 1 || n < 1) {
    stop('n must be a numeric value >= 1')
  }

  # Remember if we had to open this connection
  opened <- if (is.character(con)) {
    con <- file(con, open='rb', encoding="native.enc")
    TRUE
  } else {
    if (!isOpen(con, 'r') || summary(con)$text != 'binary') {
      stop('con must be opened for reading in binary mode')
    }
    FALSE
  }

  if (!is.null(ipos)) {
    if (!isSeekable(con)) {
      stop('ipos cannot be specified unless con is seekable')
    }
    ipos <- iteror(ipos)
  }

  nextOr_ <- function(or) {
    # Check if we've already stopped
    if (is.null(con)) {
      return(or)
    }

    # "local" arguments to readBin that may be modified by "ipos"
    lwhat <- what
    ln <- n
    lsize <- size
    lsigned <- signed
    lendian <- endian

    # Seek on the connection if a position iterator has been specified
    if (!is.null(ipos)) {
      on.exit(if (opened) {
        close(con)
        con <<- NULL
      })
      p <- ipos(or = return(or)) #and close
      on.exit()

      # default value of "origin"
      origin <- 'start'

      if (is.list(p)) {
        # XXX should check for illegal element names in "p"
        # Don't do a "seek" unless a "where" value is specified
        if (!is.null(p$where)) {
          where <- p$where
          if (!is.null(p$origin)) origin <- p$origin
          seek(con, where=where, origin=origin, rw='read')
        }
        if (!is.null(p$what)) lwhat <- p$what
        if (!is.null(p$n)) ln <- p$n
        if (!is.null(p$size)) lsize <- p$size
        if (!is.null(p$signed)) lsigned <- p$signed
        if (!is.null(p$endian)) lendian <- p$endian
      } else {
        where <- p
        seek(con, where=where, origin=origin, rw='read')
      }
    }

    # Read the next "n" items
    d <- readBin(con, what=lwhat, n=ln, size=lsize, signed=lsigned, endian=lendian)

    # Check if we've hit EOF
    if (length(d) == 0) {
      # Close the connection if necessary
      if (opened) {
        close(con)
      }
      con <<- NULL
      or
    } else {
      d
    }
  }

  iteror_internal(nextOr_)
}

#' @export
ireadBin <- i_readBin
