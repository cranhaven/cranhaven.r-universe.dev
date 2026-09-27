# Copyright (c) 2014, Stephen B. Weston
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

#FIXME does this even work? it's not tested or exported.

ireadDouble <- function(con, buflen=16*1024, where=0) {
  opened <- if (is.character(con)) {
    con <- file(con, open='rb')
    TRUE
  } else {
    if (! isOpen(con, 'r') || summary(con)$test != 'binary') {
      stop('con must be opened for reading in binary mode')
    }
    FALSE
  }

  if (where > 0) {
    if (! isSeekable(con)) {
      stop('where cannot be specified unless con is seekable')
    }
    seek(con, where=where, origin='start')
  }

  ibuf <- 0
  buffer <- NULL
  it <- ireadBin(con, what='double', n=buflen)
  isstopped <- FALSE

  nextOr_ <- function(or) {
    # Check if we've already stopped
    if (isstopped)
      return(or)
    if (ibuf >= length(buffer)) {
      on.exit({
        buffer <<- NULL
        it <<- NULL
        isstopped <<- TRUE
        if (opened)
          close(con)
      })

      # Refill our buffer
      ibuf <<- 0
      buffer <<- it(or = return(or))
      on.exit()
    }
    ibuf <<- ibuf + 1
    buffer[ibuf]
  }

  iteror_internal(nextOr_)
}
