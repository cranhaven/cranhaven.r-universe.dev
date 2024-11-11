\name{tk.convert}
\alias{tk.convert}
\title{
  RDT file conversion
}

\description{
  This function provides a tcl-tk interface to convert RDT table files into CSV-like files, and to produce basic \code{\linkS4class{track.table}} objects from such files.
}

\usage{
  tk.convert(blocking = FALSE)
}

\arguments{
  \item{blocking}{
    Single logical value, whether to wait for the interface window to be closed before unfreezing the R console. The \code{FALSE} default let you use R and the interface in parallel, the code{TRUE} is used essentially in the stand alone version.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{track.table-class}}
}
