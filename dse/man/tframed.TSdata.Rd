\name{tframed.TSdata}
\alias{tframed.TSdata}
\alias{tframe<-.TSdata}
\alias{tfwindow.TSdata}
\alias{tbind.TSdata}
\alias{trimNA.TSdata}

\alias{window.TSdata}

\title{Specific Methods for tframed Data}
\description{See the generic function description.}
\usage{
    \method{tframed}{TSdata}(x, tf=NULL, names=NULL, ...)
    \method{tframe}{TSdata}(x) <- value
    \method{tfwindow}{TSdata}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
    \method{tbind}{TSdata}(x, d2, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
    \method{trimNA}{TSdata}(x, startNAs=TRUE, endNAs=TRUE)
    \method{window}{TSdata}(x, start=NULL, end=NULL, tf=NULL, warn=TRUE, ...)
}
\arguments{
    \item{x}{See the generic function.}
    \item{tf}{a time frame. See the generic function.}
    \item{value}{a time frame to associate with x.}
    \item{names}{A list with elements input and output which are strings
         passed as names to the default method.}
    \item{start}{See the generic function.}
    \item{startNAs}{See the generic function.}
    \item{end}{See the generic function.}
    \item{endNAs}{See the generic function.}
    \item{d2}{See the generic function.}
    \item{pad.start}{See the generic function.}
    \item{pad.end}{See the generic function.}
    \item{warn}{logical indicating if some warning messages should be suppressed.}
    \item{...}{arguments passed to other functions.}
}
\details{
   The generic function is applied to input and to output data.
   }
\seealso{
    \code{\link[tframe]{tframed}},
    \code{\link[tframe]{tfwindow}},
    \code{\link[tframe]{tbind}},
    \code{\link[tframe]{trimNA}}
}

\concept{DSE}
\keyword{ts}


