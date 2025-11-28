\name{Cdate}
\alias{Cdate}
\alias{Ctime}
\alias{Save.history}
\title{System date, time plus easy save history}
\description{System date in 'yyyymmdd' format, system time in 'yyyymmdd_hhmmss' format plus easy save history}

\usage{
Cdate()
Ctime()
Save.history()
}

% \arguments{}

\details{
System date / time in compact formats. These formats are by experience, the most appropriate formats both for file systems and for spreadsheets.

There is also easy 'savehistory' (does not work under macOS R GUI -- but works under macOS 'Terminal.app' R).
}

% \value{}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{savehistory}}}

\examples{
Cdate()
Ctime()
\dontrun{
## does not work under macOS GUI
Save.history()
}
}

\keyword{utilities}
