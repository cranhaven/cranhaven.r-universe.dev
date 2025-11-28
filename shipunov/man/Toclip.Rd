\name{Toclip}
\alias{Toclip}
\title{Insert content to Linux X11 clipboard}
\description{Insert content to Linux X11 clipboard (uses 'xclip')}

\usage{Toclip(x, sep="\t", row.names=FALSE, col.names=TRUE, ...)}

\arguments{
\item{x}{Data frame}
\item{sep}{Separator, tab by default}
\item{row.names}{FALSE by default}
\item{col.names}{TRUE by default}
\item{...}{Arguments to 'write.table()'}
}

\details{Linux-specific. Inserts data frame to Linux X11 clipboard (not primary or secondary).
Useful for interface with spreadsheets.

Works if 'xclip' utility is already installed.

Alternative with more flexibility: 'clipr' package.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
\dontrun{
aa <- data.frame(1:3) # Linux- (and X11-) specific
Toclip(aa) # then load the content into spreadsheet
}
}

\keyword{utilities}
