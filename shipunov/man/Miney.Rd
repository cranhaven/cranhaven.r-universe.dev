\name{Miney}
\alias{Miney}
\title{Miney game}
\description{Minesweeper game}

\usage{Miney(n, ucol="#b8ff73", gcol="#f0f0f0", bcol="red", space=0.05, pbombs=0.15)}

\arguments{
\item{n}{Size of the field to play, i.e. n=9 (default) is 9 x 9 field.}
\item{ucol}{Color of unknown cells, default is "law green"}
\item{gcol}{Color of good cells, default is gray}
\item{bcol}{Color of bad cells}
\item{space}{Space between cells}
\item{pbombs}{Proportion of cells with bombs}
}

\details{
Heavily modified from 'Miney::miney()' of Roland Rau. See also the fun::mine_sweeper() function.
}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
\dontrun{
## interactive command:
Miney(3)
}
}

\keyword{misc}
