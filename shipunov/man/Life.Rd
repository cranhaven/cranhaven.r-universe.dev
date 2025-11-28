\name{Life}
\alias{Life}
\title{Game of Life}
\description{Conway's Game of Life}

\usage{Life(n.rows=40, n.cols=40, n.cycles=100, sleep.time=0.12,
 cols=c("#f0f0f0", "#2f81c1"), random=TRUE, rnd.threshold=0.3)}

\arguments{
\item{n.rows}{Number of rows}
\item{n.cols}{Number of columns}
\item{n.cycles}{Number of cycles}
\item{sleep.time}{Time for pause after each cycle}
\item{cols}{Main colors}
\item{random}{If FALSE, runs in the interactive mode}
\item{rnd.threshold}{0 empty board; 1 all squares are filled}
}

\details{

In the interactive mode (random=FALSE), left click to define or remove
cells, then click on red square in the bottom left corner to start
cycles. Click positions are rounded so they are not always precise.

To stop cycles, use Ctrl-C (Linux, macOS) or Esc (Windows) in the main R
window.

The code was inspired by the Github gist (which is not available anymore)
attributed to Vadim Vinichenko. Note that margins influence the behavior
of cells, i.e., the field is not infinite as in the "classic" Game of
Life.

}

% \value{}

\references{

Gardner M. 1970. The fantastic combinations of John Conway's new
solitaire game "life". Scientific American. 223: 120--123.

}

\author{Alexey Shipunov}

% \seealso{}

\examples{

Life(n.cols=10, n.rows=10, n.cycles=10, sleep.time=0.3)

}

\keyword{misc}
