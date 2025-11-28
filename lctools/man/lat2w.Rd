\name{lat2w}
\alias{lat2w}
\title{Contiguity-based weights matrix for a regular grid}
\description{
This function creates a contiguity-based (Rook or Queen) weights matrix for a regular grid with equal number of rows and columns}
\usage{
lat2w(nrows=5, ncols=5, rook=TRUE)
}
\arguments{
  \item{nrows}{number of rows}
  \item{ncols}{number of columns (identical to the number of rows)}
  \item{rook}{a TRUE/FALSE option. TRUE refers to a rook contiguity and FALSE to queen contiguity}
}
\details{
This function may also serve in simulations.
}

\value{Returns a list of neighbours for each cell of the grid as well as a weights matrix.
  \item{nbs}{a list of neighbours for each observation}
  \item{w}{a matrix of weights}
}

\references{
Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}
}
\author{Stamatis Kalogirou <stamatis.science@gmail.com}

\seealso{
 \code{\link{w.matrix}}, \code{\link{moransI.w}}, \code{\link{spGini.w}}
}
\examples{
#rook weights matrix for a 5 by 5 grid
w.mat <- lat2w(nrows=5, ncols=5)
}
\keyword{Spatial Analysis}
\keyword{Regular Grid Weights Matrix}
