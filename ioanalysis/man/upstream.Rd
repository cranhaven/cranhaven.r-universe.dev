\name{upstream}
\alias{upstream}

\title{
Upstreamness - Average Distance from Final Use
}
\description{
Measures upstreamness as in Antras et al. (2012), equation (9) page 5. The value is weakly bounded below by one, where a value close to one indicates it is near its final use on average and a higher value indicates it is further away from final use on average.
}
\usage{
upstream(io, ES, regions = "all", sectors = "all")
}
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{ES}{An \code{EasySelect} class object from \code{\link{easy.select}} to specify which region and sector combinations to use.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
}

\details{
The upstreamness is calculated as follows, where, A is the matrix of technical input coefficients, X is total production, E is exports, and M is imports.
\deqn{d_{ij} = a_{ij} \frac{x_i}{x_i + e_{ij} - m_{ij}}}
\deqn{U = (I - D)^{-1}}
\deqn{u_i = \sum_{j=1}^n U_{ij}}
}
\value{
Produces a list over regions of each region's sectors upstreamness measure.
}
\references{
Pol Antras & Davin Chor & Thibault Fally & Russell Hillberry, 2012. \emph{Measuring the Upstreamness of Production and Trade Flows}. NBER Working Papers 17819, National Bureau of Economic Research, Inc.

}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}
\note{
If the import (M) and/or export (E) is a matrix (i.e. not a nx1 vector) they are summed across region-sector combinations.
}


\seealso{
\code{\link{as.inputoutput}}
}
\examples{
data(toy.IO)
class(toy.IO)
u1 <- upstream(toy.IO)
u1$Hogwarts
}
