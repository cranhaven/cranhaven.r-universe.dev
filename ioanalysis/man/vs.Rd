\name{vs}
\alias{vs}


\title{Vertical Specialization}

\description{
Calculates the vertical specialization share of total exports of each sector as described by Hummels et al. (2001), equation 3. Creates a value between zero and one to indicate relative specialization. For each region, a Leontief inverse is calculated. You need a multi-region input-output dataset for \code{vs} to be relevant.

Caution: Inverting large matrices will take a long time. Each individual hypothetical extraction requires the inversion of a matrix. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds.
}
\usage{
vs(io, ES, regions = "all", sectors = "all")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{ES}{An \code{EasySelect} class object from \code{\link{easy.select}} to specify which region and sector combinations to use.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
}
\details{
The vertical specialization share of total exports is calculated as follows:
\deqn{
\frac{vs_r}{X_r^{total}} = \frac{1}{X_r^{total}} A^M_r L_r X_r
}
where \eqn{X_r^{total}} is the total exports for region r, \eqn{A^M_r} is the matrix of technical import coefficients, \eqn{L_r} is the domestic Leontief inverse calculated from the domestic matrix of technical coefficients i.e. \eqn{A_{rr}} not the full \eqn{A} matrix, and \eqn{X_r} is the vector of total exports.
}
\value{
Creates a region list of vs share of total exports.
}
\references{
Hummels, David & Ishii, Jun & Yi, Kei-Mu, 2001. \emph{The nature and growth of vertical specialization in world trade}. Journal of International Economics, Elsevier, vol. 54(1), pages 75-96, June.
}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}




\seealso{
\code{\link{import.coef}}, \code{\link{export.total}}, \code{\link{check.RS}}, \code{\link{leontief.inv}}
}
\examples{
data(toy.IO)
class(toy.IO)
(vs1 <- vs(toy.IO, regions = "all"))
vs1$Hogwarts
sum(vs1$Hogwarts)

data(toy.ES)
class(toy.ES)
vs2 <- vs(toy.IO, toy.ES)
vs2
}

