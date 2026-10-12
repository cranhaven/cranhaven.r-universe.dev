\name{extraction}
\alias{extraction}

\title{Hypothetical Extraction}
\description{Computes the hypothetical extraction as outlined in Dietzenbacher et al. (1993) and as outlined in Blar and Miller (2009).


Caution: Inverting large matrices will take a long time. Each individual hypothetical extraction requires the inversion of a matrix. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds. 
}

\usage{
extraction(io, ES = NULL, regions = 1, sectors = 1, type = "backward.total",
           aggregate = FALSE, simultaneous = FALSE, normalize = FALSE)
}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{ES}{An \code{EasySelect} class object from \code{\link{easy.select}} to specify which region and sector combinations to use.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
  \item{type}{Character. Any combination of \code{"backward"}, \code{"forward"}, \code{"backward.total"}, and/or \code{"forward.total"}. See details.}
  \item{aggregate}{\code{TRUE} or \code{FALSE}. If \code{TRUE} produces the value of the impact over all sectors. If \code{FALSE} produces the impact for each sector.}
  \item{simultaneous}{\code{TRUE} or \code{FALSE}. Determines whether to extract all specified regions sequentially or simultaneously.}
  \item{normalize}{\code{TRUE} or \code{FALSE}. Whether or not to divide each linkage by total production.}
}
\details{
\code{type}

(1) \code{backward} - Calculates the impact of hypothetically extracting the jth region/sector using the formula
\deqn{X - (I - A_c)^{-1} f}
where \eqn{A_c} is the matrix of technical input coefficients with the jth column replaced by zeros

(2)\code{forward} - Calculates the impact of hypothetically extracting the jth region/sector using the formula
\deqn{X - V (I - B_r)^{-1}}
where \eqn{B_r} is the matrix of technical output coefficients with the jth row replaced by zeros

(3) \code{backward.total} - Calculates the impact of hypothetically extracting the jth region/sector using the formula
\deqn{X - (I - A_{cr})^{-1} f}
where \eqn{A_{cr}} is the matrix of technical input coefficients with the jth column and jth row replaced by zeros except for the diagonal element.

(4) \code{forward.total} - Calculates the impact of hypothetically extracting the jth region/sector using the formula
\deqn{X - V (I - B_{cr})^{-1}}
where \eqn{B_{cr}} is the matrix of technical output coefficients with the jth column and jth row replaced by zeros except for the diagonal element.

\code{aggregate}

If \code{TRUE} multiplies the impact vector by a vector of ones to received the summed value of the impact from hypothetical extraction.

\code{normalize}

If \code{TRUE} each component in the impact vector is divided by the total output of that sector/region combination.
}


\value{Produces a list over regions of a list over type of extraction. If there is only one region and one type, then a matrix is returned. For example, items can be called by using \code{extraction$region$type}.}


\references{
Dietzenbacher Erik & van der Linden Jan A. &  Steenge Alben E.  (1993). The Regional Extraction Method: EC Input-Output Comparisons. Economic Systems Research. Vol. 5, Iss. 2, 1993

Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{John J. P. Wade, Ignacio Sarmiento-Barbieri}


\seealso{\code{\link{as.inputoutput}}, \code{\link{easy.select}}, \code{\link{linkages}}, \code{\link{key.sector}}}

\examples{
data(toy.IO)
class(toy.IO)
E1 <- extraction(toy.IO)

# Using an EasySelect object
data(toy.IO)
class(toy.IO)
E2 <- extraction(toy.IO, toy.ES)
E2$Hogwarts

# Using more options
E3 <- extraction(toy.IO, regions = c(1,2), sectors = c("Wii", "Minions"), 
                 type = c("backward", "backward.total"), aggregate = TRUE)
E3$Hogwarts$backward.total

# Multiple regions and types
E4 <- extraction(toy.IO, type = c("forward","forward.total"), normalize = TRUE)
E4$Hogwarts$forward.total
}















