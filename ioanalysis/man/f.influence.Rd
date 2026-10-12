\name{f.influence}
\alias{f.influence}

\title{Field of Influence}
\description{Calculates the field of influence. Can handle first to nth order field of influence. Uses the method as Sonis & Hewings 1992. This is a recursive technique, so computation time depends on the size of the data and order of field of influence.

NOTE: If you want to examine a \% productivity shock to a specific region-sector, see \code{\link{inverse.important}}.

}

\usage{f.influence(io, i , j)}

\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{i}{Numeric. The row component(s) of the coefficient(s) of interest}
  \item{j}{Numeric. The column component(S) of the coefficient(s) of interest}
}
\details{
First Order Field of Influence - This is simply the product of the jth column of the Leontief inverse multiplied by the ith row of the Leontief inverse. In matrix notation:
\deqn{F_1[i, j] = L_{.j} L_{i.}}
where \eqn{F} denotes the field of influence, and i and j are scalars

Nth Order Field of Influence - This is a recursive function used to calculate higher order fields of influence. The order cannot exceed the size of the Intermediate Transaction Matrix (Z). I.e. if Z is 20x20, you can only calculate up to the 19th order. The formula is as follows:
\deqn{F_k[(i_1,...,i_k), (j_1,...,j_k)] = \frac{1}{k-1} \sum_{s=1}^k\sum_{r=1}^k (-1)^{s+r+1} l_{i_s,j_r} F_{k-1}[i_{-s}, j_{-r}] }
where F is the field of influence, k is order of influence, l_{ij} is the ith row and jth column element of the Leontief Inverse and -s indicates the sth element has been removed.
}
\value{Returns a matrix of the Field of Influence
}
\references{
Sonis, Michael & Hewings, Geoffrey J.D. (1992), "Coefficient Chang in Input-Output Models: Theory and Applications," Economic Systems Research, 4:2, 143-158 (\url{https://doi.org/10.1080/09535319200000013})

Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}


\seealso{
\code{\link{inverse.important}}
}
\examples{
data(toy.IO)
class(toy.IO)
# First order field of influence on L[3,2]
i <- 3
j <- 2
f.influence(toy.IO, i, j)

# Second order field of influence on L[3,2], L[4,5], L[6, 3], and L[1,10]
i <- c(3, 4, 6,  1)
j <- c(2, 5, 3, 10)
f.influence(toy.IO, i, j)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

