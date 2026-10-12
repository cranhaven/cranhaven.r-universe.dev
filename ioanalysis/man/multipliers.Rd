\name{multipliers}
\alias{multipliers}

\title{Multiplier Analysis
}
\description{\code{multipliers} is currently able to calculate four different multipliers: \code{output}, \code{input}, \code{income}, and \code{employment}. See details for formulas.}

\usage{
multipliers(io, ES, regions = "all", sectors = "all", multipliers, wage.row, 
            employ.closed.row, employ.physical.row)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io}{An \code{InputOutput} class object from \code{\link{as.inputoutput}}}
  \item{ES}{An \code{EasySelect} class object from \code{\link{easy.select}} to specify which region and sector combinations to use.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
  \item{multipliers}{Character. Any combination of the following: \code{output}, \code{input}, \code{wage}, and/or \code{employment}}
  \item{wage.row}{Integer. The row(s) in Value Added where wages is stored. See \code{io$V_label} if you do not know. This is not to be confused with the labor located in the intermediate transaction matrix (\code{Z})}
  \item{employ.closed.row}{Integer. The row(s) in the intermediate transaction matrix (\code{Z}) where labor is stored. This is not to be confused with "wages" or "employee compensation" etc.}
  \item{employ.physical.row}{character or Integer. The row(s) in the phtsical matrix (\code{P}) where labor is stored. This is not to be confused with "wages" or "employee compensation" etc.}
}


\details{There are four different multipliers able to be calculated:

(1) \code{output} - Output multipliers are calculated as the sum over rows from the Leontief matrix:
\deqn{O_j = \sum_{i=1}^n l_{ij}  }
where \eqn{l_{ij}} is the ith row and jth column element of the Leontief matrix.

(2)\code{input} - Input multipliers are calculated as the sum over columns from the Ghoshian matrix:
\deqn{I_i = \sum_{j=1}^n g_{ij}}
where \eqn{g_ij} is the ith row and jth column element of the Ghoshian matrix

(3) \code{wage} - Income multipliers are calculated using value add due to employee compensation or wages. Multiple types of wages are supported. Wages are standardized and multiplied by the Leontief matrix:
\deqn{W_j = \sum_{i=1}^n \omega _i l_{ij} }
where \eqn{\omega _i = w_i/X_i} is the wage divided by the total production for that region-sector combination, and \eqn{l_{ij}} is the ith row and jth column element of the Leontief matrix.

(4) \code{employment} - Employment multipliers are calculated using the employment row in the matrix of technical input coefficients (\code{A}):
\deqn{E_j = \sum_{i=1}^n \epsilon _{ei} l_{ij} }
where \eqn{\epsilon _{ei}} is the row(s) corresponding to labor at the ith column, and \eqn{l_{ij}} is the ith row and jth column element of the Leontief matrix.
}

\value{
Produces a list over regions of multilpliers.
}
\references{
Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})
}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}


\seealso{
\code{\link{as.inputoutput}}, \code{\link{key.sector}}, \code{\link{linkages}}, \code{\link{output.decomposition}}
}

\examples{
data(toy.IO)
class(toy.IO)
M1 <- multipliers(toy.IO, multipliers = "wage", wage.row = 1)
M2 <- multipliers(toy.IO, multipliers = "employment.closed", employ.closed.row = "Minions")

data(toy.ES)
class(toy.ES)
M3 <- multipliers(toy.IO, toy.ES, multipliers = c("input", "output"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

