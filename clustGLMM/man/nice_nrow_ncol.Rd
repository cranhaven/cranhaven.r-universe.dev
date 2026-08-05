\name{nice_nrow_ncol}
\alias{nice_nrow_ncol}
\title{
Determine Optimal Matrix Dimensions Given the Number of Cells
}
\description{
Did you ever want to arrange \emph{n} small plots to one big plot of matrix grid? To do so, the easiest way is to set \code{par(mfrow = c(nrow, ncol))}. Once, you can set it manually. But what if you need to do this over and over for different \emph{n} values? The following function proposes \code{nrow} and \code{ncol} such that \code{nrow * ncol >= n} and the remainder \code{nrow * ncol - n} is the smallest possible. 
}
\usage{
nice_nrow_ncol(n, increasing = TRUE, maxsteps = 20, maxratio = 3)
}
\arguments{
  \item{n}{
number of cells to be covered.
}
  \item{increasing}{
a Boolean, if TRUE then the values are returned in increasing (non-decreasing) order, if FALSE then \code{nrow >= ncol}.
}
  \item{maxsteps}{
how many integers away from \code{sqrt{n}} to try.
}
  \item{maxratio}{
what is the maximal ratio between \code{nrow} and \code{ncol}?
}
}
\details{
Function starts at integer \code{ceiling{sqrt{n}}} and for each integer \code{x} between \code{ceiling{sqrt{n}}} and \code{ceiling{sqrt{n}} + maxsteps} finds the lowest integer \code{y} such that \code{x*y >= n}. Among all found pairs of \code{x} and \code{y} that fulfill the condition \code{x/y <= maxratio} it chooses the one that has the smallest remainder \code{x*y - n}. If more pairs achieve the optimum, the pair with the smallest ratio \code{x/y} is chosen. Depending on \code{increasing} the optimal pair \code{c(y,x)} or \code{c(x,y)} is returned. 
}
\value{
Two integers \code{c(nrow, ncol)}.
}
\author{
Jan Vávra
}
\examples{
nice_nrow_ncol(10)
nice_nrow_ncol(11)
nice_nrow_ncol(15)
nice_nrow_ncol(15, increasing = FALSE)

# (2, 17) does not fulfill the condition on maxratio
nice_nrow_ncol(34, maxratio = 3)

# (2, 17) fulfills the condition on maxratio
nice_nrow_ncol(34, maxratio = 10)

# ceiling(sqrt(34)) = 6 but 6+5=11 < 17 and pair (2, 17) is not even considered
nice_nrow_ncol(34, maxsteps = 5, maxratio = 10)
nice_nrow_ncol(34, maxsteps = 11, maxratio = 10)
nice_nrow_ncol(34, maxsteps = 12, maxratio = 10)
}
