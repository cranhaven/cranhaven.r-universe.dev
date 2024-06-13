\name{plot.OBsProb}
\alias{plot.OBsProb}
\title{Plotting of Posterior Probabilities from Objective Bayesian Design}
\description{
Method Function for plotting marginal factor posterior probabilities from Objective Bayesian Design.
}
\usage{
    \method{plot}{OBsProb}(x, code = TRUE, prt = FALSE, cex.axis=par("cex.axis"), ...)
}
\arguments{
  \item{x}{list. List of class \code{OBsProb} output from the
        \code{\link{OBsProb}} function.}
  \item{code}{logical. If \code{TRUE} coded factor names are used.}
  \item{prt}{logical. If \code{TRUE}, summary of the posterior probabilities
        calculation is printed.}
  \item{cex.axis}{Magnification used for the axis annotation.
        See  \code{\link{par}}.}
  \item{\dots}{additional graphical parameters passed to \code{plot}.}
}
\details{
    A spike plot, similar to barplots, is produced with a spike for each factor.
    Marginal posterior probabilities are used for the vertical axis.
    If \code{code=TRUE}, \code{X1}, \code{X2}, ... are used to label the factors
    otherwise the original factor names are used.
    If \code{prt=TRUE}, the \code{\link{print.OBsProb}} function is called
    and the marginal posterior probabilities are displayed.
    }
\value{
    The function is called for its side effects. It returns an invisible
    \code{NULL}.
}

\references{
  Box, G. E. P. and Meyer R. D. (1986)
  An Analysis of Unreplicated Fractional Factorials.,
  \emph{Technometrics} \bold{28}(1), 11--18.
  \doi{10.1080/00401706.1986.10488093}.


  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.


  Consonni, G. and Deldossi, L. (2016)
  Objective Bayesian Model Discrimination in Follow-up design.,
  \emph{Test} \bold{25}(3), 397--412.
  \doi{10.1007/s11749-015-0461-3}.

}

\author{Marta Nai Ruscone.}
\seealso{
    \code{\link{OBsProb}}, \code{\link{print.OBsProb}}, \code{\link{summary.OBsProb}}.
}
\examples{
library(OBsMD)
data(OBsMD.es5, package="OBsMD")
X <- as.matrix(OBsMD.es5[,1:5])
y <- OBsMD.es5[,6]
# Using for model prior probability a Beta with parameters a=1 b=1
es5.OBsProb <- OBsProb(X=X,y=y, abeta=1, bbeta=1, blk=0,mFac=5,mInt=2,nTop=32)
print(es5.OBsProb)
summary(es5.OBsProb)
plot(es5.OBsProb)
}
\keyword{ design }
\keyword{ hplot }
