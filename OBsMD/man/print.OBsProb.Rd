\name{print.OBsProb}
\alias{print.OBsProb}
\title{Printing Objective Posterior Probabilities from Bayesian Design}
\description{
    Printing method for lists of class \code{OBsProb}. It prints the posterior
    probabilities of factors and models from the Objective Bayesian procedure.
}
\usage{
    \method{print}{OBsProb}(x, X = TRUE, resp = TRUE, factors = TRUE, models = TRUE,
            nTop, digits = 3, plt = FALSE, verbose = FALSE, Sh= TRUE, CV=TRUE,...)
}
\arguments{
  \item{x}{list. Object of \code{OBsProb} class, output from the \code{\link{OBsProb}} function.}
  \item{X}{logical. If \code{TRUE}, the design matrix is printed.}
  \item{resp}{logical. If \code{TRUE}, the response vector is printed.}
  \item{factors}{logical. If \code{TRUE}, marginal posterior probabilities are printed .}
  \item{models}{logical. If \code{TRUE}, models posterior probabilities are printed.}
  \item{nTop}{integer. Number of the top ranked models to print.}
  \item{digits}{integer. Significant digits to use for printing.}
  \item{plt}{logical. If \code{TRUE}, factor marginal probabilities are plotted.}
  \item{verbose}{logical. If \code{TRUE}, the \code{unclass}-ed list \code{x} is displayed.}
  \item{Sh}{logical. If \code{TRUE}, the Shannon index is printed.}
  \item{CV}{logical. If \code{TRUE}, the coefficient of variation is printed.}
  \item{...}{additional arguments passed to \code{print} function.}
}
\value{
    The function prints out marginal factors and models posterior probabilities.
    Returns invisible list with the components:
    \item{calc}{numeric vector with general calculation information.}
    \item{probabilities}{Data frame with the marginal posterior factor probabilities.}
    \item{models}{Data frame with model posterior probabilities.}
    \item{Sh}{Normalized Shannon heterogeneity index on the posterior probabilities of models}
   \item{CV}{Coefficient of variation of factor posterior probabilities.}
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


}
\author{Marta Nai Ruscone.}
\seealso{
    \code{\link{OBsProb}}, \code{\link{summary.OBsProb}}, \code{\link{plot.OBsProb}}.
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
