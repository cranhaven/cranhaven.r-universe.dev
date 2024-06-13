\name{OBsProb}
\alias{OBsProb}
\title{Objective Posterior Probabilities from Bayesian Screening Experiments}
\description{
Objective model posterior probabilities and marginal factor posterior probabilities
from Bayesian screening experiments according to Consonni and Deldossi procedure.
}
\usage{
    OBsProb(X, y, abeta=1, bbeta=1, blk, mFac, mInt, nTop)
}
\arguments{
  \item{X}{Matrix. The design matrix.}
  \item{y}{vector. The response vector.}
  \item{abeta}{First parameter of the Beta prior distribution on model space}
  \item{bbeta}{Second parameter of the Beta prior distribution on model space}
  \item{blk}{integer. Number of blocking factors (>=0). These factors are
      accommodated in the first columns of matrix \code{X}. There are
      \code{ncol(X)-blk} design factors.}
  \item{mFac}{integer. Maximum number of factors included in the models.}
  \item{mInt}{integer <= 3. Maximum order of interactions among factors considered in the models.}
  \item{nTop}{integer <=100. Number of models to print ordered according to the highest posterior probability.}
}
\details{
Model and factor posterior probabilities are computed according to Consonni and Deldossi Objective Bayesian
procedure. The design factors are accommodated in the matrix \code{X} after
\code{blk} columns of the blocking factors. So, \code{ncol(X)-blk} design factors
are considered.
A \acronym{Beta(abeta, bbeta)} distribution is assumed as a prior on model space.
The function calls the \acronym{FORTRAN} subroutine \file{obm} and captures summary results.
The complete output of the \acronym{FORTRAN} code is save in the \file{OBsPrint.out}
file in the working directory. The output is a list of class \code{OBsProb} for which
\code{print}, \code{plot} and \code{summary} methods are available.
}
\value{
Below a list with all output parameters of the \acronym{FORTRAN} subroutine \file{obm}.
The names of the list components are such that they match the original \acronym{FORTRAN}
code. Small letters are used for capturing program's output.
  \item{X}{matrix. The design matrix.}
  \item{Y}{vector. The response vector.}
  \item{N}{integer. Number of runs of the screening experiment.}
  \item{COLS}{integer. Number of design factors.}
  \item{abeta}{integer. First parameter of the Beta prior distribution on model space}
  \item{bbeta}{integer. Second parameter of the Beta prior distribution  on model space}
  \item{BLKS}{integer. Number of blocking factors accommodated in the first
                columns of matrix \code{X}.}
  \item{MXFAC}{integer. Maximum number of factors considered in the models.}
  \item{MXINT}{integer. Maximum interaction order among factors considered in the models.}
  \item{NTOP}{integer. Number of models to print ordered according to the highest posterior probability.}
  \item{mdcnt}{integer. Total number of models evaluated.}
  \item{ptop}{vector. Vector of posterior probabilities of the top \code{ntop} models.}
  \item{nftop}{integer.  Number of factors in each of the top \code{ntop} models.}
  \item{jtop}{matrix. Matrix of the factors' labels
                of the top \code{ntop} models.}
  \item{prob}{vector. Vector of factor posterior probabilities.}
  \item{sigtop}{vector. Vector of residual variances of the top \code{ntop} models.}
  \item{ind}{integer. Indicator variable. \code{ind} is 1 if the \file{obm}
                subroutine exited properly. Any other number correspond to
                the format label number in the \acronym{FORTRAN} subroutine script.}
}
\references{
  Consonni, G. and Deldossi, L. (2016)
  Objective Bayesian Model Discrimination in Follow-up design.,
  \emph{Test} \bold{25}(3), 397--412.
  \doi{10.1007/s11749-015-0461-3}.

  Meyer, R. D., Steinberg, D. M. and Box, G. E. P. (1996)
  Follow-Up Designs to Resolve Confounding in Multifactor Experiments (with discussion).,
  \emph{Technometrics} \bold{38}(4), 303--332.
  \doi{10.2307/1271297}.

}
\author{Laura Deldossi. Adapted for \R by Marta Nai Ruscone.}
\note{
The function is a wrapper to call the \acronym{FORTRAN} subroutine \file{obm},
modification of Daniel Meyer's original program, \file{mbcqp5.f}, for the
application of Objective Bayesian follow-up design.
}
\seealso{
    \code{\link{print.OBsProb}}, \code{\link{plot.OBsProb}}, \code{\link{summary.OBsProb}}.
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
}
\keyword{ design }
