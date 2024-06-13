\name{print.OMD}
\alias{print.OMD}
\title{Print Optimal OMD Follow-Up Experiments}
\description{
    Printing method for  lists of class \code{OMD}. It displays the
    best extra-runs according to the OMD criterion together with the correspondent OMD values.
}
\usage{
    \method{print}{OMD}(x, X = FALSE, resp = FALSE, Xcand = TRUE, models = TRUE, nMod = x$nMod,
            digits = 3, verbose=FALSE, ...)
}
\arguments{
  \item{x}{list of class \code{OMD}. Output list of the \code{\link{OMD}} function.}
  \item{X}{logical. If \code{TRUE}, the initial design matrix is printed.}
  \item{resp}{logical If \code{TRUE}, the response vector of initial design is printed.}
  \item{Xcand}{logical. If \code{TRUE}, prints the candidate runs.}
  \item{models}{logical. Competing models are printed if \code{TRUE}.}
  \item{nMod}{integer. Top models to print.}
  \item{digits}{integer. Significant digits to use in the print out.}
  \item{verbose}{logical. If \code{TRUE}, the \code{unclass}-ed \code{x} is displayed.}
  \item{...}{additional arguments passed to \code{print} generic function.}
}
\value{
    The function is mainly called for its side effects. Prints out the selected
    components of the class \code{OMD} objects, output of the \code{OMD} function.
    For example the marginal factors and models posterior probabilities and
    the top OMD follow-up experiments with their corresponding OMD statistic.
    It returns invisible list with the components:
    \item{calc}{Numeric vector with basic calculation information.}
    \item{models}{Data frame with the competing models posterior probabilities.}
    \item{follow-up}{Data frame with the runs for follow-up experiments and
        their corresponding OMD statistic.}
}
\references{
  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

  Meyer, R. D., Steinberg, D. M. and Box, G. E. P. (1996)
  Follow-Up Designs to Resolve Confounding in Multifactor Experiments (with discussion).,
  \emph{Technometrics} \bold{38}(4), 303--332.
  \doi{10.2307/1271297}.

}

\author{Marta Nai Ruscone.}
\seealso{\code{\link{OMD}}, \code{\link{OBsProb}}}


\examples{
library(OBsMD)
data(OBsMD.es5, package="OBsMD")
X <- as.matrix(OBsMD.es5[,1:5])
y <- OBsMD.es5[,6]
es5.OBsProb <- OBsProb(X=X,y=y,blk=0,mFac=5,mInt=2,nTop=32)
nMod <- 26
Xcand <- matrix(c(-1,	-1,	-1, -1,	-1,
1,	-1,	-1,	-1,	-1,
-1,	1,	-1,	-1,	-1,
1,	1,	-1,	-1,	-1,
-1,	-1,	1,	-1,	-1,
1,	-1,	1,	-1,	-1,
-1,	1,	1,	-1,	-1,
1,	1,	1,	-1,	-1,
-1,	-1,	-1,	1,	-1,
1,	-1,	-1,	1,	-1,
-1,	1,	-1,	1,	-1,
1,	1,	-1,	1,	-1,
-1,	-1,	1,	1,	-1,
1,	-1,	1,	1,	-1,
-1,	1,	1,	1,	-1,
1,	1,	1,	1,	-1,
-1,	-1,	-1,	-1,	1,
1,	-1,	-1,	-1,	1,
-1,	1,	-1,	-1,	1,
1,	1,	-1,	-1,	1,
-1,	-1,	1,	-1,	1,
1,	-1,	1,	-1,	1,
-1,	1,	1,	-1,	1,
1,	1,	1,	-1,	1,
-1,	-1,	-1,	1,	1,
1,	-1,	-1,	1,	1,
-1,	1,	-1,	1,	1,
1,	1,	-1,	1,	1,
-1,	-1,	1,	1,	1,
1,	-1,	1,	1,	1,
-1,	1,	1,	1,	1,
1,	1,	1,	1,	1
),nrow=32,ncol=5,dimnames=list(1:32,c("A","B","C","D","E")),byrow=TRUE)
p_omd <- OMD(OBsProb=es5.OBsProb,nFac=5,nBlk=0,nMod=26,
nFoll=4,Xcand=Xcand,mIter=20,nStart=25,startDes=NULL,
top=30)
print(p_omd)
}
\keyword{ design }
