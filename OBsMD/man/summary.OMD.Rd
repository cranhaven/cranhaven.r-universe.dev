\name{summary.OMD}
\alias{summary.OMD}
\title{Summary of Optimal OMD Follow-Up Experiments}
\description{
    Reduced printing method for lists of class \code{OMD}. It displays the
    best extra-runs according to the OMD criterion together with the correspondent OMD value.
}
\usage{
    \method{summary}{OMD}(object, digits = 3, verbose=FALSE, ...)
}
\arguments{
  \item{object}{list of \code{OMD} class. Output list of \code{\link{OMD}} function.}
  \item{digits}{integer. Significant digits to use in the print out.}
  \item{verbose}{logical. If \code{TRUE}, the \code{unclass}-ed \code{object} is displayed.}
  \item{...}{additional arguments passed to \code{summary} generic function.}
}
\value{
    It prints out the marginal factors and models posterior probabilities and
    the top OMD follow-up experiments with their corresponding OMD statistic.}


\references{
  Box, G. E. P. and Meyer, R. D. (1993)
  Finding the Active Factors in Fractionated Screening Experiments.,
  \emph{Journal of Quality Technology} \bold{25}(2), 94--105.
  \doi{10.1080/00224065.1993.11979432}.

  Consonni, G. and Deldossi, L. (2016)
  Objective Bayesian Model Discrimination in Follow-up design.,
  \emph{Test} \bold{25}(3), 397--412.
  \doi{10.1007/s11749-015-0461-3}.

  Meyer, R. D., Steinberg, D. M. and Box, G. E. P. (1996)
  Follow-Up Designs to Resolve Confounding in Multifactor Experiments (with discussion).,
  \emph{Technometrics} \bold{38}(4), 303--332.
  \doi{10.2307/1271297}.

}

\author{Marta Nai Ruscone.}
\seealso{\code{\link{print.OMD}} and \code{\link{OMD}}}


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
summary(p_omd)
}
\keyword{ design }
