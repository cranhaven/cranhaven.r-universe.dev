\name{OMD}
\alias{OMD}
\title{Objective Model Discrimination (OMD) in Follow-Up Experiments}
\description{
Optimal follow-up experiments to discriminate between competing models. The extra-runs are
 derived from the maximization of the objective model discrimination criterion represented by a weighted average
of Kullback-Leibler divergences between all possible pairs of rival models
}
\usage{
OMD(OBsProb, nFac, nBlk = 0, nMod, nFoll, Xcand, mIter, nStart, startDes, top = 20)
}
\arguments{
  \item{OBsProb}{list. \code{OBsProb} class list. Output list of \code{OBsProb} function.}
  \item{nFac}{integer. Number of factors in the initial experiment.}
  \item{nBlk}{integer >=0. Number of blocking factors in the initial experiment.
                They are accommodated in the first columns of matrix \code{X}.}
  \item{nMod}{integer. Number of competing models considered to compute \code{OMD}.}
  \item{nFoll}{integer. Number of additional runs in the follow-up experiment.}
  \item{Xcand}{matrix. Matrix [\code{2^nFac x (nBlk + nFac)}] of candidate runs for the follow-up design.
  It generally rapresents the full \code{2^nFac} design.}
  \item{mIter}{integer >=0. Maximum number of iterations in the exchange algorithm.
  If \code{mIter = 0} exachange algorithm doesn't work.}
  \item{nStart}{integer. Number of different designs of dimension \code{nFoll}
   to be evaluated by \code{OMD} criterion.
  When exchange algorithm is used \code{nStart} represents the number of random starts
  to initialize the algorithm; otherwise \code{nStart = nrow(startDes)}.}
  \item{startDes}{matrix. Input matrix [\code{nStart x nFoll}] containing different
  \code{nStart} designs to be evaluated by \code{OMD} criterion.
  If the exchange algorithm is used \code{startDes = NULL}.}
   \item{top}{integer. Number of highest OMD follow-up designs recorded.}
}
\details{
The OMD criterion, proposed by Consonni and Deldossi, is used to discriminate
among competing models. Random starting runs chosen from \code{Xcand} are used
for the Wynn search of best OMD follow-up designs. \code{nStart} starting points are
tried in the search limited to \code{mIter} iterations. If \code{mIter=0} then
\code{startDes} user-provided designs are used. Posterior probabilities and residual
variances of the competing models are obtained from \code{\link{OBsProb}}.
The function calls the \acronym{FORTRAN} subroutine \file{omd} and captures
summary results. The complete output of the \acronym{FORTRAN} code is save in
the \file{MDPrint.out} file in the working directory.
}
\value{
Below a list with all input and output parameters of the \acronym{FORTRAN}
subroutine \code{OMD}. Most of the variable names kept to match \acronym{FORTRAN} code.
    \item{NSTART}{integer. Number of different designs of dimension \code{nFoll}
   to be evaluated by \code{OMD} criterion.
  When exchange algorithm is used \code{nStart} represents the number of random starts
  to initialize the algorithm; otherwise \code{nStart = nrow(startDes)}.}
    \item{NRUNS}{integer. Number \code{nFoll} of runs used in follow-up designs.}
    \item{ITMAX}{integer. Maximum number \code{mIter} of iterations in the exchange algorithm.}
    \item{INITDES}{integer. Indicator variable. If \code{INITDES = 1} exachange
    alghoritm is used, otherwise \code{INITDES = 0} exachange alghoritm doesn't work.}
    \item{N0}{integer. Numbers of runs \code{nrow(X)} of the initial experiment before follow-up.}
    \item{X}{matrix. Matrix from initial experiment (\code{nrow(X)}; \code{ncol(X)=nBlk+nFac}).}
    \item{Y}{double. Response values from initial experiment (\code{length(Y)=nrow(X)}).}
    \item{BL}{integer >=0. The number of blocking factors in the initial experiment.
                They are accommodated in the first columns of matrix \code{X} and \code{Xcand}.}
    \item{COLS}{integer. Number of factors \code{nFac}.}
    \item{N}{integer. Number of candidate runs \code{nrow(Xcand)}.}
    \item{Xcand}{matrix. Matrix [\code{2^nFac x (nBlk + nFac)}] candidate runs for the follow-up design.
  It generally represents the full \code{2^nFac} design [\code{nrow(Xcand)=N}, \code{ncol(Xcand)=ncol(X)}].}
     \item{NM}{integer. Number of competing models \code{nMod} considered to compute \code{OMD} .}
     \item{P}{double. Models posterior probability \code{optop}. It derives from the \code{OBsProb} output.}
     \item{SIGMA2}{double. Competing models residual variances \code{osigtop}. It derives from the \code{OBsProb} output.}
     \item{NF}{integer. Number of main factors in each competing models \code{onftop}. It derives from the \code{OBsProb} output.}
     \item{MNF}{integer. Maximum number of factor in models (\code{MNF=max(onftop)}).}
     \item{JFAC}{matrix. Matrix \code{ojtop} of dimension [\code{nMod x max(onftop)}] of the labels of the main factors present in
                each competing models. It derives from the \code{OBsProb} output.}
     \item{CUT}{integer. Maximum order of the interaction among factors in the models \code{mInt}.}
     \item{MBEST}{matrix. If \code{INITDES=0}, the first row of the \code{MBEST[1,]} matrix
        has the first user-supplied starting design. The last row the \code{NSTART}-th user-supplied starting design.}
    \item{NTOP}{integer. Number of the top best OMD designs \code{top}.}
    \item{TOPD}{double. The OMD value for the best top \code{NTOP} designs.}
    \item{TOPDES}{matrix. Top \code{NTOP} optimal OMD follow-up designs.}
    \item{flag}{integer. Indicator = 1, if the \file{md} subroutine finished properly,
        -1 otherwise.}
}

\note{
The function is a wrapper to call the modified \acronym{FORTAN} subroutine \file{omd},
 \file{OMD.f}, part of the
\pkg{mdopt} bundle for Bayesian model discrimination of multifactor
experiments.
}
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

\author{Laura Deldossi. Adapted for \R by Marta Nai Ruscone.}
\seealso{
    \code{\link{print.OMD}}, \code{\link{OBsProb}}
}
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
p_omd <- OMD(OBsProb=es5.OBsProb,nFac=5,nBlk=0,nMod=26,nFoll=4,Xcand=Xcand,
mIter=20,nStart=25,startDes=NULL,top=30)
print(p_omd)
}
\keyword{ design }
