\name{irf}
\alias{irf}
\title{Plot item response functions for polynomial IRT models.}
\description{Plot model-implied (and possibly empirical) item response function for polynomial IRT models.}
\usage{ irf(data, bParams, item, plotERF = TRUE, thetaEAP = NULL, 
                 minCut = -3, maxCut = 3, NCuts = 9)
}
\arguments{
 \item{data}{N(subjects)-by-p(items) matrix of 0/1 item response data.}
 \item{bParams}{p(items)-by-9 matrix.  The first 8 columns of the matrix should contain the FMP or FUP polynomial 
 coefficients for the p items.  The 9th column contains the value of k for each item (where the item specific 
 order of the polynomial is 2k+1).}
 \item{item}{The IRF for \code{item} will be plotted.}
 \item{plotERF}{A logical that determines whether to plot discrete values of the empirical response function.}
 \item{thetaEAP}{If \code{plotERF=TRUE}, the user must supply previously calculated eap trait estimates to \code{thetaEAP}.}
 \item{minCut, maxCut}{If \code{plotERF=TRUE}, the program will (attempt to) plot \code{NCuts} points of the 
 empirical response function between trait values of \code{minCut} and \code{maxCut} Default minCut = -3. Default maxCut = 3.}
 \item{NCuts}{Desired number of bins for the empirical response function.}
}
\author{Niels Waller}
\examples{
NSubjects <- 2000
NItems <- 15

itmParameters <- matrix(c(
 #  b0    b1     b2    b3    b4  b5,    b6,  b7,  k
 -1.05, 1.63,  0.00, 0.00, 0.00,  0,     0,  0,   0, #1
 -1.97, 1.75,  0.00, 0.00, 0.00,  0,     0,  0,   0, #2
 -1.77, 1.82,  0.00, 0.00, 0.00,  0,     0,  0,   0, #3
 -4.76, 2.67,  0.00, 0.00, 0.00,  0,     0,  0,   0, #4
 -2.15, 1.93,  0.00, 0.00, 0.00,  0,     0,  0,   0, #5
 -1.25, 1.17, -0.25, 0.12, 0.00,  0,     0,  0,   1, #6
  1.65, 0.01,  0.02, 0.03, 0.00,  0,     0,  0,   1, #7
 -2.99, 1.64,  0.17, 0.03, 0.00,  0,     0,  0,   1, #8
 -3.22, 2.40, -0.12, 0.10, 0.00,  0,     0,  0,   1, #9
 -0.75, 1.09, -0.39, 0.31, 0.00,  0,     0,  0,   1, #10
 -1.21, 9.07,  1.20,-0.01,-0.01,  0.01,  0,  0,   2, #11
 -1.92, 1.55, -0.17, 0.50,-0.01,  0.01,  0,  0,   2, #12
 -1.76, 1.29, -0.13, 1.60,-0.01,  0.01,  0,  0,   2, #13
 -2.32, 1.40,  0.55, 0.05,-0.01,  0.01,  0,  0,   2, #14
 -1.24, 2.48, -0.65, 0.60,-0.01,  0.01,  0,  0,   2),#15
 15, 9, byrow=TRUE)
 
  
ex1.data<-genFMPData(NSubj = NSubjects, bParams = itmParameters, 
                     seed = 345)$data

## compute initial theta surrogates
thetaInit <- svdNorm(ex1.data)

## For convenience we assume that the item parameter
## estimates equal their population values.  In practice,
## item parameters would be estimated at this step. 
itmEstimates <- itmParameters

## calculate eap estimates for mixed models
thetaEAP <- eap(data = ex1.data, bParams = itmEstimates, NQuad = 21, 
                priorVar = 2, 
                mintheta = -4, maxtheta = 4)

## plot irf and erf for item 1
irf(data = ex1.data, bParams = itmEstimates, 
    item = 1, 
    plotERF = TRUE, 
    thetaEAP)

## plot irf and erf for item 12
irf(data = ex1.data, bParams = itmEstimates, 
    item = 12, 
    plotERF = TRUE, 
    thetaEAP)  

}
\keyword{statistics}% at least one, from doc/KEYWORDS

