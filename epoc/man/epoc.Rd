\name{EPoC}
\alias{epoc}
\alias{epocA}
\alias{epocG}
\alias{summary.EPOCA}
\alias{summary.EPOCG}
\alias{predict.EPOCA}
\alias{predict.EPOCG}
\alias{print.EPOCA}
\alias{print.EPOCG}
\alias{coef.EPOCA}
\alias{coef.EPOCG}
\alias{epoc.lambdamax}
\alias{write.sif}
\alias{as.graph.EPOCA}
\alias{as.graph.EPOCG}
\title{EPoC}
\encoding{utf-8}
\description{
  EPoC (Endogenous Perturbation analysis of Cancer)
}
\usage{
epocA(Y, U=NULL, lambdas=NULL, thr=1.0e-10, trace=0, ...)
epocG(Y, U, lambdas=NULL, thr=1.0e-10, trace=0, ...)
epoc.lambdamax(X, Y, getall=F, predictorix=NULL)
as.graph.EPOCA(model,k=1)
as.graph.EPOCG(model,k=1)
write.sif(model, k=1, file="", append=F)
\method{print}{EPOCA}(x,...)
\method{print}{EPOCG}(x,...)
\method{summary}{EPOCA}(object, k=NULL, ...)
\method{summary}{EPOCG}(object, k=NULL,...)
\method{coef}{EPOCA}(object, k=1, ...)
\method{coef}{EPOCG}(object, k=1, ...)
\method{predict}{EPOCA}(object, newdata,k=1,trace=0, ...)
\method{predict}{EPOCG}(object, newdata,k=1,trace=0, ...)
}
\arguments{
  \item{Y}{N x p matrix of mRNA transcript levels for p genes and N samples for epocA and epocG. For \code{epoc.lambdamax} \code{Y} is a multi-response matrix}
  \item{U}{N x p matrix of DNA copy number}
  \item{lambdas}{Non-negative vector of relative regularization parameters for lasso. \eqn{\lambda=0} means no regularization which give dense solutions (and takes longer to compute). Default=NULL means let EPoC create a vector}
  \item{thr}{Threshold for convergence. Default value is 1e-10.  Iterations stop when max absolute parameter change is less than thr}
  \item{trace}{Level of detail for printing out information as iterations proceed.
Default 0 -- no information}
  \item{X}{In \code{epoc.lambdamax} \code{X} is the design matrix, i.e. predictors}
  \item{predictorix}{For \code{epoc.lambdamax} when using a multi-response matrix \code{Y} predictors are set to zero for each corresponding response. \code{predictorix} tells which of the responses that have a corresponding predictor in the network case}
  \item{getall}{Logical. For \code{epoc.lambdamax} get a vector of all inf-norms instead of a single maximum}
%  \item{layout}{Default NULL means layout.circle for 2D and layout.sphere for 3D plot}
%  \item{threed}{Default FALSE means 2D plot, 3D plot requires package \code{\link[rgl]{rgl-package}}}
%  \item{showtitle}{Show the call made to \code{epocA}/\code{epocG} if available in the plot title}
%  \item{showself}{Show self-regulating loops in plot, N.B. doesn't work as it should!}
%  \item{bthr}{Require presence of links in 100*bthr\% of the bootstrap iterations. If this is not a bootstrapped model it will still require values to be larger than this threshold.}
  \item{file}{either a character string naming a file or a connection open for writing. \code{""} indicates output to the console}
  \item{append}{logical. Only relevant if \code{file} is a character string.  If \code{TRUE}, the output is appended to the file.  If \code{FALSE}, any existing file of the name is destroyed}
  \item{model}{Model set from epocA or epocG}
  \item{k}{Select a model of sparsity level k in [1,K]. In \code{summary} default (NULL) means all. In \code{plot} default is first model.}
  \item{newdata}{List of Y and U matrices required for prediction. \code{epocG} requires just U.}
  \item{x}{Model parameter to \code{print} and \code{plot}}
  \item{object}{Model parameter to \code{summary}, \code{coef} and \code{predict}}
%  \item{type}{\code{'graph'} or \code{'fit'}, see \code{\link{modelselPlot}} for fit}
  \item{...}{Parameters passed down to underlying function, e.g. \code{\link{print.default}}.  For \code{epocA} and \code{epocG} \code{\dots} are reserved for experimental options.}
}

\details{
\code{epocA} and \code{epocG} estimates sparse matrices \eqn{A} or \eqn{G} using fast lasso regression from mRNA transcript levels \eqn{Y} and CNA profiles \eqn{U}. Two models are provided, EPoC A where

\deqn{AY + U + R = 0}

and EPoC G where

\deqn{Y = GU + E.}

The matrices \eqn{R} and \eqn{E} are so far treated as noise. For details see the reference section and the manual page of \code{\link{lassoshooting}}.

If you have different sizes of U and Y you need to sort your Y such that the U-columns correspond to the first Y-columns. Example: \code{Y.new <- cbind(Y[,haveCNA], Y[, -haveCNA])}
CHANGES: \code{predictorix} used to be a parameter with a vector of a subset of the variables \code{1:p} of U corresponding to transcripts in Y, Default was to use all which mean that Y and U must have same size.

\code{epoc.lambdamax} returns the maximal \eqn{\lambda} value in a series of lasso regression models such that all coefficients are zero.

\code{plot} if \code{type='graph'} (default) plot graph of model using the \code{Rgraphviz} package
arrows only tell direction, not inhibit or stimulate. If \code{type='modelsel'} see \code{modelselPlot}.
}
\note{
The \code{coef} function returns transposed versions of the matrices \eqn{A} and \eqn{G}.
}
\value{
  \code{epocA} and \code{epocG} returns an object of \code{class} `"epocA"' and `"epocG"' respectively.

  The methods \code{\link{summary}}, \code{\link{print}}, \code{\link{coef}}, \code{\link{predict}} can be used as with other models. \code{coef} and \code{predict} take an extra optional integer parameter \code{k} (default 1) which gives the model at the given density level.
  
An object of these classes is a list containing at least the following components:

\item{coefficients}{list of t(A) or t(G) matrices for the different \eqn{\lambda}s}
\item{links}{the number of links for the different \eqn{\lambda}s}
\item{lambdas}{the \eqn{\lambda}s used for this model}
%\item{fitted.values}{the fitted mean values}
%\item{residuals}{the residuals, i.e. the response minus fitted values}
\item{R2}{R², coefficient of determination}
\item{Cp}{Mallows Cp}
%\item{R2.adj}{adjusted R², coefficient of determination adjusted for number of explanatory terms}
\item{s2}{Estimate of the error variance}
\item{RSS}{Residual Sum of Squares (SSreg)}
\item{SS.tot}{Total sum of squares of the response}
\item{inorms}{the infinity norm of predictors transposed times response for the different responses}
\item{d}{Direct effects of CNA to corresponding gene}
}
\seealso{
  \code{\link{print}}, \code{\link{modelselPlot}},
  \code{\link{epoc.validation}},
  \code{\link{epoc.bootstrap}},
  \code{\link{plot.EPoC.validation.pred}},
  \code{\link{plot.EPoC.validation.W}},
  \code{\link{coef}}, \code{\link{predict}}
}
\author{
  Tobias Abenius
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\examples{
\dontrun{
modelA <- epocA(X,U)
modelG <- epocG(X,U)

# plot sparsest A and G models using the igraph package
# arrows only tell direction, not inhibit or stimulate
par(mfrow=c(1,2))
plot(modelA)
plot(modelG)

# OpenGL 3D plot on sphere using the igraph and rgl packages
plot(modelA,threed=T)

# Write the graph to a file in SIF format for import in e.g. Cytoscape
write.sif(modelA,file="modelA.sif")

# plot graph in Cytoscape using Cytoscape XMLRPC plugin and 
# R packages RCytoscape, bioconductor graph, XMLRPC
require('graph')
require('RCytoscape')
g <- as.graph.EPOCA(modelA,k=5)
cw <- CytoscapeWindow("EPoC", graph = g)
displayGraph(cw)

# prediction
N <- dim(X)[1]
ii <- sample(1:N, N/3)

modelG <- epocG(X[ii,], U[ii,])
K <- length(modelA$lambda) # densest model index index
newdata <- list(U=U[-ii,])
e <- X[-ii,] - predict(modelA, newdata, k=K)
RSS <- sum(e^2)
cat("RMSD:", sqrt(RSS/N), "\n")

}
}
\keyword{cancer}
\keyword{selection}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}

