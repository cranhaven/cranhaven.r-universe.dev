\name{epoc.validation}
\alias{epoc.validation}
\title{epoc.validation}
\encoding{utf-8}
\description{Model validation using random split or cross-validation}
\usage{epoc.validation(type=c('pred','concordance'),repl,Y,U,lambdas=NULL,
   method='G',thr=1e-10,trace=0,...)}
\arguments{
  \item{type}{\code{'pred'} for 10-fold CV of prediction error. \code{'concordance'} for random split network concordance using Kendall \eqn{W}.}
  \item{repl}{The number of replicates}
  \item{Y}{mRNA, samples x genes}
  \item{U}{CNA, samples x genes}
  \item{lambdas}{series of relative \eqn{\lambda}s or default=\code{NULL} which means let EPoC choose}
%  \item{predictorix}{a vector of indices that is a subset of the genes to act as predictors, default is all genes}
  \item{method}{\code{'G'} means EPoC G and \code{'A'} means EPoC A.}
  \item{thr}{Threshold for convergence to the LASSO solver}
  \item{trace}{Debug information} 
  \item{...}{Extra parameters passed through to the EPoC solver} 
}
\details{
  In the case of \code{'pred'} assess CV prediction error using 10-fold cross-validation with \code{repl} replicates.
  In the case of \code{'concordance'} assess network concordance using random split and Kendall W with \code{repl} replicates.
}
\value{
  A list of class \code{plot.EPoC.validation.pred} or \code{plot.EPoC.validation.W} respectively.
}
\seealso{
  \code{\link{epoc}}, \code{\link{plot.EPoC.validation}}
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\keyword{cancer}
\keyword{random-split}
\keyword{cross-validation}
\keyword{robustness}

