\name{epoc.bootstrap}
\alias{epoc.bootstrap}
\alias{plot.bootsize}
\alias{epoc.bootplot}
\alias{epoc.final}
\title{epoc.bootstrap}
\encoding{utf-8}
\description{Bootstrap for the EPoC methods}
\usage{
  epoc.bootstrap(Y, U, nboots=100, bthr=NULL, method='epocG',...)
  \method{plot}{bootsize}(x, lambda.boot=NULL, B, range=c(0,1), ...)
  epoc.final(epocboot, bthr=0.2, k)
}
\arguments{
  \item{Y}{mRNA, samples x genes.}
  \item{U}{CNA, samples x genes.}
  \item{nboots}{Number of bootstrap iterations to run.}
  \item{method}{For \code{epoc.bootstrap} method is \code{"epocG"} or \code{"epocA"}.}
  \item{x}{A sparse network matrix or a list of the same, non-zeros are links. These come from e.g. \code{epoc.final} or \code{epoc.bootstrap}.}
  \item{lambda.boot}{The \eqn{\lambda}s used to run the bootstrap.}
  \item{B}{Number of bootstrap iterations ran for \code{'plot.bootsize'}.}
  \item{range}{Range of bootstrap thresholds to display.}
  \item{epocboot}{For \code{epoc.final} give a list of bootstraped models from \code{epoc.bootstrap}.}
  \item{k}{For \code{epoc.final} and \code{epoc.bootplot} select the \code{k} sparsest model.}
  \item{bthr}{Require presence of links in 100*bthr\% of the bootstrap iterations.}
%  \item{showself}{Show self-regulating loops in plot, N.B. doesn't work as it should!}
%  \item{threed}{Plot the network in 3D}
%  \item{showtitle}{Show the call made to \code{epocA}/\code{epocG} if available in the plot title}
%  \item{layout}{layout, default=NULL means use \code{layout.circle} for 2D and \code{layout.sphere} for 3D plots.}
%  \item{type}{This page together with \link{plot.EPOCG} documents \code{type='graph'}}
  \item{...}{Parameters passed down to an underlying function. For \code{epoc.bootstrap} these are passed down to \code{"epocG"} or \code{"epocA"} respectively. For \code{epoc.bootplot} and \code{plot.bootsize} parameters are passed down to the underlying plot command.}
}
\details{
  \code{epoc.bootstrap} run \code{epocA} or \code{epocG} using bootstrap.
}
\value{
  \code{epoc.bootstrap} returns a list of \eqn{p \times p}{p x p} arrays of values in \eqn{[0,1]} where 1 is presence of link in 100\% of bootstrap iterations for the \eqn{k} different \eqn{\lambda} values for \eqn{p} different genes.
  \code{epoc.final} returns a sparse matrix of \eqn{p \times p}{p x p} values in \eqn{[0,1]} where 1 is presence of link in 100\% of bootstrap iterations, but thresholded such that all values have be greater than or equal to \code{bthr}.
}
\seealso{
  \code{\link{epoc}}, \code{\link{plot.EPoC.validation}}, \code{\link{plot.EPOCA}}, \code{\link{plot.EPOCG}}
}
\references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)
}
\keyword{cancer}
\keyword{random-split}
\keyword{cross-validation}
\keyword{robustness}

