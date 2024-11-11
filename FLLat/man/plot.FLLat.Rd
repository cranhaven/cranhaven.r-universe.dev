\name{plot.FLLat}

\alias{plot.FLLat}

\title{Plots Results from the Fused Lasso Latent Feature Model}

\description{Plots either the estimated features or a heatmap of the
  estimated weights from a fitted Fused Lasso Latent Feature (FLLat)
  model (i.e., an object of class \code{FLLat}).}

\usage{
\method{plot}{FLLat}(x, type=c("features","weights"), f.mar=c(5,3,4,2), f.xlab="Probe",
     w.mar=c(3,5,0,2), samp.names=1:ncol(x$Theta), hc.meth="complete", \dots)
}

\arguments{
  \item{x}{A fitted FLLat model.  That is, an object of class
  \code{FLLat}, as returned by \code{\link{FLLat}}.}
  \item{type}{The choice of whether to plot the estimated features
  \eqn{\hat{B}} or a heatmap of the estimated weights
  \eqn{\hat{\Theta}}.  Default is \code{"features"}.}
  \item{f.mar}{The margins for the plot of each estimated feature.}
  \item{f.xlab}{The label for the \eqn{x}-axis for the plot of each
  estimated feature.}
  \item{w.mar}{The margins for the heatmap of the estimated weights.}
  \item{samp.names}{The sample names used to label the columns in the
  heatmap of the estimated weights.}
  \item{hc.meth}{The agglomeration method to be used in the hierarchical
  clustering of the columns of \eqn{\hat{\Theta}}.  See \code{\link{hclust}}.}
  \item{\dots}{Further graphical parameters, for the \code{\link{plot}}
  function when \code{type="features"} and for the \code{\link{image}}
  function when \code{type="weights"}.}
}

\details{
  This function plots the estimated features \eqn{\hat{B}} or a heatmap
  of the estimated weights \eqn{\hat{\Theta}} from a fitted FLLat model.
  The features are plotted in order of decreasing total magnitude, where
  the magnitude is given by
  \eqn{\sum_{l=1}^L\hat{\beta}_{lj}^2}{sum(\hat{\beta}_{lj}^2 from l = 1
  to L)} with \eqn{\hat{\beta}_{lj}} for \eqn{l=1,\ldots,L}{l = 1 to
  L} denoting the \eqn{j}th estimated feature (column of \eqn{\hat{B}}).
  Similarly, the rows of the heatmap of the estimated weights are
  re-ordered in the same way.  The heatmap also includes a dendrogram of
  a hierarchical clustering of the samples based on their estimated
  weights (columns of \eqn{\hat{\Theta}}).
  
  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\references{G. Nowak, T. Hastie, J. R. Pollack and R. Tibshirani.  A
  Fused Lasso Latent Feature Model for Analyzing Multi-Sample aCGH
  Data. \emph{Biostatistics}, 2011, doi: 10.1093/biostatistics/kxr012}

\author{Gen Nowak \email{gen.nowak@gmail.com}, Trevor Hastie, Jonathan
  R. Pollack, Robert Tibshirani and Nicholas Johnson.}

\seealso{
  \code{\link{FLLat}}
}

\examples{
## Load simulated aCGH data.
data(simaCGH)

## Run FLLat for J = 5, lam1 = 1 and lam2 = 9.
result <- FLLat(simaCGH,J=5,lam1=1,lam2=9)

## Plot the estimated features.
plot(result)

## Plot a heatmap of the estimated weights.
plot(result,type="weights")
}
