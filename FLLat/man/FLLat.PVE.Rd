\name{FLLat.PVE}

\alias{FLLat.PVE}
\alias{plot.PVE}

\title{Choosing the Number of Features for the Fused Lasso Latent Feature
  Model}

\description{Calculates the percentage of variation explained (PVE) for
  a range of values of \eqn{J} (the number of features) for the Fused
  Lasso Latent Feature (FLLat) model.  Also plots the PVE against \eqn{J},
  which can be used for choosing the value of \eqn{J}.}

\usage{
FLLat.PVE(Y, J.seq=seq(1,min(15,floor(ncol(Y)/2)),by=2), B=c("pc","rand"),
          lams=c("same","diff"), thresh=10^(-4), maxiter=100, maxiter.B=1,
          maxiter.T=1)

\method{plot}{PVE}(x, xlab="Number of Features", ylab="PVE", \dots)
}

\arguments{
  \item{Y}{A matrix of data from an aCGH experiment (usually in the form
  of log intensity ratios) or some other type of copy number data.  Rows
  correspond to the probes and columns correspond to the samples.}
  \item{J.seq}{A vector of values of \eqn{J} (the number of features) for
  which to calculate the PVE.  The default values are every second
  integer between \eqn{1} and smaller of either \eqn{15} or the number
  of samples divided by \eqn{2}.}
  \item{B}{The initial values for the features to use in the FLLat
  algorithm for each value of \eqn{J}.  Can be one of \code{"pc"} (the
  first \eqn{J} principal components of \code{Y}) or \code{"rand"} (a
  random selection of \eqn{J} columns of \code{Y}).  The default is
  \code{"pc"}.}
  \item{lams}{The choice of whether to use the same values of the tuning
  parameters in the FLLat algorithm for each value of \eqn{J}
  (\code{"same"}) or to calculate the optimal tuning parameters for each
  value of \eqn{J} (\code{"diff"}).  When using the same values, the
  optimal tuning parameters are calculated once for the default value of
  \eqn{J} in the FLLat algorithm.  The default is \code{"same"}.}
  \item{thresh}{The threshold for determining when the solutions have
  converged in the FLLat algorithm.  The default is \eqn{10^{-4}}{10^(-4)}.}
  \item{maxiter}{The maximum number of iterations for the outer loop of
  the FLLat algorithm.  The default is \eqn{100}.}
  \item{maxiter.B}{The maximum number of iterations for the inner loop
  of the FLLat algorithm for estimating the features \eqn{B}.  The default is
  \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
  \item{maxiter.T}{The maximum number of iterations for the inner loop
  of the FLLat algorithm for estimating the weights \eqn{\Theta}.  The default
  is \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
  \item{x}{An object of class \code{PVE}, as returned by
  \code{\link{FLLat.PVE}}.}
  \item{xlab}{The title for the \eqn{x}-axis of the PVE plot.}
  \item{ylab}{The title for the \eqn{y}-axis of the PVE plot.}
  \item{\dots}{Further graphical parameters.}
}

\details{
  This function calculates the PVE for each value of \eqn{J} as
  specified by \code{J.seq}.  The PVE is defined to be: \deqn{PVE = 1 -
  \frac{RSS}{TSS}}{PVE = 1 - RSS/TSS} where RSS and TSS denote the
  residual sum of squares and the total sum of squares, respectively.
  For each value of \eqn{J}, the PVE is calculated by fitting the FLLat
  model with that value of \eqn{J}.

  There are two choices for how the tuning parameters are chosen when
  fitting the FLLat model for each value of \eqn{J}.  The first choice,
  given by \code{lams="same"}, applies the \code{\link{FLLat.BIC}}
  function just once for the default value of \eqn{J}.  The resulting
  optimal tuning parameters are then used for all values of \eqn{J} in
  \code{J.seq}.  The second choice, given by \code{lams="diff"}, applies
  the \code{\link{FLLat.BIC}} function for each value of \eqn{J} in
  \code{J.seq}.  Although this second choice will give a more accurate
  measure of the PVE, it will take much longer to run than the first
  choice.

  When the PVE is plotted against \eqn{J}, as \eqn{J} increases the PVE
  will begin to plateau after a certain point, indicating that
  additional features are not improving the model.  Therefore, the value
  of \eqn{J} to use in the FLLat algorithm can be chosen as the point at
  which the PVE plot begins to plateau.
  
  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\value{An object of class \code{PVE} with components:
  \item{PVEs}{The PVE for each value of \eqn{J} in \code{J.seq}.}
  \item{J.seq}{The sequence of \eqn{J} values used.}
  There is a \code{\link{plot}} method for \code{PVE} objects.
}

\references{G. Nowak, T. Hastie, J. R. Pollack and R. Tibshirani.  A
  Fused Lasso Latent Feature Model for Analyzing Multi-Sample aCGH
  Data. \emph{Biostatistics}, 2011, doi: 10.1093/biostatistics/kxr012}

\author{Gen Nowak \email{gen.nowak@gmail.com}, Trevor Hastie, Jonathan
  R. Pollack, Robert Tibshirani and Nicholas Johnson.}

\seealso{
  \code{\link{FLLat}}, \code{\link{FLLat.BIC}}
}

\examples{
## Load simulated aCGH data.
data(simaCGH)

## Generate PVEs for J ranging from 1 to the number of samples divided by 2.
result.pve <- FLLat.PVE(simaCGH,J.seq=1:(ncol(simaCGH)/2))

## Generate PVE plot.
plot(result.pve)
}
