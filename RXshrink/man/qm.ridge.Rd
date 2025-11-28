\name{qm.ridge}
\alias{qm.ridge}
\title{Restricted (2-parameter) Maximum Likelihood Shrinkage in Regression}
\description{
  Computes and displays TRACEs for a \bold{q}-Shaped shrinkage PATH, including the \bold{m}-Extent
  of shrinkage along that path, most likely under Normal-distribution theory to yield optimal
  reducions in MSE Risk. When rank(X-matrix) = p > 2, the most likely solution on the qm.ridge()
  path will be less likely to have minimal MSE risk than the optimal solution on the p-parameter
  eff.ridge() path. The Hoerl-Kennard "ordinary" ridge path has Shape q=0 within the qm.ridge()
  family.    
}
\usage{
  qm.ridge(form, data, rscale = 1, Q = "qmse", steps = 20, nq = 21,
              qmax = 5, qmin = -5, omdmin = 9.9e-13)
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning: 0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
  \item{Q}{Shape parameter that controls the curvature of the shrinkage path through
   regression-coefficient likelihood space (default = "qmse" implies use the value found 
   most likely to be optimal.)  Use Q = 0 to specify Hoerl-Kennard "ordinary" ridge regression.}
  \item{steps}{Number of equally spaced values per unit change along the horizontal
   m-Extent-of-shrinkage axis for estimates to be calculated and displayed in TRACES
   (default = 20.)}
  \item{nq}{Number of equally spaced values on the lattice of all possible values for
   shrinkage q-Shape between the "qmin" and "qmax" parameter settings (default = 21.)}
  \item{qmax}{Maximum allowed q-Shape (default = +5.)}
  \item{qmin}{Minimum allowed q-Shape (default = -5.)}
  \item{omdmin}{Strictly positive minimum value for one-minus-delta (default = 9.9e-013.)}
}
\details{Traditional qm.ridge() paths cannot be overall-optimal when p > 2 because they are
  restricted to using strictly "monotone" (increasing or decreasing) shrinkage factors. Still,
  the "best" m-Extent of qm-shrinkage is marked by a vertical dashed-line on all 5-types of
  qm.ridge() TRACE displays. Compared to OLS estimates, these shrunken estimates have higher
  likelihood of reduced MSE risk and can be much more stable and reasonable, numerically. On
  the other hand, because only OLS estimates are guaranteed to be minimax when risk is MATRIX
  valued (truly multivariate), no guarantee of an actual reduction in MSE Risk is necessarily
  associated with shrinkage. 
}
\value{ An output list object of class qm.ridge:
  \item{form}{The regression formula specified as the first argument.}
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{p}{Number of regression predictor variables.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{r2}{Numerical value of R-square goodness-of-fit statistic.}
  \item{s2}{Numerical value of the residual mean square estimate of error.}
  \item{prinstat}{Listing of principal statistics.}
  \item{mx}{Matrix containing mean values of X-predictors.}
  \item{crlqstat}{Listing of criteria for maximum likelihood selection of path q-Shape.}
  \item{qmse}{Numerical value of q-Shape most likely to be optimal.}
  \item{qp}{Numerical value of the q-Shape actually used for shrinkage.}
  \item{coef}{Matrix of shrinkage-ridge regression coefficient estimates.}
  \item{risk}{Matrix of MSE risk estimates for fitted coefficients.}
  \item{exev}{Matrix of excess MSE eigenvalues (ordinary least squares minus ridge.)}
  \item{infd}{Matrix of direction cosines for the estimated inferior direction, if any.}
  \item{spat}{Matrix of shrinkage pattern multiplicative delta factors.}
  \item{mlik}{Listing of criteria for maximum likelihood selection of m-Extent-of-shrinkage.}
  \item{sext}{Listing of summary statistics for all m-Extents-of-shrinkage.}
  \item{mClk}{Most-Likely Extent of Shrinkage Observed: best multiple of (1/steps) <= p.} 
  \item{minC}{Minimum Observed Value of Normal-theory -2*log(Likelihood-Ratio).}
  \item{QS}{Was a Mesh-Search for Best q-Shape requested? : 1 => Yes, 0 => No.}
  \item{qML}{Computable only when p=rank=2: True Most-Likely q-Shape.}
  \item{kML}{Computable only when p=rank=2: True Most-Likely k-Factor.}
  \item{dML1}{Computable only when p=rank=2: True Most-Likely Delta[1]-Factor.}
  \item{dML2}{Computable only when p=rank=2: True Most-Likely Delta[2]-Factor.}
  \item{mML}{Computable only when p=rank=2: True Most-Likely m-Extent.}  
}
\references{
  Burr TL, Fry HA. (2005) Biased Regression: The Case for Cautious Application.
  \emph{Technometrics} \bold{47}, 284-296.

  Goldstein M, Smith AFM. (1974) Ridge-type estimators for regression analysis.
  \emph{J. Roy. Stat. Soc. B} \bold{36}, 284-291. (The 2-parameter shrinkage family.)
  
  Obenchain RL. (1975) Ridge Analysis Following a Preliminary Test of the Shrunken
  Hypothesis. \emph{Technometrics} \bold{17}, 431-441. \doi{10.1080/00401706.1975.10489369}
  
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. <doi:10.1214/aos/1176344314>
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108} [Best q-Shape when p = 2.]
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.3. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}}, \code{\link{correct.signs}}, \code{\link{MLtrue}} 
and \code{\link{RXpredict}}.}
\examples{
  data(longley2)
  form <- GNP~GNP.deflator+Unemployed+Armed.Forces+Population+Year+Employed
  rxrobj <- qm.ridge(form, data=longley2)
  rxrobj
  plot(rxrobj)
  str(rxrobj)
}
\keyword{regression}
