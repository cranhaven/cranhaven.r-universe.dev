\name{RXshrink-package}
\alias{RXshrink-package}
\docType{package}
\title{Maximum Likelihood (ML) Shrinkage using Generalized Ridge or Least Angle Regression}
\description{The functions in this package augment basic calculations of Generalized Ridge and
  Least Angle Regression plus Visual Insights from five types of ridge TRACE display: [1] regression
  coefficients, [2] relative MSE risk, [3] excess eigenvalues, [4] inferior direction cosines, and [5]
  shrinkage delta-factors. These TRACEs reveal the primary Effects of Shrinkage along ridge paths with
  1, 2 or more parameters: shrinkage m-Extent, path q-Shape, and the p shrinkage delta-factors applied
  to the uncorrelated components of the Ordinary Least Squares estimator. All paths start at the OLS
  estimate [m = 0] and end at the shrinkage Terminous, (0, 0, ..., 0), where m = p = rank of the
  centered and rescaled X-matrix. Three different measures of overall Likelihood of minimal MSE risk
  (Classical Normal-Theory, Empirical Bayes, and Random Coefficients) are monitored to suggest an
  optimal m-Extent of shrinkage for the given matrix of non-constant x-Variables and the observed
  y-Outcome vector.  
}
\details{
  \tabular{ll}{
  Package: \tab RXshrink\cr
  Type: \tab Package\cr
  Version: \tab 2.3\cr
  Date: \tab 2023-08-15\cr
  License: \tab GNU GENERAL PUBLIC LICENSE, Version 2, June 1991\cr
  }
  
  The eff.ridge() function calculates generalized ridge TRACE statistics for the Efficient Shrinkage
  PATH with p > 1 parameters. This PATH always passes through the Beta coefficient point-estimate that
  is most likely to achieve optimal MSE risk reductions under normal distribution-theory. This PATH is
  as Short as Possible; it consists of a Two-Piece Linear function with its single "interior" Knot at
  the MSE Risk Optimal m-Extent of Shrinkage. 
  
  MLboot(), MLcalc() and MLhist() support use of Bootstrap resampling to study both the Bias and the
  MSE Risk characeristics of non-linear (unrestricted) Generalized Ridge Regression (GRR) estimators.
  
  When true regression parameters have user-specified (KNOWN) numerical values, MLtrue() uses this
  information and generates a new data.frame that contains a y-Outcome vector of the expected form
  with "disturbance" terms that are I.I.D. Normal errors-in-measurement. Arguments to MLtrue() must
  include the "formula" for a desired linear model and a data.frame containing the specified
  X-variables.
  
  qm.ridge() calculates and displays TRACEs for traditional PATHs defined by just 2-parameters:
  \bold{q}-Shape and \bold{m}-Extent of Shrinkage. By default, the search for the Path with most
  likely \bold{q}-Shape uses a lattice of only 21 values within [-5,+5]. However, lattice searches for
  both q-Shape and m-Extent are easy to modify using the qmax, qmin, nq and steps arguments to qm.ridge().
  The "ordinary" ridge Path of Hoerl and Kennard always uses \bold{q}-Shape = 0, while "uniform" shrinkage
  corresponds to \bold{q}-Shape = +1. NONE of these qm-Paths generally achieve Overall Minimum MSE
  Risk when p > 2 because they restrict attention to a "monotome" (increasing or decreasing) family of
  "delta" shrinkage-factors.  
  
  aug.lars() augments the Efron-Hastie lars() R-function to perform Least Angle Regression with
  MSE risk calculations and Maximum Likelihood TRACE displays ...like those of eff.ridge() and
  qm.ridge().
  
  uc.lars() applies Least Angle Regression methods to the \bold{Uncorrelated Components} of a possibly
  ill-conditioned set of x-Variables. Calculations use a closed-form expression for lars/lasso
  shrinkage delta-factors that apply because NO Ill-Conditioning is present in these "uc" cases.
  
  correct.signs() displays the Normal-theory maximum likelihood estimate of the regression
  coefficient vector that minimizes MSE Risk in the UNKNOWN direction of p-space PARALLEL to
  the true Beta vector. This estimate corrects "wrong-sign" problems in the sense that its
  coefficients have the same relative magnitudes and numerical signs as those of the "Correlation
  Form" of the X'y vector.
  
  YonX() displays Shrinkage statistics and graphics for "simple" linear regression (p = 1) models.
  
  RXpredict() makes predictions (i.e. computes "fitted.values") for 6 types of RXshrink
  estimation ...either at a user-specified m-Extent of Shrinkage or at the Normal-theory "minMSE"
  m-Extent.
}
\author{Bob Obenchain <wizbob@att.net>}
\references{
  Efron B, Hastie T, Johnstone I, Tibshirani R. (2003) Least angle regression.
  \emph{Annals of Statistics} \bold{32}, 407-499.

  Goldstein M, Smith AFM. (1974) Ridge-type estimators for regression analysis.
  \emph{J. Roy. Stat. Soc. B} \bold{36}, 284-291. (The 2-parameter shrinkage family.)

  Obenchain RL. (1975) Ridge Analysis Following a Preliminary Test of the Shrunken
  Hypothesis. \emph{Technometrics} \bold{17}, 431-441. \doi{10.1080/00401706.1975.10489369}

  Obenchain RL. (1977) Classical F-tests and Confidence Regions for Ridge Regression.
  \emph{Technometrics} \bold{19}, 429-439. \doi{10.1080/00401706.1977.10489582}
  
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
  
  Obenchain RL. (2023) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.3. \url{http://localcontrolstatistics.org}
}
\keyword{package}
\examples{
  demo(longley2)
}
