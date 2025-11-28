\name{MLtrue}
\alias{MLtrue}
\title{Simulate data for Linear Models with known Parameter values and Normal Errors}
\description{
  Using specified numerical values for Parameters, such as the true error-term variance,
  that usually are unknown, MLtrue() creates a new data.frame that contains variables
  "Yhat" (the vector of expected values) and "Yvec" (Yhat + IID Normal error-terms) as
  well as the p given X-variables. Thus, MLtrue() produces "correct" linear models that
  can be ideal for analysis and bootstraping via methods based on Normal-theory.
}
\usage{
  MLtrue(form, data, seed, go=TRUE, truv, trub, truc)
}
\arguments{
  \item{form}{A regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{Data frame containing observations on all variables in the formula.}
  \item{seed}{Seed for random number generation between 1 and 1000. When this seed
   number is missing in a call to MLtrue(), a new seed is generated using runif().
   Error terms are then generated using calls to rnorm(), and the seed used is
   reported in the MLtrue output.list to enable reproducible research.}
  \item{go}{Logical value: go=TRUE starts the simulation using OLS estimates or the
   numerical values from the (optional) truv, trub and truc arguments to define the
   "Yhat" and "Yvec" variables; go=FALSE causes MLtrue() to compute OLS estimates
   that ignore the truv, trueb and truc arguments. Thus, go=FALSE provides a
   convenient way for users to Print and Examine the MLtrue() output.list before
   deciding which parameter-settings they wish to actually USE or MODIFY before
   making a final run with go=TRUE.}
  \item{truv}{Optional: numerical value for the true error variance, sigma^2.}
  \item{trub}{Optional: column vector of numerical values for the true regression
   coefficients.}
  \item{truc}{Optional: column vector of numerical values for the true uncorrelated
   components of the regression coefficient vector.}
}
\details{RXshrink functions like eff.ridge() and qm.ridge() calculate maximum likelihood
  estimates ...either unbiased (BLUE) or "optimally" biased (minimum MSE risk) along a
  specified "path"... for typical statistical inference situations where true regression
  parameters are unknown. Furthermore the specified linear-model may be "incorrect" or
  the error-terms may not be IID Normal variates.  In sharp contrast with this usual
  situation, the MLtrue() function generates a "Yvec" vector of outcomes from a CORRECT
  model of the given form that does contain IID Normal error-terms and all true parameter
  values are KNOWN. This makes it interesting to compare plots and output.lists from
  RXshrink GRR-estimation functions on a "real" data.frame with the corresponding outputs
  from a MLtrue() data.frame with known parameter-setting that are either identical to or 
  "close" to those estimated from "real" data. WARNING: All output X-variables are
  "centered" and "rescaled" to have mean ~0 and variance 1. Yhat expected values are
  "centered" but usually have variance differing from 1. Yvec values are not "centered"
  and have variance determined by either the original data.frame or the "truv" setting.
}
\value{ An output list object of class MLtrue:
  \item{new}{A data.frame containing the Yvec and Yhat vectors as well as centered and
   rescaled versions of the input X-matrix and original Y-vector.}
  \item{Yvec}{An additional copy of Yvec pseudo-data containing Normal error-terms.}
  \item{Yhat}{An additional copy of Yhat linear fitted-values.}
  \item{seed}{Seed actually used in random-number generation; needed for replication.}
  \item{tvar}{True numerical value of sigma^2 ...[1,1] matrix.}
  \item{tbeta}{True numerical values of beta-coefficients ...[p,1] matrix.}
  \item{tcomp}{True numerical values of uncorrelated components ...[p,1] matrix.}
  \item{useb}{Logical: TRUE => tbeta vector was used in simulation. FALSE => tcomp
   vector was used rather than tbeta.}
  \item{data}{The name of the data.frame object specified in the second argument.}  
  \item{form}{The regression formula specified in the first argument. NOTE: The final
   11 output items [p, ..., xs] are identical to those in output lists from RXshrink
   functions like eff.ridge() and qm.ridge().}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}}, \code{\link{qm.ridge}} and \code{\link{MLboot}}.}
\examples{
  # require(RXshrink)
  data(mpg)
  form <- mpg~cylnds+cubins+hpower+weight
  MLtest <- MLtrue(form, mpg, go=FALSE)   # all other potential arguments "missing"...
  MLtest   # print current parameter estimates...
  cvec <- c(-0.5, 0.15, -0.16, -0.6)   # define alternative "true" components...
  MLout <- MLtrue(form, mpg, truc = cvec ) # Use "truc" input with default go=TRUE...
  str(MLout)
  formY <- Yhat~cylnds+cubins+hpower+weight   # Formula for true expected Y-outcomes...
  lmobj <- lm(formY, MLout$new)
  max(abs(lmobj$residuals))   # essentially 0 because linear model is "correct"...
  # effobj <- eff.ridge(formY, MLout$new)   ...generates "Error" because RSQUARE=1.
}
\keyword{regression}
