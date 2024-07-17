\name{systemfitAR}
\alias{systemfitAR}
\title{Linear Equation System Estimation with Correction on Autocorrelation}

\description{
  Fits a set of linear structural equations using Ordinary Least
  Squares (OLS), Weighted Least Squares (WLS), Seemingly Unrelated Regression (SUR), with the option of autocorrelation correlation.
}

\usage{
systemfitAR(formula, method = "OLS", inst = NULL, data = list(),
  restrict.matrix = NULL, restrict.rhs = NULL, restrict.regMat = NULL, 
  pooled = FALSE, control = systemfit.control( ... ),
  AR1 = FALSE, rho.sel = c("all", "mean"), model = c("static", "dynamic"), ...)
}

\arguments{
   \item{formula}{an object of class \code{formula}
      (for single-equation models) or (typically) a list of objects of class 
      \code{formula} (for multiple-equation models).}
   \item{method}{the estimation method, one of "OLS", "WLS", "SUR";
      iterated estimation methods can be specified by setting control parameter
      \code{maxiter} larger than 1 (e.g. 500).}
   \item{inst}{one-sided model formula specifying instrumental variables
      or a list of one-sided model formulas if different instruments should
      be used for the different equations (only needed for 2SLS, W2SLS, and
      3SLS estimations).}
   \item{data}{an optional data framecontaining the variables in the model.}
   \item{restrict.matrix}{an optional j x k matrix to impose linear
      restrictions on the coefficients by 
      \code{restrict.matrix} * \eqn{b} = \code{restrict.rhs}
      (j = number of restrictions, k = number of all coefficients,
      \eqn{b} = vector of all coefficients)}
   \item{restrict.rhs}{an optional vector with j elements to impose linear
      restrictions (see \code{restrict.matrix}); default is a vector
      that contains j zeros.}
   \item{restrict.regMat}{an optional matrix to impose restrictions
      on the coefficients by post-multiplying the regressor matrix
      with this matrix (see details).}
   \item{control}{list of control parameters.
      The default is constructed by the function \code{\link{systemfit.control}}.
      See the documentation of \code{\link{systemfit.control}} for details.}
   \item{pooled}{logical, restrict coefficients to be equal in all equations
      (only for panel-like data).}
   \item{AR1 }{ whether first-order autocorrelation is corrected}
   \item{rho.sel }{how rho is computed; \code{rho.sel = "all"} means that the system is estimated as a single equation and the residuals are used to compute rho. If "mean", each equation in the system is estimated separately and the average of rhos from all the equations are used.}   
   \item{model }{Static model has interceipt, while dynamic model has no intercept; see translog cost function and the package for detail.}
   \item{...}{arguments passed to \code{\link{systemfit.control}}.}
}

\details{
   This is a wrapper of \code{systemfit} with an addition of autocorrelation correction. It is mainly used for SUR model with autocorrelation. The main reference sources are Greene (2003), LIMDEP 9.0 manual, Judge et al. (1985), and Berndt and Savin (1975).
}

\value{
  \code{systemfit} returns a list of the class \code{systemfit}.
  This list contains one special object: "eq". It is a list and contains
  one object for each estimated equation. These objects are of the class
  \code{systemfit.equation} and contain the results that belong only to the
  regarding equation.  In addition, there are four new items in the output:

  The objects of the class \code{systemfit} and
  \code{systemfit.equation} have the following components (the elements of
  the latter are marked with an asterisk (\eqn{*})):

  \item{rho }{ autocorrelation coefficient}
  \item{rho_ste }{standard error of rho; if \code{rho.sel = "mean"}, then it is a vector of the standar errors for individual equations.}
  \item{data }{data used for \code{systemfit}; this is data adjusted for autocorrelation if \code{AR1 = TRUE}; otherwise, it is just the raw data.}
  \item{formula }{ forumula used for \code{systemfit}. This can be adjusted for autocorrelation; costant is adjusted as (1 - rho).}    
}

\references{
  Greene, W. H. (2003)
  \emph{Econometric Analysis, Fifth Edition}, Prentice Hall.

  Judge, George G.; W. E. Griffiths; R. Carter Hill; Helmut Luetkepohl and Tsoung-Chao Lee (1985)
  \emph{The Theory and Practice of Econometrics, Second Edition}, Wiley.

  LIMDEP 9.0 software manual. 
  
  Berndt, E.R., and N.E. Savin. 1975. Estimation and hypothesis testing in singular equation systems with autoregressive disturbances. Econometrica 43(5/6):937-957.
}

\author{Changyou Sun (\email{cs258@msstate.edu})}

\seealso{\code{\link{lm}}; \code{\link{aiStaFit}}; \code{\link{systemfit}}}

\examples{
# Check Berndt and Savin (1975) dataset

# Check Kemenda data          

}

\keyword{models}
\keyword{regression} 