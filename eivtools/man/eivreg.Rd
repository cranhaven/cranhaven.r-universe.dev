\name{eivreg}
\alias{eivreg}

\title{Errors-in-variables (EIV) linear regression}

\description{
  Fits errors-in-variables (EIV) linear regression given
  specified reliabilities, or a specified variance/covariance matrix for
  the measurement errors. For either case, it computes robust standard
  error estimates that allow for weighting and/or clustering.
}

\usage{
eivreg(formula, data, subset, weights, na.action, method = "qr",
model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = FALSE,
contrasts = NULL, reliability = NULL, Sigma_error = NULL,
cluster_varname = NULL, df_adj = FALSE, stderr = TRUE, offset,
...)
}

\arguments{
  
  \item{formula, data, subset, weights, na.action, method, model, x, y,
    qr}{
    See documentation for \code{\link{lm}}.
  }

  \item{singular.ok, contrasts, offset, \dots}{
    See documentation for \code{\link{lm}}.
  }
  
  \item{reliability}{
    Named numeric vector giving the reliability for each error-prone
    covariate.  If left \code{NULL}, \code{Sigma_error} must be specified.
  }
  
  \item{Sigma_error}{
    Named numeric matrix giving the variance/covariance matrix of the
    measurement errors for the error-prone covariate(s).  If left
    \code{NULL}, \code{reliability} must be specified.
  }

  \item{cluster_varname}{
    A character variable providing the name of a variable in \code{data}
    that will be used as a clustering variable for robust standard error
    computation.
  }
    
  \item{df_adj}{
    Logical (default FALSE); if TRUE, the estimated variance/covariance
    matrix of the regression parameters is multiplied by \code{N/(N-p)},
    where \code{N} is the number of observations used in the
    model fit and \code{p} is the number of regression parameters
    (including an intercept, if any).
  }

  \item{stderr}{
    Logical (default TRUE); if FALSE, does not compute estimated
    variance/covariance matrix of the regression parameters.
  }
}

\details{

  \emph{Theory}

  The EIV estimator applies when one wishes to estimate the parameters
  of a linear regression of \eqn{Y}{Y} on \eqn{(X,Z)}{(X,Z)}, but
  covariates \eqn{(W,Z)}{(W,Z)} are instead observed, where \eqn{W = X +
  U}{W = X + U} for mean zero measurement error \eqn{U}{U}. Additional
  assumptions are required about \eqn{U}{U} for consistent estimation;
  see references for details.

  The standard EIV estimator of the regression coefficients is \eqn{(Q'Q
  - S)^{-1}Q'Y}{(Q'Q - S)\^\{-1\}Q'Y}, where \eqn{Q}{Q} is the design
  matrix formed from \eqn{(W,Z)}{(W,Z)} and \eqn{S}{S} is a matrix that
  adjusts \eqn{Q'Q}{Q'Q} to account for elements that are distorted due
  to measurement error.  The value of \eqn{S}{S} depends on whether
  \code{reliability} or \code{Sigma_error} is specified.  When
  \code{Sigma_error} is specified, \eqn{S}{S} is known.  When
  \code{reliability} is specified, \eqn{S}{S} must be estimated using
  the marginal variances of the observed error-prone covariates.

  The estimated regression coefficients are solutions to a system of
  estimating equations, and both the system of equations and the
  solutions depend on whether \code{reliability} or \code{Sigma_error}
  is specified.  For each of these two cases, standard errors for the
  estimated regression coefficients are computed using standard results
  from M-estimation; see references.  For either case, adjustments for
  clustering are provided if specified.

  \emph{Syntax Details}

  Exactly one of \code{reliability} or \code{Sigma_error} must be
  specified in the call.  \code{Sigma_error} need not be diagonal in the
  case of correlated measurement error across multiple error-prone
  covariates.
  
  Error-prone variables must be included as linear main effects only; the
  current version of the code does not allow interactions among
  error-prone covariates, interactions of error-prone covariates with
  error-free covariates, or nonlinear functions of error-prone
  covariates.  The error-prone covariates cannot be specified with any
  construction involving \code{I()}.

  The current version does not allow \code{singular.ok=TRUE}.

  It is strongly encouraged to use the \code{data} argument to pass a dataframe
  containing all variables to be used in the regression, rather than
  using a matrix on the right hand side of the regression formula.  In
  addition, if \code{cluster_varname} is specified, everything including
  the clustering variable must be passed as \code{data}.

  If \code{weights} is specified, a weighted version of the EIV
  estimator is computed using operations analogous to weighted least
  squares in linear regression, and a standard error for this weighted
  estimator is computed.  Weights must be positive and will be
  normalized inside the function to sum to the number of observations
  used to fit the model.  Cases with missing weights will get dropped
  just like cases with missing covariates.

  Different software packages that compute robust standard errors make
  different choices about degrees-of-freedom adjustments intended to
  improve small-sample coverage properties.  The \code{df_adj} argument
  will inflate the estimated variance/covariance matrix of the estimated
  regression coefficients by \code{N/(N-p)}; see Wooldridge (2002, p. 57).  In
  addition, if \code{cluster_varname} is specified, the estimated
  variance/covariance matrix will be inflated by \code{M/(M-1)} where
  \code{M} is the number of unique clusters present in the estimation sample.
}

\value{
  An list object of class \code{eivlm} with the following components:

  \item{coefficients}{Estimated regression coefficients from EIV
    model.
  }

  \item{residuals}{Residuals from fitted EIV model.
  }

  \item{rank}{Column rank of regression design matrix.
  }

  \item{fitted.values}{Fitted values from EIV model.
  }

  \item{N}{Number of observations used in fitted model.
  }

  \item{Sigma_error}{The measurement error covariance matrix, if
    supplied.
  }

  \item{reliability}{The vector of reliabilities, if supplied.
  }

  \item{relnames}{The names of the error-prone covariates.
  }

  \item{XpX_adj}{The cross-product matrix of the regression, adjusted
    for measurement error.
  }

  \item{varYXZ}{The maximum likelihood estimate of the covariance matrix
    of the outcome \eqn{Y}{Y}, the latent covariates X and the observed,
    error-free covariates \eqn{Z}{Z}.  }

  \item{latent_resvar}{A degrees-of-freedom adjusted estimate of the
    residual variance of the latent regression.  NOTE: this not an
    estimate of the residual variance of the regression on the observed
    covariates \eqn{(W,Z)}{(W,Z)}, but rather an estimate of the
    residual variance of the regression on \eqn{(X,Z)}{(X,Z)}.  }

  \item{vcov}{The estimated variance/covariance matrix of the regression
    coefficients.
  }

  \item{cluster_varname, cluster_values, cluster_num}{If
    \code{cluster_varname} is specified, it is returned in the object,
    along with \code{cluster_values} providing the actual values of the
    clustering variable for the cases used in the fitted model, and
    \code{cluster_num}, the number of unique such clusters.
  }

  \item{OTHER}{ The object also includes components \code{assign},
    \code{df.residual}, \code{xlevels}, \code{call}, \code{terms},
    \code{model} and other optional components such as \code{weights},
    depending on the call; see \code{\link{lm}}. In addition, the object
    includes components \code{unadj_coefficients},
    \code{unadj_fitted.values}, \code{unadj_residuals},
    \code{unadj_effects}, and \code{unadj_qr} that are computed from the
    unadjusted regression model that ignores measurement error; see
    \code{\link{lm}}.  } }

\references{
  Carroll R.J, Ruppert D., Stefanski L.A. and Crainiceanu C.M.
  (2006). \emph{Measurement Error in Nonlinear Models: A Modern
    Perspective} (2nd edition).  London: Chapman & Hall.
  
  Fuller W. (2006). \emph{Measurement Error Models} (2nd edition). New York: John
  Wiley & Sons.
  
  Stefanksi L.A. and Boos D.B. (2002). \dQuote{The calculus of
    {M}-estimation,} \emph{The American Statistician} 56(1):29-38.

  Wooldridge J. (2002). \emph{Econometric Analysis of Cross Section and
    Panel Data}. Cambridge, MA: MIT Press.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org} modified the \code{\link{lm}}
  function to adapt it for EIV regression.
}

%% \note{
%% }
%% \section{Warning }{....}

\seealso{
  \code{\link{lm}}, \code{\link{summary.eivlm}}, \code{\link{deconv_npmle}}
}

\examples{
set.seed(1001)
## simulate data with covariates x1, x2 and z.
.n    <- 1000
.d    <- data.frame(x1 = rnorm(.n))
.d$x2 <- sqrt(0.5)*.d$x1 + rnorm(.n, sd=sqrt(0.5))
.d$z  <- as.numeric(.d$x1 + .d$x2 > 0)

## generate outcome
## true regression parameters are c(2,1,1,-1)
.d$y  <- 2.0 + 1.0*.d$x1 + 1.0*.d$x2 - 1.0*.d$z + rnorm(.n)

## generate error-prone covariates w1 and w2
Sigma_error <- diag(c(0.20, 0.30))
dimnames(Sigma_error) <- list(c("w1","w2"), c("w1","w2"))
.d$w1 <- .d$x1 + rnorm(.n, sd = sqrt(Sigma_error["w1","w1"]))
.d$w2 <- .d$x2 + rnorm(.n, sd = sqrt(Sigma_error["w2","w2"]))

## fit EIV regression specifying known measurement error covariance matrix
.mod1 <- eivreg(y ~ w1 + w2 + z, data = .d, Sigma_error = Sigma_error)
print(class(.mod1))
.tmp <- summary(.mod1)
print(class(.tmp))
print(.tmp)

## fit EIV regression specifying known reliabilities.  Note that
## point estimator is slightly different from .mod1 because
## the correction matrix S must be estimated when the reliability
## is known.
.lambda <- c(1,1) / (c(1,1) + diag(Sigma_error))
.mod2 <- eivreg(y ~ w1 + w2 + z, data = .d, reliability = .lambda)
print(summary(.mod2))
}

\keyword{models}
\keyword{regression}
