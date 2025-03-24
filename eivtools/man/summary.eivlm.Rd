\name{summary.eivlm}
\alias{summary.eivlm}

\title{\code{summary} method for objects of class \code{eivlm}.}

\description{
  Computes summary quantities for a model of class \code{eivlm}.  The
  computations include some quantities for the standard regression model
  (uncorrected for covariate measurement error), as well as quantities
  relevant to the EIV model.
}

\usage{
\method{summary}{eivlm}(object, correlation = FALSE, symbolic.cor = FALSE, ...)
}

\arguments{
  \item{object}{
    A model object of class \code{eivlm}.
  }

  \item{correlation, symbolic.cor, \dots}{
    See help for \code{\link{summary.lm}}
  }
}

\value{An list object of class \code{summary.eivlm} with components:

  \item{residuals, fitted.values, N, latent_resvar, vcov, relnames,
    coefficients}{See \code{\link{eivreg}}.
  }

  \item{call, terms, aliased, df, coefficients}{See \code{\link{summary.lm}}.}

  \item{reliability}{If \code{reliability} is specified in model fit.
  }

  \item{Sigma_error}{If \code{Sigma_error} is specified in model fit.
  }

  \item{symbolic.cor}{If \code{symbolic.cor} is specified in call to
    function.
  }

  \item{latent_R2}{Maximum likelihood estimate of R-squared for
    regression of Y on (X,Z).  NOTE: This is not the R-squared of the
    fitted regression of Y on (W,Z).
  }

  \item{latent_R2_dfadj}{Estimate of R-squared for regression of Y on
    (X,Z) adjusted by number of estimated regression parameters.
  }

  \item{OTHER}{The object also includes components
    \code{unadj_residuals}, \code{unadj_coefficients},
    \code{unadj_sigma}, \code{unadj_r.squared},
    \code{unadj_adj.r.squared}, \code{unadj_fstatistic} and
    \code{unadj_cov.unscaled} that are computed from the "unadjusted"
    regression model that ignores measurement error; see
    \code{\link{summary.lm}}.  Also, additional components will be
    included if either \code{weights} or \code{cluster_varname} is
    specified in the call to \code{eivlm}; see that help file for
    details.
  }
}

\seealso{
  The model fitting function \code{\link{eivreg}}, \code{\link{summary}}.

  Function \code{\link{coef}} will extract the matrix of coefficients
  with standard errors, t-statistics and p-values.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\keyword{methods}
