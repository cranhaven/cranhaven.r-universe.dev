\name{gMLE.nn}
\alias{gMLE.nn}

\title{ Normal-Normal Parameter Estimation }
\description{
Computes type-II Maximum likelihood estimates \eqn{\hat{\mu}} and \eqn{\hat{\tau}^2} for Normal prior \eqn{g\sim}Normal\eqn{(\mu, \tau^2)}.  
}
\usage{
gMLE.nn(value, se, fixed = FALSE, method = c("DL","SJ","REML","MoM"))
}

\arguments{
  \item{value}{Vector of values.}
  \item{se}{Standard error for each value.}
  \item{fixed}{When \code{FALSE}, treats the input as if from a random effects model; otherwise, will treat it as if it a fixed effect.}
  \item{method}{Determines the method to find \eqn{\tau^2}: \code{"DL"} uses Dersimonian and Lard technique, \code{"SJ"} uses Sidik-Jonkman, \code{"REML"} uses restricted maximum likelihood, and \code{"MoM"} uses a method of moments technique.
}
}

\value{
  \item{estimate}{Vector with both estimated \eqn{\hat{\mu}} and \eqn{\hat{\tau}^2}.}
  \item{mu.hat}{Estimated \eqn{\hat{\mu}}.}
  \item{tau.sq}{Estimated \eqn{\hat{\tau}^2}.}
  \item{method}{User-selected method.}
  }
\references{
Marin-Martinez, F. and Sanchez-Meca, J., 2010. "Weighting by inverse variance or by sample size in random-effects meta-analysis," \emph{Educational and Psychological Measurement, 70}(1), pp. 56-73.

Brown, L.D., 2008. "In-season prediction of batting averages: A field test of empirical Bayes and Bayes methodologies," \emph{The Annals of Applied Statistics}, pp. 113-152.

Sidik, K. and Jonkman, J.N., 2005. "Simple heterogeneity variance estimation for meta-analysis," \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, 54(2), pp. 367-384.
}

\author{Doug Fletcher
}

\examples{
data(ulcer)
### MLE estimate of alpha and beta
ulcer.mle <- gMLE.nn(ulcer$y, ulcer$se, method = "DL")$estimate
ulcer.mle
ulcer.reml <- gMLE.nn(ulcer$y, ulcer$se, method = "REML")$estimate
ulcer.reml
}