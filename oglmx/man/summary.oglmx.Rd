\name{summary.oglmx}
\alias{summary.oglmx}
\alias{print.summary.oglmx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing Ordered Discrete Outcome Model Fits
}
\description{
\code{summary} method for class "\code{oglmx}"
}
\usage{
\method{summary}{oglmx}(object, tol = 1e-20, ... )

\method{print}{summary.oglmx}(x, ... )

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
   an object of class "oglmx"
}
  \item{tol}{
  argument passed to qr.solve, defines the tolerance for detecting linear dependencies in the hessian matrix to be inverted.
}
  \item{\dots}{
  additional arguments, currently ignored.
  }
  \item{x}{object of class \code{summary.oglmx}.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{regtype }{character string describing the type of model estimated.}
  \item{loglikelihood }{log-likelihood for the estimated model.}
  \item{estimate }{matrix with four columns and number of rows equal to the number of estimated parameters. Columns of the matrix correspond to estimated coefficients, standard errors, t-statistics and (two-sided) p-values.}
  \item{estimateDisplay}{the same data as in \code{estimate} but separated into a list with elements for each type of parameter estimate. The first element is for parameters in the mean equation, second element for parameters in the variance equation and the final element is for threshold parameters.}
  \item{no.iterations }{number of iterations used in function that maximises the log-likelihood.}
  \item{McFaddensR2 }{McFadden's \eqn{R^2} aka Pseudo-\eqn{R^2}. Calculated as:
    \deqn{R^2=1-\log{L_{fit}}/\log{L_0}}
    where \eqn{\log{L_{fit}}} is the log-likelihood for the fitted model and \eqn{\log{L_0}} is the log-likelihood from an intercept only model that estimates the probability of each alternative to be the sample average.
  }
  \item{AIC }{Akaike Information Criterion, calculated as:
    \deqn{AIC=2k-2\log{L_{fit}}}
  where \eqn{k} is the number of estimated parameters.
  }
  \item{coefficients }{named vector of estimated parameters.}
}
\references{
McFadden, D. (1973) \emph{Conditional Logit Analysis of Qualitative Choice Behavior} in \emph{Frontiers in Econometrics}.
P.Zarembka (Ed.), New York, Academic Press.
}
\author{
Carroll, Nathan \email{nathan.carroll@ur.de}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (object, tol = 1e-20) 
%{
%    stdEr.oglmx <- diag(vcov(object, tol = tol))^0.5
%    t <- object$coefficients/stdEr.oglmx
%    p <- 2 * pnorm(-abs(t))
%    results <- cbind(Estimate = object$coefficients, `Std. error` = stdEr.oglmx, 
%        `t value` = t, `Pr(>|t|)` = p)
%    betaresults <- results[attr(object$coefficients, "coefftypes")[[1]], 
%        ]
 %   deltaresults <- results[attr(object$coefficients, "coefftypes")[[2]], 
  %      ]
  %  cutoffresults <- results[attr(object$coefficients, "coefftypes")[[3]], 
  %      ]
  %  resultsSplit <- list(betaresults, deltaresults, cutoffresults)
  %  summary <- list(regtype = regtype.oglmx(object), loglikelihood = object$loglikelihood, 
  %      estimate = results, estimateDisplay = resultsSplit, no.iterations = object$no.iterations, 
  %      McFaddensR2 = McFaddensR2.oglmx(object))
  %  class(summary) <- "summary.oglmx"
  %  summary
  %}
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
