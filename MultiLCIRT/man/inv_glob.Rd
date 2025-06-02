\name{inv_glob}
\alias{inv_glob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Invert marginal logits}
\description{Function used within \code{est_multi_glob} to invert marginal logits and fit the marginal
regression model; this is an internal function.}
\usage{
inv_glob(eta, type = "g", der = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eta}{vector of logits}
  \item{type}{type of logit (l = local-logits, g = global-logits)}
  \item{der}{indicator that the derivative of the canonical parameters with respect to the vector of marginal logits
             is required (F = not required, T = required)}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{p}{vector of probabilities}
\item{D}{derivative of the canonical parameters with respect to the vector of marginal logits (if der = T)}
}

\references{Colombi, R. and Forcina, A. (2001), Marginal regression models for the analysis of positive association
of ordinal response variables, \emph{Biometrika}, \bold{88}, 1007-1019.

Glonek, G. F. V. and McCullagh, P. (1995), Multivariate logistic models, \emph{Journal of the Royal 
Statistical Society, Series B}, \bold{57}, 533-546.}

\author{Francesco Bartolucci - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
