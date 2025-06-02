\name{matr_glob}
\alias{matr_glob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Matrices to compute generalized logits}
\description{It provides the matrices used to compute a vector of generalized logits on the basis of a
vector of probabilities according to the formula \emph{Co*log(Ma*p)}; this is an internal function.}
\usage{matr_glob(l, type = "g")}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{number of response categories}
  \item{type}{type of logit (l = local-logits, g = global-logits)}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  \item{Co}{matrix of contrasts}
  \item{Ma}{marginalization matrix}
}

\references{Colombi, R. and Forcina, A. (2001), Marginal regression models for the analysis of positive
association of ordinal response variables, \emph{Biometrika}, \bold{88}, 1007-1019.

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