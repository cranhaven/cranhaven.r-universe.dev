\name{prob_multi_glob_gen}
\alias{prob_multi_glob_gen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Global probabilities}
\description{It provides matrix of probabilities under different parametrizations and for the case of response variables having a different number of categories.}

\usage{prob_multi_glob_gen(X, model, be, ind=(1:dim(X)[3]))}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{array of all distinct covariate configurations}
  \item{model}{type of logit (g = global, l = local, m = multinomial)}
  \item{be}{initial vector of regression coefficients}
  \item{ind}{vector to link responses to covariates}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  \item{Pdis}{matrix of distinct probability vectors}
  \item{P}{matrix of the probabilities for each covariate configuration}
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

%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
