\name{est_multi_glob}
\alias{est_multi_glob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit marginal regression models for categorical responses}
\description{It estimates marginal regression models to datasets consisting of a categorical response and one or more covariates by a Fisher-scoring algorithm; this is an internal function.}

\usage{est_multi_glob(Y, X, model, ind = 1:nrow(Y), be = NULL, Dis = NULL,
               dis = NULL, disp=FALSE, only_sc = FALSE, Int = NULL,
               der_single = FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{matrix of response configurations}
  \item{X}{array of all distinct covariate configurations}
  \item{model}{type of logit (g = global, l = local, m = multinomial)}
  \item{ind}{vector to link responses to covariates}
  \item{be}{initial vector of regression coefficients}
  \item{Dis}{matrix for inequality constraints on be}
  \item{dis}{vector for inequality constraints on be}
  \item{disp}{to display partial output}
  \item{only_sc}{to exit giving only the score}
  \item{Int}{matrix of the fixed intercepts}  
  \item{der_single}{to require single derivatives}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  \item{be}{estimated vector of regression coefficients}
  \item{lk}{log-likelihood at convergence}
  \item{Pdis}{matrix of the probabilities for each distinct covariate configuration}  
  \item{P}{matrix of the probabilities for each covariate configuration}
  \item{sc}{score}
  \item{Sc}{single derivative (if der_single=TRUE)}
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
\keyword{maximum likelihood estimation}
\keyword{Fisher-scoring algorithm}% __ONLY ONE__ keyword per line