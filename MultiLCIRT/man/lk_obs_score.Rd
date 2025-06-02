\name{lk_obs_score}
\alias{lk_obs_score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute observed log-likelihood and score}
\description{Function used within \code{est_multi_poly} to compute observed log-likelihood and score.}
\usage{lk_obs_score(par_comp, lde, lpar, lga, S, R, yv, k, rm, l, J, fv, link,
	         disc, indga, glob, refitem, miss, ltype, XXdis, Xlabel,
	         ZZ0, fort)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par_comp}{complete vector of parameters}
  \item{lde}{length of de}
  \item{lpar}{length of par}
  \item{lga}{length of ga}
  \item{S}{matrix of responses}
  \item{R}{matrix of observed responses indicator}
  \item{yv}{vector of frequencies}
  \item{k}{number of latent classes}
  \item{rm}{number of dimensions}
  \item{l}{number of respnse categories}
  \item{J}{number of items}
  \item{fv}{indicator of constrained parameters}
  \item{link}{link function}
  \item{disc}{presence of discrimination parameter}
  \item{indga}{indicator of gamma parameters}
  \item{glob}{indicator of gloabl parametrization for the covariates}
  \item{refitem}{vector of reference items}
  \item{miss}{indicator of presence of missing responses}
  \item{ltype}{type of logit}
  \item{XXdis}{array of covariates}
  \item{Xlabel}{indicator for covariate configuration}
  \item{ZZ0}{design matrix}
  \item{fort}{to use fortran}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{lk}{log-likelihood function}
\item{sc}{score vector}
}

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
