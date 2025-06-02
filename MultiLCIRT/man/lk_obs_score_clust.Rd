\name{lk_obs_score_clust}
\alias{lk_obs_score_clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute observed log-likelihood and score}
\description{Function used within \code{est_multi_poly} to compute observed log-likelihood and score.}
\usage{lk_obs_score_clust(par_comp, lde1, lde2, lpar, lga, S, R, kU, kV, rm,
                   l, J, fv, link, disc, indga, refitem, miss, ltype,
                   WWdis, Wlabel, XXdis, Xlabel, ZZ0, clust, fort)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par_comp}{complete vector of parameters}
  \item{lde1}{length of de}
  \item{lde2}{length of de}
  \item{lpar}{length of par}
  \item{lga}{length of ga}
  \item{S}{matrix of responses}
  \item{R}{matrix of observed responses indicator}
  \item{kU}{number of latent classes at cluster level}
  \item{kV}{number of latent classes at individual level}
  \item{rm}{number of dimensions}
  \item{l}{number of respnse categories}
  \item{J}{number of items}
  \item{fv}{indicator of constrained parameters}
  \item{link}{link function}
  \item{disc}{presence of discrimination parameter}
  \item{indga}{indicator of gamma parameters}
  \item{refitem}{vector of reference items}
  \item{miss}{indicator of presence of missing responses}
  \item{ltype}{type of logit}
  \item{WWdis}{array of covariates at cluster level}
  \item{Wlabel}{indicator for covariate configuration at cluster level}
  \item{XXdis}{array of covariates at individual level}
  \item{Xlabel}{indicator for covariate configuration at individual level}
  \item{ZZ0}{design matrix}
  \item{clust}{vector of cluster indicator for each unit}  
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
