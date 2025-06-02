\name{lk_obs_score_within}
\alias{lk_obs_score_within}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute observed log-likelihood and score}
\description{Function used within \code{est_multi_poly_within} to compute observed log-likelihood and score.}

\usage{lk_obs_score_within(part_comp, lde1, lde2, lpart, lga1t, lga2t, S, R, yv, k1, k2, 
                    rm1, rm2, lv, J, fv, disc, glob, refitem, miss, ltype, XX1dis, XX2dis,
                    Xlabel, ZZ0, fort, Zpar, zpar, Zga1, zga1, Zga2, zga2, items1, items2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{part_comp}{complete vector of parameters}
  \item{lde1}{length of de1}
  \item{lde2}{length of de2}
  \item{lpart}{length of part}
  \item{lga1t}{length of ga1t}
  \item{lga2t}{length of ga2t}
  \item{S}{matrix of responses}
  \item{R}{matrix of observed responses indicator}
  \item{yv}{vector of frequencies}
  \item{k1}{number of latent classes for the 1st latent variable}
  \item{k2}{number of latent classes for the 2nd latent variable}
  \item{rm1}{number of dimensions for the 1st latent variable}
  \item{rm2}{number of dimensions for the 2nd latent variable}
  \item{lv}{number of response categories for each item}
  \item{J}{number of items}
  \item{fv}{indicator of constrained parameters}
  \item{disc}{presence of discrimination parameters}
  \item{glob}{indicator of global parametrization for the covariates}
  \item{refitem}{vector of reference items}
  \item{miss}{indicator of presence of missing responses}
  \item{ltype}{type of logit}
  \item{XX1dis}{array of covariates for the 1st latent variable}
  \item{XX2dis}{array of covariates for the 2nd latent variable}
  \item{Xlabel}{indicator for covariate configuration}
  \item{ZZ0}{design matrix}
  \item{fort}{to use Fortran}
  \item{Zpar}{array for the specification of constraints on the support points of the 1st and the 2nd latent variable and for the item difficulty parameters}
  \item{zpar}{vector for the specification of constraints on the support points of the 1st and the 2nd latent variable and for the item difficulty parameters}
  \item{Zga1}{matrix for the specification of constraints on the item discriminating indices for the 1st latent variable}
  \item{zga1}{vector for the specification of constraints on the item discriminating indices for the 1st latent variable}
  \item{Zga2}{matrix for the specification of constraints on the item discriminating indices for the 2nd latent variable}
  \item{zga2}{vector for the specification of constraints on the item discriminating indices for the 2nd latent variable}
  \item{items1}{items affected by the 1st latent variable} 
  \item{items2}{items affected by the 2nd latent variable}
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
