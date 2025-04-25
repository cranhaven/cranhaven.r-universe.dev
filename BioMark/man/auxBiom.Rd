\name{aux.biom}
\alias{aux.biom}
\alias{pcr.coef}
\alias{pcr.stab}
\alias{pls.coef}
\alias{lasso.coef}
\alias{lasso.stab}
\alias{vip.coef}
\alias{vip.stab}
\alias{pls.coef}
\alias{pls.stab}
\alias{shrinkt.coef}
\alias{shrinkt.stab}
\alias{studentt.coef}
\alias{studentt.stab}
\alias{pval.plsvip}
\alias{pval.pcr}
\title{Auxiliary functions in the biomarker package}
\description{These functions return coefficient sizes for a variety of
modelling methods. Not to be called directly by the user - use function
\code{get.biom} for that.}
\usage{
pcr.coef(X, Y, ncomp, scale.p, \dots)
pcr.stab(X, Y, ncomp, scale.p,
           segments = NULL, variables = NULL, \dots)
%
pls.coef(X, Y, ncomp, scale.p, \dots)
pls.stab(X, Y, ncomp, scale.p,
           segments = NULL, variables = NULL, \dots)
%
vip.coef(X, Y, ncomp, scale.p, \dots)
vip.stab(X, Y, ncomp, scale.p,
         segments = NULL, variables = NULL, \dots)
%
lasso.coef(X, Y, scale.p,
           lasso.opt = biom.options()$lasso,\dots)
lasso.stab(X, Y, scale.p,
           segments = NULL, variables = NULL, \dots)
%
shrinkt.coef(X, Y, scale.p, \dots)
shrinkt.stab(X, Y, scale.p,
             segments = NULL, variables = NULL, \dots)
%
studentt.coef(X, Y, scale.p, \dots)
studentt.stab(X, Y, scale.p,
              segments = NULL, variables = NULL, \dots)
%
pval.pcr(X, Y, ncomp, scale.p, npermut)
pval.plsvip(X, Y, ncomp, scale.p, npermut, smethod)
}
\arguments{
  \item{X}{Data matrix. Usually the number of columns (variables) is
    (much) larger than the number of rows (samples).}
  \item{Y}{Class indication. Either a factor, or a numeric vector.}
  \item{ncomp}{Number of latent variables to use in PCR and PLS (VIP)
    modelling. In function \code{get.biom} this may be a vector; in all
    other functions it should be one number. Default: 2.}
  \item{scale.p}{Scaling. This is performed individually in every
    crossvalidation iteration, and can have a profound effect on the
    results. Default: "none". Other possible choices: "auto" for
    autoscaling, "pareto" for pareto scaling, "log" and "sqrt" for log
    and square root scaling, respectively.}
  \item{segments}{matrix where each column indicates a set of samples to
    be left out of the analysis.}
  \item{variables}{indices of variables to be used in the analysis.}
  \item{lasso.opt}{optional arguments to the \code{glmnet} function, in
    the form of a list.}
  \item{\dots}{Further arguments for modelling functions. Often used to
    catch unused arguments.}
  \item{npermut}{Number of permutations to use in the calculation of the
    p values.}
  \item{smethod}{Either "both", "pls", or "vip" - indicates what
    coefficients to convert to p values. Both are derived from PLS
    models so it is much more efficient to calculate them together.}
}
\value{The functions ending in \code{coef} return t-statistics or
  model coefficients for all variables. The functions
  ending in \code{stab} return these statistics in a matrix, one column
  per segment. The functions starting with \code{pval} convert model
  coefficients or VIP statistics into p values, using permutation
  resampling.}
\author{Ron Wehrens}
\seealso{\code{\link{get.biom}}, \code{\link{glmnet}}, \code{\link{scalefun}}}
\keyword{multivariate}
\keyword{models}
