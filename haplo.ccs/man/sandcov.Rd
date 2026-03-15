\name{sandcov}
\alias{sandcov}
\title{Compute Sandwich Variance-Covariance Estimates}
\description{
  
  'sandcov' computes sandwich variance-covariance estimates for the
  coefficients of a fitted model. These estimates may be used to
  calculate robust standard error estimates.

}
\usage{

sandcov(model, id)

}
\arguments{
  
  \item{model}{a fitted model of class '"lm"' or '"glm"'.}

  \item{id}{the numeric vector used to identify subjects, expanded for
    each subject by the number of observations for that subject.}

}
\details{

  For a model of class '"haplo.ccs"', the sandwich variance-covariance
  matrix is automatically provided as the object 'covariance', or may be
  extracted by 'vcov' (i.e., 'vcov.haplo.ccs'). See examples below.

}
\value{
  
  A named matrix for the covariance of the regression coefficients
  specified in 'model', calculated using the sandwich method.

}
\author{
  
  Benjamin French and Thomas Lumley, University of Washington
  
}
\references{

  French B, Lumley T, Monks SA, Rice KM, Hindorff LA, Reiner AP, Psaty
  BM. Simple estimates of haplotype relative risks in case-control
  data. Genetic Epidemiology 2006; 30(6):485-494.
  
  Liang KY, Zeger SL. Longitudinal data analysis using generalized
  linear models. Biometrika 1986; 73(1):13-22.

}
\seealso{

  \code{\link{glm}},
  \code{\link{haplo.ccs}},
  \code{\link{lm}}

}
\examples{

data(renin)

## Fit a model for covariate effects.

m1 <- glm(case ~ age + factor(race) + gender, family=binomial(link=logit))

## Obtain sandwich variance-covariance matrix.

id <- 1:length(case)
v1 <- sandcov(model = m1, id = id)

## Calculate robust standard error estimates.

se1 <- sqrt(diag(v1))

## Fit a model for haplotype and covariate effects.

m2 <- haplo.ccs(case ~ gender + age + factor(race) + haplo(geno))

## Obtain sandwich variance-covariance matrix by one of two methods.

v2 <- m2$covariance
v2 <- vcov(m2)

## Calculate robust standard error estimates.

se2 <- sqrt(diag(v2))

}
\keyword{models}
\keyword{regression}
