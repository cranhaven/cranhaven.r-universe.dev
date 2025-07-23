\name{MTest}
\alias{MTest}
\title{MTest}
\usage{
MTest(object, nboot = 100,
                  nsam = NULL,trace = FALSE,seed = NULL,
                  valor_vif = 0.9)
}
\description{

MTest is a nonparametric test based on bootstrap for detecting multicollinearity. This test gives statistical support to two of the most famous methods for detecting multicollinearity in applied work: Klein’s rule and Variance Inflation Factor (VIF for essential multicollinearity). 
}


\arguments{
  \item{object}{an object representing a model of an appropriate class (mainly "lm"). This is used as the model in MTest.}
  \item{nboot}{Numeric; number of bootstrap iterations to obtain the probability distribution of R squared (global and auxiliar).}
  \item{nsam}{Numeric; sample size for bootstrap samples.}
  \item{trace}{Logical; prints iteration process.}
  \item{seed}{Numeric; seed value for the bootstrap in nboot parameter.}
  \item{valor_vif}{Numeric; value to be compared in kleins rule.}
}

\details{
MTest generates a bootstrap distribution for the coefficient of determination which lets the researcher assess multicollinearity by setting a statistical significance \eqn{\alpha}, or more precisely, an achieved significance level (ASL) for a given threshold.


Consider the regression model

\eqn{
Y_i	= \beta_0X_{0i} + \beta_1X_{1i} + \cdots+ \beta_pX_{pi} +u_i
}

where \eqn{i = 1,...,n}, \eqn{X_{j,i}} are the predictors with \eqn{j = 1,...,p}, \eqn{X_0 = 1} for all \eqn{i} and \eqn{u_i} is the gaussian error term. 

In order to describe Klein's rule and VIF methods, we need to define \emph{auxiliary regressions} associated to model. An example of an auxiliary regressions is:

\eqn{
X_{2i} =  \gamma_1X_{1i} + \gamma_3X_{3i} + \cdots+ \gamma_pX_{pi} +u_i.
}

In general, there are \eqn{p} auxiliary regressions and the dependent variable is omitted in each auxiliary regression. Let \eqn{R_{g}^{2}} be the coefficient of determination of the model and \eqn{R_{j}^{2}} the \eqn{j\text{th}} coefficient of determination of the \eqn{j\text{th}} auxiliary regression.


}

\value{
  Returns an object of class \code{MTest}.
  An object of class \code{MTest} is a list containing
  at most the following components:
  \item{pval_vif}{p values for vif test;}
  \item{pval_klein}{p values for klein test;}
  \item{Bvals}{A \eqn{nboot \times (p+1)} matrix where rows are the number of bootstap samples and the columns are  \eqn{R_{g_{boot}}^{2}} and \eqn{R_{j_{boot}}^{2}} which are estimates of estimates of \eqn{R_{g}^{2}} and \eqn{R_{j}^{2}}, see Section \bold{Details}}
  \item{vif.tot}{Observed VIF values;}
  \item{R.tot}{Observed \eqn{R_{g}^{2}} and \eqn{R_{j}^{2}} values;}
  \item{nsam}{sample size used in bootstrap procedure.}
  }

\author{Víctor Morales Oñate, \email{victor.morales@uv.cl}, \url{https://sites.google.com/site/moralesonatevictor/},\url{https://www.linkedin.com/in/vmoralesonate/}
Bolívar Morales Oñate, \email{bmoralesonate@gmail.com}, \url{https://sites.google.com/site/moralesonatevictor/}
}

\references{
  Morales-Oñate, V., and Morales-Oñate, B. (2023). \emph{MTest: a Bootstrap Test for Multicollinearity}. Revista Politécnica, 51(2), 53–62. \doi{https://doi.org/10.33333/rp.vol51n2.05}
}

\examples{
library(MTest)
data(simDataMTest)
m1 <- lm(y~.,data = simDataMTest)

boot.sol <- MTest(m1,trace=FALSE,seed = 1,nboot = 50)
boot.sol$pval_vif
boot.sol$pval_klein
head(boot.sol$Bvals)
print(boot.sol)
}
\keyword{Multicollinearity}