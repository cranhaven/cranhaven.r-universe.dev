\name{penmodelEM}
\alias{penmodelEM}
\title{EM algorithm for estimating the penetrance model with missing genotypes}
\description{
Fits a penetrance model for family data with missing genotypes via the EM algorithm and provides model parameter estimates. 
}
\usage{
penmodelEM(formula, cluster = "famID", gvar = "mgene", parms, cuts = NULL, data, 
design = "pop", base.dist = "Weibull", agemin = NULL, robust = FALSE, method = "data", 
mode = "dominant", q = 0.02)
}
\arguments{
\item{formula}{A formula expression as for other regression models. The response should be a survival object as returned by the \code{Surv} function. See the documentation for \code{Surv}, \code{lm} and \code{formula} for details.
}
\item{cluster}{
Name of cluster variable. Default is \code{"famID"}.
}
\item{gvar}{Name of genetic variable. Default is \code{"mgene"}.}
\item{parms}{
Vector of initial values for the parameters in the model including baseline parameters and regression coefficients. \code{parms = c(baseparm, coef)}, where \code{baseparm} includes the initial values for baseline parameters used for \code{base.dist}, and \code{coef} includes the initial values for regression coefficients for the variables specified in \code{formula}. See details for the baseline parameters.
}
\item{cuts}{ 
Vector of cuts that define the intervals where the hazard function is constant. The \code{cuts} should be specified \code{base.dist="piecewise"} and must be strictly positive and finite. Default is \code{NULL}.
}	
\item{data}{
 Data frame generated from \code{\link{simfam}} or data frame containing specific variables: \code{famID}, \code{indID}, \code{gender}, \code{currentage}, \code{mgene}, \code{time}, \code{status} and \code{weight} with \code{attr(data,"agemin")} specified.
}
\item{design}{
Study design of the family data. Possible choices are: \code{"pop"}, \code{"pop+"}, \code{"cli"}, \code{"cli+"} or \code{"twostage"}, where \code{"pop"} is for the population-based design with affected probands whose mutation status can be either carrier or non-carrier, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands, \code{"cli"} is for the clinic-based design that includes affected probands with at least one parent and one sib affected, \code{"cli+"} is similar to \code{"cli"} but with mutation carrier probands, and \code{"twostage"} is for the two-stage design with oversampling of high risks families. Default is \code{"pop"}.
	}
 \item{base.dist}{
Choice of baseline hazard distributions to fit. Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"}, \code{"gamma"}, \code{"logBurr"}, or \code{"piecewise"}. Default is \code{"Weibull"}.
}
\item{agemin}{
Minimum age of disease onset or minimum age. Default is \code{NULL}.
}
\item{robust}{
Logical; if \code{TRUE}, the robust `sandwich' standard errors and variance-covariance matrix are provided, otherwise the conventional standard errors and variance-covariance matrix are provided.
 }
 \item{method}{
Choice of methods for calculating the carrier probabilities for individuals with missing mutation status. Possible choices are \code{"data"} for empirical calculation of the carrier probabilities based on the observed carriers' statuses in the entire sample, specific to generation and proband's mutation status or \code{"mendelian"} for calculating carrier probabilities based on Mendelian transmission probabilies with the given allele frequency and mutation statuses observed in the family. Default is \code{"data"}.
 
 If \code{method = "mendelian"}, specify both \code{mode} of the inheritance and the allele frequency \code{q}. 
 
}
 \item{mode}{
Choice of modes of inheritance for calculating carrier probabilies for individuals with missing mutation status. Possible choices are \code{"dominant"} for dominant model or \code{"recessive"} for recessive model. Default is \code{"dominant"}.
}
 \item{q}{
Frequency of the disease causing allele used for calculating carrier pobabilities. The value should be between 0 and 1. If \code{NULL}, the estimated allele frequency from data will be used. Default value  is \code{0.02}.
}

}
\details{

The expectation and maximization (EM) algorithm is applied for making inference about the missing genotypes. In the expectation step, for individuals with unknown carrier status, we first compute their carrier probabilities given their family's observed phenotype and genotype information based on current estimates of parameters \ifelse{html}{\out{&theta;}}{\eqn{\theta}{theta}} as follows,
\ifelse{html}{\out{w<sub>fi</sub> = P(X<sub>fi</sub> = 1 | Y<sub>fi</sub>, X<sub>f</sub><sup>o</sup>), }}{\deqn{ w_{fi} = P(X_{fi}=1|Y_{fi}, X^o_f),}{ w(i,f) = P( X(i,f) = 1 | Y(i,f), Xo(f) ), }}
where  \ifelse{html}{\out{X<sub>fi</sub>}}{\eqn{X_{fi}}{w(i,f)}} represents the mutation carrier status and \ifelse{html}{\out{Y<sub>fi</sub>}}{ \eqn{Y_{fi}}{Y(i,f)}} represents the phenotype in terms of age at onset \ifelse{html}{\out{t<sub>fi</sub>}}{\eqn{t_{fi}}{t(i,f)}} and disease status \ifelse{html}{\out{&delta;<sub>fi</sub>}}{\eqn{\delta_{fi}}{\delta(i,f)}} for individual \ifelse{html}{\out{i, i = 1, ..., n<sub>f</sub>,}}{\eqn{i, i = 1, \ldots, n_f,}{i, i = 1, ..., nf}} in family \eqn{f, f = 1, ..., n,} and \ifelse{html}{\out{X<sub>f</sub><sup>o</sup>}}{\eqn{X^o_f}{Xo(f)}} represents the observed genotypes in family \eqn{f}.

Then, we obtain the conditional expectation of the log-likelihood function (\eqn{\ell}{l}) of the complete data given the observed data as a weighted log-likelihood, which has the form
\ifelse{html}{\out{ E<sub>&theta;</sub> [ l (&theta; | Y, X<sup>o</sup>) ] =  &sum;<sub>f</sub> &sum;<sub>i</sub> l <sub> fi</sub> (&theta; | X<sub>fi</sub> = 1) * w<sub>fi</sub> +  l<sub>fi</sub> (&theta; | X<sub>fi</sub> = 0) * ( 1 - w<sub>fi</sub> ). }}{ \deqn{ E_{\theta} [\ell (\theta | Y, X^o)] =  \sum_f^n \sum_i^{n_f} \ell_{fi}(\theta | X_{fi}=1) w_{fi} +  \ell_{fi}(\theta | X_{fi}=0) (1-w_{fi}). }{ E [ l (\theta | Y, Xo) ] =  \sum \sum l(\theta | X(i,f) = 1) * w(i,f) +  l(\theta | X(i,f) = 0) * (1 - w(i,f)). }}
In the maximization step, the updated parameter estimates are obtained by maximizing the weighted log likelihood computed in the E-step. These expectation and maximization steps iterate until convergence to obtain the maximum likelihood estimates. See more details in Choi and Briollais (2011) or Choi et al. (2014).

Note that the baseline parameters include \code{lambda} and \code{rho}, which represent the scale and shape parameters, respectively, and \code{eta}, additional parameter to specify for \code{"logBurr"} distribution. For the \code{"lognormal"} baseline distribution, \code{lambda} and \code{rho} represent the location and scale parameters for the normally distributed logarithm, where \code{lambda} can take any real values and \code{rho} > 0. For the other baselinse distributions, \code{lambda} > 0, \code{rho} > 0, and \code{eta} > 0. When a piecewise constant distribution is specified for the baseline hazards, \code{base.dist="piecewise"}, \code{baseparm} should specify the initial interval-constant values, one more than the cut points specified by\code{cuts}.

Transformed baseline parameters are used for estimation; log transformation is applied to both scale and shape parameters (\eqn{\lambda, \rho}{lambda, rho}) for \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"} and \code{"gamma"} baselines, to (\eqn{\lambda, \rho, \eta}{lambda, rho, eta}) for \code{"logBurr"} and to the piecewise constant parameters for a \code{piecewise} baseline hazard. For \code{"lognormal"} baseline distribution, the log transformation is applied only to \eqn{\rho}{rho}, not to \eqn{\lambda}{lambda}, which represents the location parameter for the normally distributed logarithm.

Calculations of penetrance estimates and their standard errors and 95\% confidence intervals at given ages can be obtained by \code{\link{penetrance}} function via Monte-Carlo simulations of the estimated penetrance model.
}
\value{
Returns an object of class \code{'penmodel'}, including the following elements:

\item{estimates}{
Parameter estimates of transformed baseline parameters and regression coefficients.
}
\item{varcov}{
Variance-covariance matrix of parameter estimates obtained from the inverse of Hessian matrix. 
}
\item{varcov.robust}{
Robust `sandwich' variance-covariance matrix of parameter estimates when \code{robust=TRUE}. 
}
\item{se}{
Standard errors of parameter estimates obtained from the inverse of Hessian matrix.
}
\item{se.robust}{
Robust `sandwich' standard errors of parameter estimates when \code{robust=TRUE}.
}
\item{logLik}{Loglikelihood value for the fitted penetrance model.}
\item{AIC}{Akaike information criterion (AIC) value of the model; AIC = 2*k - 2*logLik, where k is the number of parameters used in the model.}
}
\references{
Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07

Choi, Y.-H. and Briollais, L. (2011) An EM composite likelihood approach for multistage sampling of family data with missing genetic covariates, Statistica Sinica 21, 231-253.

Choi, Y.-H., Briollais, L., Green, J., Parfrey, P., and Kopciuk, K. (2014) Estimating successive cancer risks in Lynch Syndrome families using a progressive three-state model, Statistics in Medicine 33, 618-638.
}
\author{
Yun-Hee Choi
}
\seealso{
\code{\link{simfam}}, \code{\link{penmodel}}, \code{\link{print.penmodel}}, \code{\link{summary.penmodel}},
\code{\link{print.summary.penmodel}}, \code{\link{plot.penmodel}}, \code{\link{carrierprob}} 
}

\examples{
# Family data simulated with 20\% of members missing their genetic information.

set.seed(4321)
fam <- simfam(N.fam = 100, design = "pop+", base.dist = "Weibull", base.parms = c(0.01, 3),
       vbeta = c(1, 2), agemin = 20, allelefreq = 0.02, mrate = 0.2)
 
# EM algorithm for fitting family data with missing genotypes

fit <- penmodelEM(Surv(time, status) ~ gender + mgene, cluster = "famID", gvar = "mgene", 
       parms = c(0.01, 3, 1, 2), data = fam, design="pop+", robust = TRUE, 
       base.dist = "Weibull", method = "mendelian", mode = "dominant", q = 0.02)

# Summary of the model parameter estimates from the model fit by penmodelEM 

summary(fit)

# Plot the lifetime penetrance curves from model fit for gender and 
# mutation status groups along with their nonparametric penetrance curves 
# based on observed data excluding probands.
 
plot(fit)

}
\keyword{Penetrance model}
