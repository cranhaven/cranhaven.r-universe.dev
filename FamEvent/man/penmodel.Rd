\name{penmodel}
\alias{penmodel}
\title{Fit a penetrance model}
\description{
Fits a penetrance model for family data based on a prospective likelihood with ascertainment correction and provides model parameter estimates. 
}
\usage{
penmodel(formula, cluster = "famID", gvar = "mgene", parms, cuts = NULL, data, 
design = "pop", base.dist = "Weibull", frailty.dist = "none",
agemin = NULL, robust = FALSE)
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
Vector of cut points that define the intervals where the hazard function is constant. The \code{cuts} should be specified when \code{base.dist="piecewise"} and must be strictly positive and finite. Default is \code{NULL}.
}
\item{data}{
 Data frame generated from \code{\link{simfam}} or data frame containing variables named in the \code{formula} and specific variables: \code{famID}, \code{indID}, \code{gender}, \code{currentage}, \code{mgene}, \code{time}, \code{status} and \code{weight} with \code{attr(data,"agemin")} specified.
}
\item{design}{
Study design of the family data.  Possible choices are: \code{"pop", "pop+", "cli"}, \code{"cli+"} or \code{"twostage"}, where \code{"pop"} is for the population-based design with affected probands whose mutation status can be either carrier or non-carrier, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands, \code{"cli"} is for the clinic-based design that includes affected probands with at least one parent and one sib affected, \code{"cli+"} is similar to \code{"cli"} but with mutation carrier probands, and \code{"twostage"} is for the two-stage design with oversampling of high risks families. Default is \code{"pop"}.
	}
 \item{base.dist}{
Choice of baseline hazard distributions to fit. Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"}, \code{"gamma"}, \code{"logBurr"}, or \code{"piecewise"}. Default is \code{"Weibull"}.
}
 \item{frailty.dist}{
Choice of frailty distribution. Possible choices are: \code{"gamma"}, \code{"lognormal"}, or \code{"none"}. Default is \code{"none"}.
}
\item{agemin}{
Minimum age of disease onset or minimum age. Default is \code{NULL}.
}
\item{robust}{
Logical; if \code{TRUE}, the robust `sandwich' standard errors and variance-covariance matrix are provided, otherwise the conventional standard errors and variance-covariance matrix are provided.
}
}
\details{
The penetrance model is fitted to family data with a specified baseline hazard distribution, 
\ifelse{html}{ \out{ h(t|x<sub>s</sub>, x<sub>g</sub>) = h<sub>0</sub>(t - t<sub>0</sub>)  exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>), }}{ \deqn{ h(t|x_s, x_g) = h_0(t - t_0) \exp(\beta_s x_s+\beta_g x_g),}{ h(g|xs, xg) = h0(t - t0) * Z * exp(beta.s * xs + beta.g * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazards function specified by \code{base.dist}, which depends on the shape and scale parameters, \ifelse{html}{\out{&lambda;}}{\eqn{\lambda}{lambda}} and \ifelse{html}{\out{&rho;}}{\eqn{\rho}{rho}}; \ifelse{html}{\out{x<sub>s</sub>}}{\eqn{x_s}{xs}} indicates male (1) and female (0) and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicates carrier (1) or non-carrier (0) of a gene of interest (major gene). Additional covariates can be added to \code{formula} in the model.

For family data arising from population- or clinic-based study designs (\code{design="pop", "pop+"}, \code{"cli"}, or \code{"cli+"}), the parameters of the penetrance model are estimated using the ascertainment-corrected prospective likelihood approach (Choi, Kopciuk and Briollais, 2008).

For family data arising from a two-stage study design (\code{design="twostage"}), model parameters are estimated  using the composite likelihood approach (Choi and Briollais, 2011)

Note that the baseline parameters include \code{lambda} and \code{rho}, which represent the scale and shape parameters, respectively, and \code{eta}, additional parameter to specify for \code{"logBurr"} distribution. For the \code{"lognormal"} baseline distribution, \code{lambda} and \code{rho} represent the location and scale parameters for the normally distributed logarithm, where \code{lambda} can take any real values and \code{rho} > 0. For the other baselinse distributions, \code{lambda} > 0, \code{rho} > 0, and \code{eta} > 0. When a piecewise constant distribution is specified for the baseline hazards, \code{base.dist="piecewise"}, \code{baseparm} should specify the initial interval-constant values, one more than the cut points specified by\code{cuts}.

Transformed baseline parameters are used for estimation; log transformation is applied to both scale and shape parameters (\eqn{\lambda, \rho}) for \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"} and \code{"gamma"} baselines, to (\eqn{\lambda, \rho, \eta}) for \code{"logBurr"} and to the piecewise constant parameters for a \code{piecewise} baseline hazard. For \code{"lognormal"} baseline distribution, the log transformation is applied only to \eqn{\rho}, not to \eqn{\lambda}, which represents the location parameter for the normally distributed logarithm.

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

Choi, Y.-H., Kopciuk, K. and Briollais, L. (2008) Estimating Disease Risk Associated Mutated Genes in Family-Based Designs, Human Heredity 66, 238-251.

Choi, Y.-H. and Briollais (2011) An EM Composite Likelihood Approach for Multistage Sampling of Family Data with Missing Genetic Covariates, Statistica Sinica 21, 231-253.
}
\author{
Yun-Hee Choi
}
\seealso{
\code{\link{penmodelEM}}, \code{\link{simfam}}, \code{\link{penplot}}, \code{\link{print.penmodel}},
\code{\link{summary.penmodel}}, \code{\link{print.summary.penmodel}}, \code{\link{plot.penmodel}}
}
\examples{

# Family data simulated from population-based design using a Weibull baseline hazard 

set.seed(4321)
fam <- simfam(N.fam = 200, design = "pop+", variation = "none", base.dist = "Weibull", 
       base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35), agemin = 20, allelefreq = 0.02)
 
# Penetrance model fit for simulated family data

fit <- penmodel(Surv(time, status) ~ gender + mgene, cluster = "famID", design = "pop+",
       parms = c(0.01, 3, -1.13, 2.35), data = fam, base.dist = "Weibull")

# Summary of the model parameter estimates from the model fit

summary(fit)

# Plot the lifetime penetrance curves with 95% CIs from the model fit for specific  
# gender and mutation status groups along with their nonparametric penetrance curves  
# based on data excluding probands. 

plot(fit, add.KM = TRUE, conf.int = TRUE, MC = 100)
}
\keyword{Penetrance model}
