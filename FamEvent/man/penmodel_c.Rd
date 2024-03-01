\name{penmodel_c}
\alias{penmodel_c}
\title{Fit a penetrance model for competing risks data}
\description{
Fits a competing risks model for family data with ascertainment correction and provides model parameter estimates. 
}
\usage{
penmodel_c(formula1, formula2, cluster = "famID", gvar = "mgene", 
parms, cuts = NULL, data,  design = "pop", base.dist = "Weibull", 
frailty.dist = "none", agemin = NULL, robust = FALSE)
}
\arguments{
\item{formula1}{A formula expression for event 1 as for other regression models. The response should be a survival object as returned by the \code{Surv} function. See the documentation for \code{Surv}, \code{lm} and \code{formula} for details.
}
\item{formula2}{A formula expression for event 2 as for other regression models. The response should be a survival object as returned by the \code{Surv} function. See the documentation for \code{Surv}, \code{lm} and \code{formula} for details.
}
\item{cluster}{
Name of cluster variable. Default is \code{"famID"}.
}
\item{gvar}{Name of genetic variable. Default is \code{"mgene"}.}
\item{parms}{
list of Vectors of initial values for the parameters in each model including baseline parameters and regression coefficients and frailty parameters. \code{parms = list(c(baseparm1, coef1), c(baseparms2, coef2), c(k1, k2)}, where \code{baseparm1} and \code{baseparm2} include the initial values for baseline parameters used for each \code{base.dist}; \code{coef1} and \code{coef2} include the initial values for regression coefficients for the variables specified in \code{formula1} and \code{formula2}, respectively, and \code{c(k1, k2)} are the initial values for frailty parameters used for the specified frailty.distribution.
See Details for more details.
}
\item{cuts}{ 
Vector of cut points that define the intervals when \code{base.dist="piecewise"} is specified and must be strictly positive and finite and greater than \code{agemin}. Default is \code{NULL}.
}
\item{data}{
Data frame generated from \code{\link{simfam}} or data frame containing variables named in the \code{formula} and specific variables: \code{famID}, \code{indID}, \code{gender}, \code{currentage}, \code{mgene}, \code{time}, \code{status} and \code{weight} with \code{attr(data,"agemin")} specified.
}
\item{design}{
Study design of the family data.  Possible choices are: \code{"pop"} and \code{"pop+"}, where \code{"pop"} is for the population-based design with affected probands whose mutation status can be either carrier or non-carrier, \code{"pop+"} is similar to \code{"pop"} but with mutation carrier probands. Default is \code{"pop"}.
}
 \item{base.dist}{
Vector of two baseline hazard distributions to be fitted for competing events. Possible choices for each event are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"}, \code{"gamma"}, \code{"logBurr"}, or \code{"piecewise"}. If only one distribution is specified, the same distribution will be assumed for both events. A vector of two distributions should be specified if different baseline distributions are assumed for different events.
Default is \code{c("Weibull", "Weibull")}.
}
\item{frailty.dist}{
Choice of frailty distribution to fit a shared frailty model for competing events. Possible choices are: \code{"gamma"} for independent gamma, \code{"lognormal"} for independent log-normal, \code{"cgamma"} for correlated gamma, \code{"clognormal"} for correlated log-normal distributions, or \code{"none"} can be chosen to ignoring frailties in the model assuming no residual familial correlation given covariates. 
Default is \code{"gamma")}.
}

\item{agemin}{
Minimum age of disease onset or minimum age. Default is \code{NULL}.
}
\item{robust}{
Logical; if \code{TRUE}, the robust `sandwich' standard errors and variance-covariance matrix are provided, otherwise the conventional standard errors and variance-covariance matrix are provided.
}
}
\details{
The shared frailty comepting risks model is fitted to family data with specified baseline hazard distributions and frailty distribution

Event 1:

\ifelse{html}{
\out{<p style="text-align:center"> h<sub>1</sub>(t|X,Z) = h<sub>01</sub>(t - t<sub>0</sub>) Z<sub>1</sub> exp(&beta;<sub>s1</sub> * x<sub>s</sub> + &beta;<sub>g1</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X,Z_1) = h_{01}(t - t_0) Z \exp(\beta_{s1} x_s+\beta_{g1} x_g) ,}{ h(g|X,Z) = h01(t - t0) * Z1 * exp(\beta s1 * xs + \beta g1 * xg) ,}}

Event 2:

\ifelse{html}{
\out{<p style="text-align:center"> h<sub>2</sub>(t|X,Z) = h<sub>02</sub>(t - t<sub>0</sub>) Z<sub>2</sub> exp(&beta;<sub>s2</sub> * x<sub>s</sub> + &beta;<sub>g2</sub> * x<sub>g</sub>), </p>}}{	
		\deqn{ h(t|X,Z_1) = h_{02}(t - t_0) Z \exp(\beta_{s2} x_s+\beta_{g2} x_g) ,}{ h(g|X,Z) = h02(t - t0) * Z1 * exp(\beta s2 * xs + \beta g2 * xg) ,}}
		
where \ifelse{html}{\out{h<sub>01</sub>(t)}}{\eqn{h_{01}(t)}{h01(t)}} and \ifelse{html}{\out{h<sub>02</sub>(t)}}{\eqn{h_{02}(t)}{h02(t)}} are the baseline hazard functions for event 1 and event 2, respectively, which can be specified by \code{base.dist}. \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{Z<sub>1</sub>}}{\eqn{Z_1}{Z1}} and \ifelse{html}{\out{Z<sub>2</sub>}}{\eqn{Z_2}{Z2}} are frailties shared within families for each event and follow either a gamma, log-normal, correlateg gamma, or correlated log-normal distributions, which can be specified by \code{frailty.dist}. \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively. Additional covariates can be added to \code{formula1} for event 1 and \code{formula2} for event 2 in the model. 


\emph{Choice of frailty distributions for competing risk models}

\code{frailty.dist = "gamma"} shares the frailties within families generated from a gamma distribution independently for each competing event, where
\ifelse{html}{\out{Z<sub>j</sub>}}{\eqn{Z_j}{Zj}} follows Gamma(\ifelse{html}{\out{k<sub>j</sub>}}{\eqn{k_j}{kj}}, \ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}}). 

\code{frailty.dist = "lognormal"} shares the frailties within families generated from a log-normal distribution independently for each competing event, where 
\ifelse{html}{\out{Z<sub>j</sub>}}{\eqn{Z_j}{Zj}} follows log-normal distribution with mean 0 and variance (\ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}}.

\code{frailty.dist = "cgamma"} shares the frailties within families generated from a correlated gamma distribution to allow the frailties between two events to be correlated, where the correlated gamma frailties (\ifelse{html}{\out{Z<sub>1</sub>, Z<sub>2</sub>}}{\eqn{Z_1, Z_2}{Z1, Z2}}) are generated with three independent gamma frailties (\ifelse{html}{\out{Y<sub>0</sub>, Y<sub>1</sub>, Y<sub>2</sub>}}{\eqn{Y_0, Y_1, Y_2}{Y0, Y1, Y2}}) as follows:

\ifelse{html}{\out{Z<sub>1</sub> = k<sub>0</sub>/(k<sub>0</sub> + k<sub>1</sub>) Y<sub>0</sub> + Y<sub>1</sub>} }{\eqn{Z_1 = (k_0/(k_0 + k_1) Y_0 + Y_1}{Z1 = k0/(k0+k1)Y0 + Y1} }

\ifelse{html}{\out{Z<sub>2</sub> = k<sub>0</sub>/(k<sub>0</sub> + k<sub>2</sub>) Y<sub>0</sub> + Y<sub>2</sub>} }{\eqn{Z_2 = (k_0/(k_0 + k_2) Y_0 + Y_2}{Z2 = k0/(k0+k2)Y0 + Y2} }

where \ifelse{html}{\out{Y<sub>0</sub>}}{\eqn{Y_0}{Y0}} from Gamma(\ifelse{html}{\out{k<sub>0</sub>}}{\eqn{k_0}{k0}}, \ifelse{html}{\out{1/k<sub>0</sub>}}{\eqn{1/k_0}{1/k0}});

\ifelse{html}{\out{Y<sub>1</sub>}}{\eqn{Y_1}{Y1}} from Gamma(\ifelse{html}{\out{k<sub>1</sub>}}{\eqn{k_1}{k1}}, \ifelse{html}{\out{1/(k<sub>0</sub> + k<sub>1</sub>)}}{\eqn{1/(k_0+k_1)}{1/(k0+k1)}});

\ifelse{html}{\out{Y<sub>2</sub>}}{\eqn{Y_2}{Y2}} from Gamma(\ifelse{html}{\out{k<sub>2</sub>}}{\eqn{k_2}{k2}}, \ifelse{html}{\out{1/(k<sub>0</sub> + k<sub>2</sub>)}}{\eqn{1/(k_0+k_2)}{1/(k0+k2)}}).

\code{frailty.dist = "clognormal"} shares the frailties within families generated from a correlated log-normal distribution where 
\ifelse{html}{\out{log(Z<sub>j</sub>)}}{\eqn{log(Z_j)}{log(Zj)}} follows  a normal distribution with mean 0, variance \ifelse{html}{\out{1/k<sub>j</sub>}}{\eqn{1/k_j}{1/kj}} and correlation between two events \ifelse{html}{\out{k<sub>0</sub>}}{\eqn{k_0}{k0}}.

\code{depend} should specify the values of related frailty parameters: \code{c(k1, k2)} with \code{frailty.dist = "gamma"} or \code{frailty.dist = "lognormal"}; \code{c(k1, k2, k0)} for \code{frailty.dist = "cgamma"} or \code{frailty.dist = "clognormal"}. 


More details about the competing risks model for family data arising from population-based study designs (\code{design="pop", "pop+"} and their inference procedure based on the ascertainment-corrected likelihood approach can be found in Choi et al., 2021.


Note that the baseline parameters include \code{lambda} and \code{rho}, which represent the scale and shape parameters, respectively, and \code{eta}, additional parameter to specify for \code{"logBurr"} distribution. For the \code{"lognormal"} baseline distribution, \code{lambda} and \code{rho} represent the location and scale parameters for the normally distributed logarithm, where \code{lambda} can take any real values and \code{rho} > 0. For the other baselinse distributions, \code{lambda} > 0, \code{rho} > 0, and \code{eta} > 0. When a piecewise constant distribution is specified for the baseline hazards, \code{base.dist="piecewise"}, \code{baseparm} should specify the initial interval-constant values, one more than the cut points specified by\code{cuts}.

Transformed baseline parameters are used for estimation; log transformation is applied to both scale and shape parameters (\eqn{\lambda, \rho}) for \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"} and \code{"gamma"} baselines, to (\eqn{\lambda, \rho, \eta}) for \code{"logBurr"} and to the piecewise constant parameters for a \code{piecewise} baseline hazard. For \code{"lognormal"} baseline distribution, the log transformation is applied only to \eqn{\rho}, not to \eqn{\lambda}, which represents the location parameter for the normally distributed logarithm.

Calculations of penetrance estimates and their standard errors and 95\% confidence intervals at given ages can be obtained by \code{\link{penetrance}} function via Monte-Carlo simulations of the estimated penetrance model.

}
\value{
Returns an object of class \code{'penmodel_c'}, including the following elements:

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
Choi, Y.-H., Jung, H., Buys, S., Daly, M., John, E.M., Hopper, J., Andrulis, I., Terry, M.B., Briollais, L. (2021) A Competing Risks Model with Binary Time Varying Covariates for Estimation of Breast Cancer Risks in BRCA1 Families, Statistical Methods in Medical Research 30 (9), 2165-2183. https://doi.org/10.1177/09622802211008945.

Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07
}
\author{
Yun-Hee Choi
}
\seealso{
\code{\link{simfam_c}}, \code{\link{penplot_c}}, \code{\link{print.penmodel_c}},
\code{\link{summary.penmodel_c}}, \code{\link{print.summary.penmodel_c}}, \code{\link{plot.penmodel_c}}
}
\examples{

# Competing risk family data simulated from population-based design 
# using Weibull baseline hazards with gamma frailty distribution.
\dontrun{
set.seed(4321)
fam1 <- simfam_c(N.fam = 200, design = "pop+", variation = "frailty", competing=TRUE,
              base.dist = "Weibull", frailty.dist = "cgamma", depend=c(0.5, 1, 0.5), 
              allelefreq = 0.02, base.parms = list(c(0.01, 3), c(0.01, 3)), 
              vbeta = list(c(-1.13, 2.35), c(-1, 2)))

# Fitting shared gamma frailty Penetrance model for simulated competing risk data

fit1 <- penmodel_c(
        formula1 = Surv(time, status==1) ~ gender + mgene, 
        formula2 = Surv(time, status==2) ~ gender + mgene,
        cluster = "famID", gvar = "mgene", design = "pop+",  
        parms = list(c(0.01, 3, -1, 2), c(0.01, 3, -1, 2), c(0.5, 1)),
        base.dist = "Weibull", frailty.dist = "gamma", data = fam1, robust = TRUE)

# Fitting shared correlated gamma frailty Penetrance model for simulated competing risk data

fit2 <- penmodel_c(
        formula1 = Surv(time, status==1) ~ gender + mgene, 
        formula2 = Surv(time, status==2) ~ gender + mgene,
        cluster = "famID", gvar = "mgene", design = "pop+",  
        parms = list(c(0.01, 3, -1, 2), c(0.01, 3, -1, 2), c(0.5, 1, 0.5)),
        base.dist = "Weibull", frailty.dist = "cgamma", data = fam1, robust = TRUE)

# Summary of the model parameter estimates from the model fit

summary(fit1)

# Plot the lifetime penetrance curves with 95% CIs from the model fit for specific  
# gender and mutation status groups along with their nonparametric penetrance curves  
# based on data excluding probands. 

plot(fit1, add.CIF = TRUE, conf.int = TRUE, MC = 100)
}
}
\keyword{Penetrance model}
