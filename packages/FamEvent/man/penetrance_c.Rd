\name{penetrance_c}
\alias{penetrance_c}
\title{
  Estimating Penetrances for competing risk models and confidence intervals
}
\description{
  Estimates the cumulative disease risks (penetrances) and confidence intervals for the event of interest in the presence of competing event given fixed values of covariates based on the fitted competing risk model. 
}
\usage{
  penetrance_c(fit, event = 1, fixed, age, CI = TRUE, MC = 100)
}

\arguments{
  \item{fit}{
    An object class of \code{'penmodel_c'}, a fitted model by \code{\link{penmodel_c}} function.
  }
  \item{event}{
    Event of interest (either 1 or 2) for penetrance estimation. Default value is 1.
  }
  \item{fixed}{
    list of vectors of fixed values of the covariates for both events used for penetrance calculation.
  }
  \item{age}{
    Vector of ages used for penetrance calculation.
  }
  \item{CI}{
    Logical; if \code{TRUE}, the 95\% confidence interval will be obtained using a Monte-Carlo method, otherwise no confidence interval will be provided. Default is \code{TRUE}.
  }
  \item{MC}{
    Number of simulated samples used to calculate confidence intervals with a Monte-Carlo method. 
    If \code{MC=0}, no confidence intervals will be calculated. Default value is 100.
  }

}
\details{
The cause-specific hazard for event \eqn{j} is specified as:
\ifelse{html}{\out{h<sub>j</sub>(t|x) = h<sub>0j</sub>(t) exp(&beta;<sub>j</sub> *x<sub>j</sub>),}}{
\deqn{ h_j(t|x) = h_{0j}(t) \exp(\beta_j x_j) ,}{hj(t|x) = h0j(t) exp(bj*xj) ,}}
where \ifelse{html}{\out{h<sub>0j</sub>(t)}}{\eqn{h_{0j}(t)}{h0j(t)}} is the baseline hazards function for event \eqn{j}, \ifelse{html}{\out{x<sub>j</sub>}}{\eqn{x_j}{xj}} is the vector of covariates associated with event \eqn{j} and \ifelse{html}{\out{&beta;<sub>j</sub>}}{\eqn{\beta_j}{bj}} is the vector of corresponding regression coefficients, \eqn{j = 1, 2}. 

The penetrance function for event \eqn{j} in the presence of competing risks based on cause-specific hazards (with no frailties assumed) model is defined as the probability of developing an event of interest by age \eqn{t} given fixed values of covariates \eqn{x} in the following form: 
\ifelse{html}{\out{ P(T < t, d = j |x) = &int;<sub>t0</sub><sup>t</sup> h<sub>j</sub>(u|x<sub>j</sub>)exp(-H<sub>1</sub>(u|x<sub>1</sub>)-H<sub>2</sub>(u|x<sub>2</sub>))du,}}{
\deqn{ P(T < t, d = j | x) = \int_{t_0}^t h_j(u|x_j) \exp(-H_1(u|x_1)-H_2(u|x_2))du }{
P(T < t, d = j | x) = \int_{t0}^t hj(u|xj)exp(-H1(u|x1)-H2(u|x2))du 
}}
where \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is the minimum age of onset, \eqn{d} is the type of event which takes \eqn{(j = 1, 2)}.

The shared frailty competing risks model is:
\ifelse{html}{\out{
h<sub>j</sub>(t|z<sub>j</sub>, x) = z<sub>j</sub>*h<sub>0j</sub>(t) exp(&beta;<sub>j</sub> *x<sub>j</sub>),}}{		
\deqn{ h_j(t|z_j, x_j) = z_j h_{0j}(t) \exp(\beta_j x_j) ,}{ hj(t|zj, xj) = zj h0j(t) exp(bj*xj), }}
where \ifelse{html}{\out{z<sub>j</sub>}}{\eqn{z_j}{zj}} is the shared frailty for event \eqn{j} within families whose distribution is specified by \code{frailty.dist}.

  The penetrance function for event \eqn{j} from the shared frailty competing risks model is obtained by integrating over the frailty distribution of \ifelse{html}{\out{G(z1, z2)}}{\eqn{G(z_1, z_2)}{G(z1, z2)}},
\ifelse{html}{\out{
P(T < t, d = j | x) = &int;<sub>t0</sub><sup>t</sup>
&int; &int; h<sub>j</sub>(u|x<sub>j</sub>, z<sub>j</sub>) exp(-H<sub>1</sub>(u|x<sub>1</sub> z<sub>j</sub>)-H<sub>2</sub>(u|x<sub>2</sub>, z<sub>j</sub>)) dG(z<sub>1</sub> dz<sub>2</sub>)du, }}{
\deqn{ P(T < t, d = j | x) = \int_{t_0}^t \int \int h_j(u|x_j, z_j) \exp(-H_1(u|x_1, z_1)-H_2(u|x_2, z_2))dG(z_1, z_2) du,}{
P(T < t, d = j | x) = \int_{t0}^t \int \int hj(u|xj, zj)exp(-H1(u|x1, z1)-H2(u|x2, z2)) dG(z1, z2) du,}}
where \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is the minimum age of onset, \eqn{d} is the type of event which takes \eqn{(j = 1, 2)}. 

See Choi et al. (2021) for more details about the penetrance functions.
  
  Calculations of standard errors of the penetrance estimates and 95\% confidence intervals (CIs) for the penetrance at a given age are based on Monte-Carlo simulations of the estimated penetrance model. A multivariate normal distribution is assumed for the parameter estimates, and \code{MC = n} sets of the parameters are generated from the multivariate normal distribution with the parameter estimates and their variance-covariance matrix.
  For each simulated set, a penetrance estimate is calculated at a given age by substituting the simulated parameters into the penetrance function.
  
  The standard error of the penetrance estimate at a given age is calculated by the standard deviation of penetrance estimates obtained from \eqn{n} simulations.
  
  The 95\% CI for the penetrance at a given age is calculated using the 2.5th and 97.5th percentiles of the penetrance estimates obtained from \eqn{n} simulations.
}
\value{
  Returns the following values:
  \item{age}{Ages at which the penetrances are calculated.}
  \item{penetrance}{Penetrance estimates at given ages.}
  \item{lower}{Lower limit of the 95\% confidence interval; simulation-based 2.5th percentile of the penetrance estimates.}
  \item{upper}{Upper limit of the 95\% confidence interval; simulation-based 97.5th percentile of the penetrance estimates.}
   \item{se}{Simulation-based standard errors of the penetrance estimates.}
}
\references{
Choi, Y.-H., Jung, H., Buys, S., Daly, M., John, E.M., Hopper, J., Andrulis, I., Terry, M.B., Briollais, L. (2021) A Competing Risks Model with Binary Time Varying Covariates for Estimation of Breast Cancer Risks in BRCA1 Families, Statistical Methods in Medical Research 30 (9), 2165-2183. https://doi.org/10.1177/09622802211008945.

Choi, Y.-H., Briollais, L., He, W. and Kopciuk, K. (2021) FamEvent: An R Package for Generating and Modeling Time-to-Event Data in Family Designs, Journal of Statistical Software 97 (7), 1-30. doi:10.18637/jss.v097.i07
}

\author{
  Yun-Hee Choi
}
\seealso{
  \code{\link{simfam_c}, \link{penmodel_c}}
}
\examples{
\dontrun{
set.seed(4321)
fam2 <- simfam_c(N.fam = 200, design = "pop+", variation = "frailty", competing = TRUE,
                base.dist = "Weibull", frailty.dist = "cgamma", depend=c(2, 2, 2), 
                base.parms = list(c(0.01, 3), c(0.01, 3)), 
                vbeta = list(c(-1.13, 2.35), c(-1, 2)), allelefreq = 0.02)

fit2 <- penmodel_c(Surv(time, status==1) ~ gender + mgene, 
                   Surv(time, status==2) ~ gender + mgene, 
                   cluster = "famID", gvar = "mgene", frailty.dist = "cgamma",
                   parms = list(c(0.01, 3, -1, 2.3), c(0.01, 3, -1, 2), c(2, 2, 2)), 
                   data = fam2, design = "pop+", base.dist = "Weibull", 
                   agemin = NULL, robust = TRUE)

# Compute penetrance estimates for event 1 for male carriers at age 40, 50, 60, 70 and
# their 95% CIs based on 100 Monte Carlo simulations.
 
penetrance_c(fit2, event = 1, fixed = list(c(1,1), c(1,1)), 
             age = c(40, 50, 60, 70), CI = TRUE, MC = 200)
}
}
\keyword{Penetrance estimation}