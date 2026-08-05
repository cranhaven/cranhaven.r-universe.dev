\name{generate_longitudinal_mixed_type_data}
\alias{generate_longitudinal_mixed_type_data}
\title{
Generate an Artificial Longitudinal Dataset with Mixed-type Outcomes
}
\description{
Five different longitudinal outcomes are sampled accoring to the model described by Vávra et al. (2024). Column \code{i} is the unit id, \code{j} is the number of observation for \code{i}th unit. Numeric \code{ynum}, count \code{ypoi}, binary \code{ybin}, ordinal \code{yord} and categorical \code{ycat} outcomes are sampled according to predictors \code{pred_...} which are constructed from the intercept, time covariate \code{x}, categorical covariate \code{f} and the random effects \code{b_...}. The definition of the predictor can be adjusted.
}
\usage{
generate_longitudinal_mixed_type_data(n, n_i = 4, G = 2, timepar = "parallel", 
  catpar = "no", catsubjfix = TRUE, xin1 = 1, Sigma, 
  Kord = 5, Kcat = 4, kspec_bi_cat = FALSE)
}
\arguments{
  \item{n}{
number of sample units.
}
  \item{n_i}{
number of observations per unit.
}
  \item{G}{
number of clusters, only \code{G=2} and \code{G=3} are predefined.
}
  \item{timepar}{
a string declaring what is the parametrization of time covariate \code{x}. One of \code{no}, \code{parallel}, \code{cross}, \code{spline}, \code{mix}.
}
  \item{catpar}{
a string declaring what is the parametrization of binary covariate \code{f}. One of \code{no}, \code{fixed}, \code{group}.
}
  \item{catsubjfix}{
a logical value; should the values of categorical covariate be fixed for the same unit or alternate among observations?
}
  \item{xin1}{
\code{1/xin1} is the width of observational window for one unit, so \code{xin1} declares how many times does the observational period of one unit fit into the overall data gathering period.
}
  \item{Sigma}{
the covariance matrix for the random intercepts (of dimension 5, one for each outcome). Can be a list of covariance matrices, in such a case the model assumes group-specific covariance matrix.
}
  \item{Kord}{
number of categories for ordinal outcome.
}
  \item{Kcat}{
number of categories for categorical outcome (only up to 4 categories).
}
  \item{kspec_bi_cat}{
a logical value; should the random effects be specific to each level of categorical outcome? If so, the dimension of Sigma should be larger.
}
}
\details{
The function is limitted in the number of clusters \code{G} since it is quite laborious to define all the true values of model parameters (e.g. effects \eqn{\beta}).

Time covariate \code{x} is sampled so that observations for one unit are close to each other. First, half-length of the observational window \code{xmid = 1/(2xin1)} is defined. Then, values for a unit are sampled by uniform distribution on \code{[-xmid, xmid]} and randomly shifted by multiples of \code{xmid}. Time covariate is limitted to interval [0, 1]. 

The spline parametrization (\code{library("splines")}) of the time covariate is of \code{degree = 2} and has \code{knots = c(0, 0.25, 0.5, 0.75, 1)} 

Binary covariate \code{f} is sampled with probability 0.5 either once per unit (\code{catsubjfix = TRUE}) or repeatedly for each observation (\code{catsubjfix = FALSE}). 

The group allocation indicator \code{g} is sampled uniformly on \{1, \ldots, G\} for each unit. 

Then the linear predictors for each observation and outcome are created according to selected parametrization of time covariate \code{x} (\code{timepar}) and parametrization of binary covariate \code{f} (\code{catpar}). The intercepts are always group-specific which ensures at least some difference among clusters. 

Choices for \code{timepar}:
\describe{
\item{no}{covariate \code{x} does not modify the predictor,}
\item{parallel}{clusters have the same linear trend in time \code{x} but different intercepts,}
\item{cross}{clusters have different linear trends in time \code{x} (one is inscreasing, another decreasing),}
\item{spline}{each cluster has a different randomly generated spline parametrization,}
\item{mix}{outcomes have different selection of all of the above (currently, numeric is \code{spline}, count is \code{cross}, binary is \code{parallel}, ordinal is \code{spline}, categorical is \code{cross}).}
}

Choices for \code{catpar}:
\describe{
\item{no}{covariate \code{f} does not modify the predictor,}
\item{fixed}{clusters have the same effect of \code{f},}
\item{group}{clusters have different effects of \code{f}.}
}

Numerical outcome is sampled from normal distribution with group-specific standard deviations (from 0.5 to 1.0). 

Count outcome is sampled from Poisson distribution with rate made by \code{exp()} of the predictor.

Binary outcome (0 or 1) is sampled with probability computed by \code{\link[stats]{plogis}}.

Ordinal outcome (ordered factor) is sampled by probabilities computed by sequential differences of \code{\link[stats]{plogis}} applied to shifted predictor.

Categorical outcome (unordered factor) is sampled by probabilities computed by \code{softmax} function from several predictors specific for each category; \code{softmax(x) = exp(x) / sum(exp(x))} where \code{x} is \code{Kcat}-dimensional. By default, predictor for the first category is set to 0 for identifiability purposes.
}
\value{
A list containing:
\item{data }{a data frame with covariates, random effects, predictors and sampled outcomes,}
\item{sd_num }{a list of true values of standard deviations for numeric covariate depending on the total number of clusters \code{G},}
\item{beta_num }{a list of true values (named vector) of \eqn{\beta} parameters for numeric outcome for each cluster,}
\item{beta_poi }{a list of true values (named vector) of \eqn{\beta} parameters for count outcome for each cluster,}
\item{beta_bin }{a list of true values (named vector) of \eqn{\beta} parameters for binary outcome for each cluster,}
\item{beta_ord }{a list of true values (named vector) of \eqn{\beta} parameters for ordinal outcome for each cluster,}
\item{c_ord }{a list of true values of intercepts specific to each level of the ordinal outcome for each cluster,}
\item{beta_cat }{a list of true values (matrix with named columns) of \eqn{\beta} parameters for categorical outcome for each cluster.}
}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Implemented by Jan Vávra for performing a simulation study in Vávra et al. (2024).
}
\examples{
n <- 100      
n_i <- 4       
G <- 2         
timepar <- "mix" 
catpar <- "no"  
catsubjfix <- TRUE 
xin1 <- 1        
Kord <- 5        
Kcat <- 4        
kspec_bi_cat <- FALSE
corr <- matrix(c(1, -0.5, -0.5, -0.4, -0.4,
                 -0.5, 1, 0.3, 0.4, 0.5,
                 -0.5, 0.3, 1, 0.2, 0.3,
                 -0.4, 0.4, 0.2, 1, 0.1,
                 -0.4, 0.5, 0.3, 0.1, 1), byrow = TRUE, ncol = 5)
sdSigmas <- c(0.5, 0.5, 0.5, 0.5, 0.5)
Sigma <- diag(sdSigmas) %*% corr %*% diag(sdSigmas)

set.seed(31415)
ldata <- generate_longitudinal_mixed_type_data(n = n, n_i = n_i,
                     G = G, timepar = timepar,
                     catpar = catpar, catsubjfix = catsubjfix,
                     xin1 = xin1, Sigma = Sigma,
                     Kord = Kord, Kcat = Kcat, kspec_bi_cat = kspec_bi_cat)
data <- ldata$data
head(data)

# The same as the "longitudinal_mixed_type_data"
# which will be used in other examples
data("longitudinal_mixed_type_data")
}
