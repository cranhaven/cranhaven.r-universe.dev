\name{robustsae}
\alias{robustsae}

\title{Robust Small Area Estimation Modeling Both Means and Variances}

\description{This function provides full Bayesian Analysis for specific area-level small area models when data are provided for modeling both the mean and the variance.}

\usage{
robustsae(formula, S2, ni, nsim = 1000, burnin = 500, data, truemean)}

\arguments{
  \item{formula}{a symbolic description of the model to be fitted. The details of model
                  specification are given under Details.}

  \item{S2}{a vector contain the sampling variances which are given for estimating the true variances.}

  \item{ni}{a vector containing the sample sizes for each area.}

  \item{nsim}{user-specified number of MCMC draws. See German (2006).}

  \item{burnin}{the number of burning iterations for the sampler. See German (2006).}

  %\item{thin}{the thinning interval used in the simulation. See Gelman (2006).}

  \item{data}{an optional data frame containing the variables named in \code{formula}, \code{S2} and \code{ni}.}

  \item{truemean}{true mean values for each area.}
}

\details{
Let \eqn{\theta_i} denotes interest parameter for each area i, \eqn{x_i} the available area-specific auxiliary data, \eqn{\beta} the regression coefficients and \eqn{m} the number of small areas. A typical area level model is given by
\deqn{y_i= x_i \beta + u_i + e_i, (i = 1, \ldots, m).}
Assume that the random effects \eqn{u_i} and the sampling errors \eqn{e_i} are to be independently distributed  with the \eqn{u_i ~ N(0, \sigma^2)} and the \eqn{e_i ~ N(0, v_i)}. To foster robustness in small area estimation procedures, student t distribution is used for the random effects. Also, due to the availability of additional data purported to estimate the error variances, this considers modeling of both the means and the variances. 

The robust Bayesian small area estimation model is
\deqn{y_i | \theta_i ~ N(\theta_i, v_i)}

\deqn{S_i^2 | v_i  ~ Gamma((n_i-1)/2,  1/(2v_i))}
\deqn{ \theta_i | \beta, \sigma^2, df ~ t(x_i\beta, \sigma^2, df)},
where \eqn{df} is degrees of freedom parameter.
For a full Bayesian analysis, this function uses the modified Jeffrey' prior which is the product of the general Jeffrey' prior and \eqn{e^(-a/(2*\sigma^2))} where \eqn{a} is chosen as 1:

\deqn{\pi(\beta) ~ 1}

\deqn{\pi(v_i)~ 1/v_i}

\deqn{\sigma^2 ~ Inv-Gamma(p/2, a/2),  for a>0}

\deqn{\pi(df) ~ df^{-1/2} (df+1)^{p/2 -1} (df+3)^{- p/2 - 1/2}   }

The estimates of interest parameters are obtained by Rao-Balackwellization with Gibbs sampling with Metropolis-Hastings algorithm.
}

\value{The function returns a object of class \code{"robustsae"} containing the following components:
\item{mean}{ Rao-Balackwellization estimates of theta's}
\item{variance}{ Rao-Balackwellization estimates of v's}
\item{Criteria}{ a list containing the following comparison criteria : Returns NA if \code{truemean} is not provided. 
\itemize{
\item{\code{ASD}: } average squared deviation, defined as \eqn{1/m \sum_{i=1}^{m} (\hat{\theta}_i - \theta_i)^2}
\item{\code{AAB}: } average absolute bias, defined as \eqn{1/m \sum_{i=1}^{m} |\hat{\theta}_i - \theta_i|}
\item{\code{ASRB}: } average squared relative bias, defined as \eqn{1/m \sum_{i=1}^{m} ((\hat{\theta}_i - \theta_i)/{\theta_i})^2}
\item{\code{ARB}: } average relative bias, defined as \eqn{1/m \sum_{i=1}^{m}
|(\hat{\theta}_i - \theta_i)/\theta_i|}
}}
}

\references{
Rao, J. N. K. (2003) \emph{ Small Area Estimation}. John Wiley and Sons.

Chip, S., and Green berg, E. (1995). Understanding the Metropolis-Hastings Algorithm.
\emph{The American Statistician}, \bold{49}, 327-335.
}

\author{
Malay Ghosh, Jiyoun Myung, Fernando Moura
}


\examples{
# If there is truemean data,
# load data set
data(BZdata)
attach(BZdata)

result <- robustsae(y ~ X1 + X2, S2, ni = BZdata$ni, nsim = 1000, burnin = 500, 
                      data = BZdata, truemean = truemean)
result

detach(BZdata)

# If there is no truemean data,
#load data set
data(corndata)
attach(corndata)

result2 <- robustsae(Xi ~ Z1i, Si^2, ni=corndata$ni, data = corndata) # no truemean
result2$mean
result2$variance

detach(corndata)
}

\keyword{Hierarchical Bayes, Full Bayesian Analysis}
