\name{causalEff}
\alias{causalEff}
\title{Assessment of dynamic causal effects}
\description{All the pathwise causal lag shapes and the overall one connecting two or more variables are computed.}
\usage{causalEff(x, from = NULL, to = NULL, lag = NULL, cumul = FALSE, conf = 0.95,
  use.ns = FALSE)}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{from}{The name of the starting variable, or a vector containing the names of starting variables, which must be endogenous variables.}
  \item{to}{The name of the ending variable, which must be an endogenous variable.}
  \item{lag}{A non-negative integer or a vector of non-negative integers indicating the time lags to be considered. If \code{NULL}, the whole lag shapes will be considered.}
  \item{cumul}{Logical. If \code{TRUE}, cumulative causal effects are returned. Default is \code{FALSE}.}
  \item{conf}{The confidence level. Default is 0.95.}
  \item{use.ns}{A logical value indicating whether edges without statistically significant causal effect (at level \code{conf}) should be considered or not. If \code{FALSE} (the default), they will be ignored.}
}
\details{A \emph{pathwise causal lag shape} is the set of causal effects associated to a path at different time lags.
An \emph{overall causal lag shape} is the set of overall causal effects of a variable on another one at different time lags.

Note that, due to the properties of the multiple linear regression model,
causal effects are net of the influence of the group factor and exogenous variables.
}
\value{A list containing several matrices including point estimates, standard errors and asymptotic confidence intervals (at level \code{conf}) for all the pathwise causal lag shapes and the overall one connecting the starting variables to the ending variable.
}
\note{Value \code{NULL} is returned if one of the following occurs:
(i) no significant path at confidence level \code{conf} exists connecting the starting variables to the ending variable;
(ii) the requested path does not exist or is not significant at confidence level \code{conf}.
Note that the edges between the starting variables and their respective parents are deleted
as a consequence of intervention. %(Pearl, 2000, page 24).
See Magrini (2018) for technical details on causal inference in distributed-lag linear structural equation models.
}
\references{
A. Magrini (2018). Linear Markovian models for lag exposure assessment. Biometrical Letters, 55(2): 179-195. DOI: 10.2478/bile-2018-0012.

%J. Pearl (2000). Causality: Models, Reasoning, and Inference. Cambridge University Press. Cambridge, UK. ISBN: 978-0-521-89560-6
%
%J. Pearl (2012). The Causal Foundations of Structural Equation Modelling. In: R. H. Hoyle (ed.), Handbook of Structural Equation Modelling, Chapter 5. Guilford Press, New York, US-NY. ISBN: 978-16-062-3077-0
}
\seealso{\link{dlsem}; \link{lagPlot}.}
\examples{
data(industry)
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
causalEff(indus.mod,from="Job",to="Pollution",lag=c(0,5,10,15),cumul=TRUE)
}