\name{isIndep}
\alias{isIndep}
\title{Conditional independence check}
\description{Conditional independence between two variables is checked using the d-separation criterion (Pearl, 2000, page 16 and following).}
\usage{isIndep(x, var1 = NULL, var2 = NULL, given = NULL, conf = 0.95, use.ns = FALSE)}
\arguments{
  \item{x}{An object of class \code{dlsem}.}
  \item{var1}{The name of the first variable.}
  \item{var2}{The name of the second variable.}
  \item{given}{A vector containing the names of conditioning variables. If \code{NULL}, marginal independence is checked.}
  \item{conf}{The confidence level for each edge: only edges with statistically significant causal effect at such confidence are considered. Default is 0.95.}
  \item{use.ns}{A logical value indicating whether edges without statistically significant causal effect (at level \code{conf}) should be considered or not. If \code{FALSE} (the default), they will be ignored.}
}
\value{Logical}
\note{Conditional independence is checked statically, that is the whole history of conditioning variables is supposed to be known.

The result is unchanged if arguments \code{var1} and \code{var2} are switched.

Dependence is a necessary but not sufficient condition for causation: see the discussion in Pearl (2000).}
\references{J. Pearl (2000). Causality: Models, Reasoning, and Inference. Cambridge University Press. Cambridge, UK. ISBN: 978-0-521-89560-6}
\seealso{\link{dlsem}.}
\examples{
data(industry)
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
isIndep(indus.mod,"Job","Pollution",given=c("Consum"))
}
