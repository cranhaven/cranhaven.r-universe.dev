\name{sim.glm}
\alias{sim.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of glm
}
\description{
function for predictions from the results of model fitting functions (\code{\link{fit.glm}}) or
(\code{\link{glm}})
}
\usage{
sim.glm(fit, datapred, fam.glm = "gaussian", occ.cond = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
climatic variable fitted returned from "glm" which inherits from the class "lm".
}
  \item{datapred}{
Data frame that contains dates, climatic variables and all covariates used to fit the variable to simulate
}
  \item{fam.glm}{
family objects to specify probability distribution used for the model ("gaussian", "gaussian-hetero", "binomial" or "Gamma")
}
  \item{occ.cond}{
character object that specifies the name of the occurrence variable if that exists 
}
}

\value{
Value returned belong to the same class of its first argument
}

\seealso{
\code{\link{glm}}, \code{\link{fit.glm}}
}

\examples{
temp_fitted=glm(temp~Rh, family=gaussian, data=myclimatic_data)
temp.sim=sim.glm(temp_fitted, myclimatic_data, fam.glm = "gaussian", occ.cond = NULL)
}
