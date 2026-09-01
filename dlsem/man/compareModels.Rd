\name{compareModels}
\alias{compareModels}
\title{Comparison among different distributed-lag linear structural equation models}
\description{Several competing distributed-lag linear structural equation models are compared based on information criteria.}
\usage{compareModels(x)}
\arguments{
  \item{x}{A list of 2 or more objects of class \code{dlsem} estimated on the same data.}
}
\value{A data.frame with one record for each model in \code{x} on the following quantities: log-likelihood, number of parameters, Akaike Information Criterion (AIC),
%corrected Akaike Information Criterion (AICc) 
Bayesian Information criterion (BIC).}
\note{
In order to keep the sample size constant, only the non-missing residuals across all the models are considered (see Magrini, 2020, for details).}
\references{
H. Akaike (1974). A New Look at the Statistical Identification Model. \emph{IEEE Transactions on Automatic Control}, 19, 716-723. DOI: 10.1109/TAC.1974.1100705

%J. E. Cavanaugh (1997), Unifying the derivations of the Akaike and corrected Akaike information criteria. \emph{Statistics & Probability Letters}, 31, 201-208.

A. Magrini (2020). A family of theory-based lag shapes for distributed-lag linear regression. To be appeared on \emph{Italian Journal of Applied Statistics}.

G. Schwarz (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, 6, 461-464. DOI: 10.1214/aos/1176344136
}
\seealso{\link{dlsem}.}
\examples{
data(industry)

# model with endpoint-contrained quadratic lag shapes
indus.code <- list(
  Consum~ecq(Job,0,5),
  Pollution~ecq(Job,1,8)+ecq(Consum,1,7)
  )
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
  
# model with gamma lag shapes
indus.code_2 <- list(
  Consum~gam(Job,0.85,0.2),
  Pollution~gam(Job,0.95,0.05)+gam(Consum,0.9,0.15)
  )
indus.mod_2 <- dlsem(indus.code_2,group="Region",exogenous=c("Population","GDP"),data=industry,
  log=TRUE)
  
compareModels(list(indus.mod,indus.mod_2))
}