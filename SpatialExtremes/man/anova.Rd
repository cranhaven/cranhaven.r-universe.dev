\name{anova}
\alias{anova}
\alias{anova.maxstab}
\alias{anova.spatgev}

\title{Anova Tables}
\description{
  Computes analysis of deviance for objects of class 'maxstab' or 'spatgev'.
}
\usage{
\method{anova}{maxstab}(object, object2, method = "RJ", square = "chol", \dots)
\method{anova}{spatgev}(object, object2, method = "RJ", square = "chol", \dots)
}
\arguments{
  \item{object, object2}{Two objects of class 'maxstab' or 'spatgev'.}
  \item{method}{Character string. Must be one of "CB" or "RJ" for the
    Chandler and Bate or the Rotnitzky and Jewell approaches
    respectively. See function
    \code{\link[=profile.maxstab]{profile}}.}
  \item{square}{The choice for the matrix square root. This is only
    useful for the 'CB' method. Must be one of 'chol' (Cholesky) or
    'svd' (Singular Value Decomposition).}
  \item{\dots}{Other options to be passed to the \code{\link{anova}}
    function.}
}
\details{
  As ''maxstab'' objects are fitted using pairwise likelihood, the model
  is misspecified. As a consequence, the likelihood ratio statistic is
  no longer \eqn{\chi^2}{\chi^2} distributed. To compute the anova
  table, we use the methodology proposed by Rotnitzky and Jewell to
  adjust the distribution of the likelihood ratio statistic.
}
\value{
This function returns an object of class anova. These objects
represent analysis-of-deviance tables. 
}
\references{
  Chandler, R. E. and Bate, S. (2007) Inference for clustered data using
  the independence loglikelihood \emph{Biometrika}, \bold{94},
  167--183.
  
  Rotnitzky, A. and Jewell, N. (1990) Hypothesis testing of regression
  parameters in semiparametric generalized linear models for cluster
  correlated data. \emph{Biometrika} \bold{77}, 485--497.
}
\author{Mathieu Ribatet}
\seealso{\code{\link{fitmaxstab}}, \code{\link{fitspatgev}},
  \code{\link[=profile.maxstab]{profile}}, \code{\link{TIC}}}
\examples{
##Define the coordinates of each location
n.site <- 30
locations <- matrix(rnorm(2*n.site, sd = sqrt(.2)), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(50, locations, cov.mod = "gauss", cov11 = 100, cov12 =
25, cov22 = 220)

##Now define the spatial model for the GEV parameters
param.loc <- -10 + 2 * locations[,2]
param.scale <- 5 + 2 * locations[,1] + locations[,2]^2
param.shape <- rep(0.2, n.site)

##Transform the unit Frechet margins to GEV 
for (i in 1:n.site)
  data[,i] <- frech2gev(data[,i], param.loc[i], param.scale[i],
param.shape[i])

##Define three models for the GEV margins to be fitted
loc.form <- loc ~ lat
scale.form <- scale ~ lon + I(lat^2)
shape.form <- shape ~ lon

M0 <- fitspatgev(data, locations, loc.form, scale.form, shape.form)
M1 <- fitspatgev(data, locations, loc.form, scale.form, shape.form,
shapeCoeff2 = 0)

##Model selection
anova(M0, M1)
anova(M0, M1, method = "CB", square = "svd")
}
\keyword{htest}
