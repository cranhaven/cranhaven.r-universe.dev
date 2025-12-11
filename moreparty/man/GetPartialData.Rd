\name{GetPartialData}
\alias{GetPartialData}

\title{Partial dependence for a conditional random forest.}

\description{
Computes the partial dependence for several covariates in a conditional random forest and gathers them into a single data frame.
}

\usage{
GetPartialData(object, xnames=NULL, ice = FALSE, center = FALSE,
               grid.resolution = NULL, quantiles = TRUE, probs = 1:9/10,
               trim.outliers = FALSE, which.class = 1L, prob = TRUE,
               pred.fun = NULL, parallel = FALSE, paropts = NULL)
}

\arguments{
  \item{object}{
An object as returned by \code{\link[party]{cforest}} (or \code{\link{fastcforest}}).
  }
  \item{xnames}{
A character vector of the covariates for which to compute the partial dependence. If NULL (default), partial dependence is computed for all the covariates in the model.
}
  \item{ice}{
Logical indicating whether or not to compute individual conditional expectation (ICE) curves. Default is FALSE. See Goldstein et al. (2014) for details.
}
  \item{center}{
Logical indicating whether or not to produce centered ICE curves (c-ICE curves). Only used when ice = TRUE. Default is FALSE. See Goldstein et al. (2014) for details.
}
  \item{grid.resolution}{
Integer giving the number of equally spaced points to use for the continuous variables listed in \code{xnames}. If left NULL, it will default to the minimum between 51 and the number of unique data points for each of the continuous independent variables listed in \code{xnames}.
}
  \item{quantiles}{
Logical indicating whether or not to use the sample quantiles of the continuous predictors listed in \code{xnames}. If \code{quantiles = TRUE} and \code{grid.resolution = NULL} (default), the sample quantiles will be used to generate the grid of joint values for which the partial dependence is computed.
}
  \item{probs}{
Numeric vector of probabilities with values in [0,1]. (Values up to 2e-14 outside that range are accepted and moved to the nearby endpoint.) Default is \code{1:9/10} which corresponds to the deciles of the predictor variables. These specify which quantiles to use for the continuous predictors listed in \code{xnames} when \code{quantiles = TRUE}.
}
  \item{trim.outliers}{
Logical indicating whether or not to trim off outliers from the continuous predictors listed in \code{xnames} (using the simple boxplot method) before generating the grid of joint values for which the partial dependence is computed. Default is FALSE.
}
  \item{which.class}{
Integer specifying which column of the matrix of predicted probabilities to use as the "focus" class. Default is to use the first class. Only used for classification problems.
}
  \item{prob}{
Logical indicating whether or not partial dependence for classification problems should be returned on the probability scale, rather than the centered logit. If FALSE, the partial dependence function is on a scale similar to the logit. Default is TRUE.
}
  \item{pred.fun}{
Optional prediction function that requires two arguments: \code{object} and \code{newdata}. If specified, then the function must return a single prediction or a vector of predictions (i.e., not a matrix or data frame). Default is NULL.
}
  \item{parallel}{
Logical indicating whether or not to run \code{\link[pdp]{partial}} in parallel using a backend provided by the \code{foreach} package. Default is FALSE.
}
  \item{paropts}{
List containing additional options to be passed onto \code{\link[foreach]{foreach}} when \code{parallel = TRUE}.
}
}

\details{
The computation of partial dependence uses \code{\link[pdp]{partial}} function from \code{pdp} package for each covariate. The results are then gathered and reshaped into a friendly data frame format.}

\value{
A data frame with covariates, their categories and their partial dependence effects.
}

\references{
J. H. Friedman. Greedy function approximation: A gradient boosting machine. Annals of Statistics, 29: 1189-1232, 2001.

Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E., Peeking Inside the Black Box: Visualizing Statistical Learning With Plots of Individual Conditional Expectation. (2014) Journal of Computational and Graphical Statistics, 24(1): 44-65, 2015.
}

\author{Nicolas Robette}

\seealso{
\code{\link[pdp]{partial}},\code{\link{GetAleData}},\code{\link{GetInteractionStrength}}
}

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  iris.cf = party::cforest(Species ~ ., data = iris2, 
              controls = party::cforest_unbiased(mtry=2, ntree=50))
  GetPartialData(iris.cf)
}

\keyword{tree}
