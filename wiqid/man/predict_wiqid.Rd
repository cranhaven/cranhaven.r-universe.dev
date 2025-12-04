\name{predict.wiqid}
\alias{predict.wiqid}
\title{
Predict method for objects of class 'wiqid'
}
\description{
Obtains predictions, with estimates, standard errors and confidence intervals, from a fitted model object of class \code{wiqid}, as produced by frequentist estimation functions in the \pkg{wiqid} package. Not all functions produce objects that enable predictions to be made; see Details. Please treat this as a 'beta' version and check output carefully.
}
\usage{
\method{predict}{wiqid}(object, newdata, parameter, ci, type=c("link", "response"), ...) 
    
}
\arguments{
  \item{object}{
an object of class \code{wiqid}.
}
  \item{newdata}{
a data frame with columns for each of the covariates in the model. Unused columns are ignored. Missing values are not allowed. See Details.
}
  \item{parameter}{
character; the name of the parameter to predict; this will appear on the left hand side of one of the formulae in the model.
}
  \item{ci}{
the confidence interval to use; the default is to use \code{object$ci} or, if that is \code{NULL}, 0.95.
}
  \item{type}{
the type of prediction required. The default is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable. Thus if the parameter is a probability, the default predictions are on the logit or probit scale and \code{type = "response"} gives the predicted probabilities. May be abbreviated.
}
  \item{\dots}{
further arguments for other methods.
}
}
\details{
Most \pkg{wiqid} functions have models with multiple submodels, corresponding to the formulae in the \code{model} argument. Check \code{object$formulae} for a list of the available submodels.

The argument \code{newdata} is required (even for intercept-only models), and must be a data frame with named columns for each of the covariates in the submodel. For factors, the levels must be (a subset of) the levels in the original data; check \code{object$xlev} for possible levels.

\code{predict} is not yet implemented for the following functions:
  \tabular{ll}{
  \code{occSStime} and \code{occSScovSite} \tab : use \code{occSS} instead.\cr
  \code{occMStime} and \code{occMScovSite} \tab : use \code{occMS} instead.\cr
  \code{closedCap*} functions \tab : these models have no covariates.\cr
  \code{surv*} functions \tab : these have no covariates.}

}
\value{
Returns a matrix with four columns (estimate, SE, lower and upper confidence limits) and a row for each row in \code{newdata}. If \code{newdata} has row names, these will be used for the output. Note that for an intercept-only submodel, all rows will have identical values. Attributes give information on the link used and the confidence level.
}
\author{
Mike Meredith.
}

\examples{
# Generate some simulated occupancy data for 300 sites:
set.seed(2017)
original.data <- data.frame(
  elev = runif(300, 0, 1000),
  forType = factor(sample(c("dry", "swamp", "mangrove"), size=300, replace=TRUE, prob=3:1)))
modMat <- model.matrix( ~ elev + forType, data = original.data)
psiCoef <- c(3, -0.003, -3, -1) # declines with 'elev'; highest for 'dry', lowest 'mangrove'
psi <- plogis(modMat \%*\% psiCoef)
hist(psi, breaks=20)
z <- rbinom(300, 1, psi)
mean(z)  # true realized occupancy
# detection history for 3 replicates, constant p = 0.6:
DH <- matrix(rbinom(300*3, 1, 0.6*z), nrow=300)
# fit models
m0 <- occSS(DH)
mE <- occSS(DH, psi ~ elev, data = original.data)
mEF <- occSS(DH, psi ~ elev + forType, data = original.data)

# now try predictions:
newdata <- expand.grid(elev=c(200, 500, 800), forType=c("dry", "swamp"))
predict(mEF, newdata, "psi")
cbind(newdata, predict(mEF, newdata, "psi", type='res'))
cbind(newdata, predict(mE, newdata, "psi", type='res'))
cbind(newdata, predict(m0, newdata, "psi", type='res'))

# do a nice plot
xx <- seq(0, 1000, length=51)
plotdata <- expand.grid(elev=xx, forType=c("dry", "swamp", "mangrove"))
toPlot <- predict(mEF, plotdata, "psi", type='res')
plot(xx, rep(0.5, 51), type='n', las=1, ylim=range(toPlot),
  xlab="Elevation", ylab="Occupancy probability")
ciCols <- adjustcolor(c('lightgreen', 'skyblue', 'pink'), 0.5)
estCols <- c('darkgreen', 'blue', 'red')
for(i in 1:3) {
  this1 <- toPlot[plotdata$forType == levels(plotdata$forType)[i], ]
  polygon(c(xx, rev(xx)), c(this1[, 3], rev(this1[, 4])), col=ciCols[i])
  lines(xx, this1[, 1], col=estCols[i])
}
legend('topright', levels(plotdata$forType), lty=1, col=estCols, bty='n')

# Add a survey-level covariate: observer ID with different detection probabilities
observer <- c(sample(1:2, size=300, replace=TRUE),  # A and B on first survey occasion
              sample(1:3, size=300, replace=TRUE),  # A, B and C for second
              sample(2:3, size=300, replace=TRUE))  # only B and C for third
obsID <- matrix(LETTERS[observer], nrow=300)
colnames(obsID) <- c("obs1", "obs2", "obs3")
original.data <- cbind(original.data, as.data.frame(obsID))
str(original.data)
p <- c(0.4, 0.6, 0.8)[observer]
DH <- matrix(rbinom(300*3, 1, p*z), nrow=300)
mEFO <- occSS(DH, list(psi ~ elev + forType, p ~ obs), data = original.data)
# Check the categorical covariate names and levels:
mEFO$xlev
predict(mEFO, data.frame(obs=c("A", "B", "C")), "p")
predict(mEFO, data.frame(obs=c("A", "B", "C")), "p", type="resp")

}
\keyword{predict}
\keyword{methods}
