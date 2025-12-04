\name{predictAvg}
\alias{predictAvg}
\title{
Predict average values from multiple fitted models
}
\description{
Produce model-averaged estimates of predictions from multiple models of the same type fitted with a function in the \pkg{wiqid} package.
}
\usage{
predictAvg(modList, newdata, parameter, ci=0.95, type=c("link", "response"), IC=AICc)
}
\arguments{
  \item{modList}{
a list of fitted model objects of class \code{wiqid}.
}
  \item{newdata}{
a data frame with columns for each of the covariates in the model. Unused columns are ignored. Missing values are not allowed. See Details.
}
  \item{parameter}{
character; the name of the parameter to predict; this will appear on the left hand side of one of the formulae in the model.
}
  \item{ci}{
the confidence interval to use.
}
  \item{type}{
the type of prediction required. The default is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable. Thus if the parameter is a probability, the default predictions are on the logit or probit scale and \code{type = "response"} gives the predicted probabilities. May be abbreviated.
}
  \item{IC}{
the information criterion function to use to calculate model weights.
}
}
\details{
The function calls \code{predict} with each of the models in \code{modList} in turn to obtain predictions (estimates and SEs). The information criterion specified by \code{IC} is applied to each model to get model weights, and these are used to average the estimates.

The algorithm to calculate the SEs (and hence CIs) of the model-averaged estimates follows Anderson (2008, p.111).
}
\value{
Returns a matrix with four columns (estimate, SE, lower and upper confidence limits) and a row for each row in \code{newdata}. If \code{newdata} has row names, these will be used for the output. Attributes give information on the link used and the confidence level.
}
\author{
Mike Meredith.
}
\references{
Anderson, D.R. (2008) \emph{Model based inference in the life sciences: a primer on evidence}. Springer Science + Business Media, New York NY.
}


\examples{
data(toves)

# Extract detection histories
DH <- toves[, 1:4]

# Fit some models
m.1 <- occSS(DH, psi ~ x1, data=toves)
m.12 <- occSS(DH, psi ~ x1 + x2, data=toves)
m.13 <- occSS(DH, psi ~ x1 + x3, data=toves)
m.123 <- occSS(DH, psi ~ x1 + x2 + x3, data=toves)
m.23 <- occSS(DH, psi ~ x2 + x3, data=toves)
AICtable(AICc(m.1, m.12, m.13, m.123, m.23))

# Covariate x1 is essential, x3 is unnecessary, and there's
#   doubt about x2, as the difference in AICc between m.1 and m.12
#   is small.
# We'll use m.1 and m.12 to get model-averaged estimates  of 'psi' for
#   the first 10 sites in the data set.

newdata <- toves[1:10, ]

psi.ma <- predictAvg(list(m.1, m.12), newdata, "psi", type="response")

# Get estimates for the individual models and plot
psi.1 <- predict(m.1, newdata, parameter="psi", type="response")
psi.12 <- predict(m.12, newdata, parameter="psi", type="response")

require(graphics)
plot(1:10, psi.ma[,1], xlab="Site number", ylab="psi", pch=16, cex=1.5,
    las=1, ylim=0:1, xlim=c(0.5, 10.5))
arrows(1:10, psi.ma[,3], 1:10, psi.ma[,4], angle=90, length=0.03, code=3, lwd=2)
# Add values from psi.1 and psi.12
points(1:10 - 0.2, psi.1[,1], col='red')
arrows(1:10 - 0.2, psi.1[,3], 1:10 - 0.2, psi.1[,4],
    angle=90, length=0.03, code=3, col='red')
points(1:10 + 0.2, psi.12[,1], pch=2, col='blue')
arrows(1:10 + 0.2, psi.12[,3], 1:10 + 0.2, psi.12[,4],
    angle=90, length=0.03, code=3, col='blue')
}
\keyword{predict}
\keyword{methods}
