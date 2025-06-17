\name{ALEPlot}
\alias{ALEPlot}
\title{
Accumulated Local Effects (ALE) Plots
}
\description{
Computes and plots accumulated local effects (ALE) plots for a fitted supervised learning model. The effects can be either a main effect for an individual predictor (\code{length(J) = 1}) or a second-order interaction effect for a pair of predictors (\code{length(J) = 2}).
}

\usage{
ALEPlot(X, X.model, pred.fun, J, K = 40, NA.plot = TRUE)
}

\arguments{

  \item{X}{The data frame of predictor variables to which the supervised learning model was fit. The names of the predictor variables must be the same as when the model was fit. The response variable should not be included in \code{X}.}

  \item{X.model}{The fitted supervised learning model object (e.g., a tree, random forest, neural network, etc.), typically an object to which a built-in \code{predict} command associated with that object can be applied.}

  \item{pred.fun}{A user-supplied function that will be used to predict the response for \code{X.model} for some specified inputs. \code{pred.fun} has two arguments. The first argument is named \code{X.model} and must be the same object as the \code{X.model} argument to the \code{ALEPlot} function. The second argument is named \code{newdata} and is a data frame of predictor values at which the object \code{X.model} is to be predicted. The output of \code{pred.fun} must be a numeric vector of predictions having length equal to the number of rows of \code{newdata}. For most \code{X.model} objects, \code{pred.fun} can simply call the \code{predict} function that was written as part of that modeling object package, assuming the package contains a \code{predict} function. An example of where a more customized \code{pred.fun} would be used is a multi (> 2) class classification problem for which the built-in predict function returns a vector of predicted probabilities, one for each response class. In this case it may make sense to have \code{pred.fun} return the predicted probabilities (or its log-odds, etc.) for one particular class of interest.}

  \item{J}{A numeric scalar or two-length vector of indices of the predictors for which the ALE plot will be calculated. \code{J} is either a single index (for a main effects plot) or a pair of indices (for a second-order interaction plot). For a single index, the corresponding predictor must be either numeric or a factor. For a pair of indices, the corresponding predictors must be either both numeric or the first a factor and the second numeric.}

  \item{K}{A numeric scalar that specifies the number of intervals into which the predictor range is divided when calculating the ALE plot effects. If \code{length(J) = 2}, the same \code{K} will be used for both predictors, resulting in an array of \code{K^2} cells over the two-dimensional predictor space. Note that the algorithm may adjust (reduce) \code{K} internally if the predictors are discrete and have many repeated values. \code{K} is only used if the predictor is numeric. For factor predictors, the equivalent of \code{K} is the number of used levels of the factor, which is automatically determined internally.}

  \item{NA.plot}{A logical value that is only used if \code{length(J) = 2}. If \code{NA.plot = TRUE} (the default), the ALE second-order effects are also plotted for empty cells. Empty cells are defined as cells in the \code{(X[,J[1]], X[,J[2]])} space into which no training observations fall. If \code{NA.plot = FALSE}, the accumulated local second-order effects are only plotted for non-empty cells, and black rectangles are plotted over any empty cells to indicate their locations. Either way, when accumulating the local second-order effects, values are need for the empty cells, and these values are taken to be the local second-order effects for the nearest-neighbor non-empty cell.}

}

\details{
See the Apley (2016) reference paper listed below for details. For \code{J = j} (i.e., if the index for a single predictor \eqn{x_j}{x_j} is specified), the function calculates and returns the ALE main effect of \eqn{x_j}{x_j}, which is denoted by \eqn{f_{j,ALE}(x_j)}{f_j,ALE(x_j)} in Apley (2016). It also plots \eqn{f_{j,ALE}(x_j)}{f_j,ALE(x_j)}. For \code{J = c(j1,j2)} (i.e., if the indices for a pair of predictors \eqn{(x_{j1},x_{j2})}{(x_j1,x_j2)} are specified), the function calculates and returns the ALE second-order interaction effect of \eqn{(x_{j1},x_{j2})}{(x_j1,x_j2)}, which is denoted by \eqn{f_{{j1,j2},ALE}(x_{j1},x_{j2})}{f_{j1,j2},ALE(x_j1,x_j2)} in Apley (2016). It also plots \eqn{f_{{j1,j2},ALE}(x_{j1},x_{j2})}{f_{j1,j2},ALE(x_j1,x_j2)}. 
}

\value{

  \item{K}{The same as the input argument \code{K}, but possibly adjusted internally. For numeric predictors, \code{K} is the number of intervals into which the range of each predictor is divided. If the predictor is discrete with many repeated values, \code{K} can be reduced internally, as mentioned above. For \code{length(J)=1}, \code{K} is an integer. For \code{length(J)=2}, \code{K = c(K1, K2)}, where \code{K1} and \code{K2} are the numbers of intervals for the \code{X[,J(1)]} and \code{X[,J(2)]} ranges, respectively. For factor predictors, \code{K} is the number of non-empty levels, which is calculated internally.}

  \item{f.values}{If \code{length(J) = 1}, a vector of ALE plot function values at the predictor values in \code{x.values}. If \code{length(J) = 2}, \code{f.values} is a matrix of ALE plot function values at the grid of values defined by the \code{X[,J(1)]} and \code{X[,J(2)]} values in \code{x.values}. The rows of \code{f.values} correspond to \code{X[,J(1)]}, and the columns to \code{X[,J(2)]}.}

  \item{x.values}{For numeric predictors, if \code{length(J) = 1}, a (\code{K+1})-length vector specifying the ordered predictor values at which the ALE plot function is calculated. These are the break points for the \code{K} intervals into which the predictor range is divided, plus the lower boundary of the first interval and the upper boundary of the last interval. If \code{length(J) = 2}, a list of two such vectors, the first containing the \code{X[,J(1)]} values and the second containing the \code{X[,J(2)]} values at which the ALE plot function is calculated. \code{x.values} is the same for factor predictors, except it is a \code{K}-length character vector containing the ordered levels of the predictor (the ordering is determined internally, based on the similarity of the predictor in question to the other predictors), where \code{K} is the number of used levels. The elements of \code{f.values} are ordered accordingly.}

}

\references{
Apley, D. W. (2016), "Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models," submitted for publication.
}
\author{
\packageAuthor{ALEPlot}
}

\seealso{
See \code{\link{PDPlot}} for partial dependence plots.
}

\examples{
########################################################################
## A transparent example in which the supervised learning model is a linear regression \code{lm},
## but we will pretend it is black-box
########################################################################

## Generate some data and fit a \code{lm} supervised learning model
N=500
x1 <- runif(N, min=0, max=1)
x2 <- runif(N, min=0, max=1)
x3 <- runif(N, min=0, max=1)
y = x1 + 2*x2^2 + rnorm(N, 0, 0.1)
DAT = data.frame(y, x1, x2, x3)
lm.DAT = lm(y ~ .^2 + I(x1^2) + I(x2^2) + I(x3^2), DAT)

## Define the predictive function (easy in this case, since \code{lm} has a built-in 
## predict function that suffices)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

## Calculate and plot the ALE main and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
ALE.1=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=1, K=50, NA.plot = TRUE)
ALE.2=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=2, K=50, NA.plot = TRUE)
ALE.3=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=3, K=50, NA.plot = TRUE)
ALE.12=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(1,2), K=20, NA.plot = TRUE)
ALE.13=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(1,3), K=20, NA.plot = TRUE)
ALE.23=ALEPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(2,3), K=20, NA.plot = TRUE)

## The following manually recreates the same plots produced by the above ALEPlot function calls
par(mfrow = c(2,3))
plot(ALE.1$x.values, ALE.1$f.values, type="l", xlab="x1", ylab="ALE main effect for x1")
plot(ALE.2$x.values, ALE.2$f.values, type="l", xlab="x2", ylab="ALE main effect for x2")
plot(ALE.3$x.values, ALE.3$f.values, type="l", xlab="x3", ylab="ALE main effect for x3")
image(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, xlab = "x1", ylab = "x2")
contour(ALE.12$x.values[[1]], ALE.12$x.values[[2]], ALE.12$f.values, add=TRUE, drawlabels=TRUE)
image(ALE.13$x.values[[1]], ALE.13$x.values[[2]], ALE.13$f.values, xlab = "x1", ylab = "x3")
contour(ALE.13$x.values[[1]], ALE.13$x.values[[2]], ALE.13$f.values, add=TRUE, drawlabels=TRUE)
image(ALE.23$x.values[[1]], ALE.23$x.values[[2]], ALE.23$f.values, xlab = "x2", ylab = "x3")
contour(ALE.23$x.values[[1]], ALE.23$x.values[[2]], ALE.23$f.values, add=TRUE, drawlabels=TRUE)

\donttest{
       
########################################################################
## A larger example in which the supervised learning model is a neural network (\code{nnet})
########################################################################

## Generate some data and fit a \code{nnet} supervised learning model

library(nnet)
N=5000
x1 <- runif(N, min=0, max=1)
x2 <- runif(N, min=0, max=1)
x3 <- runif(N, min=0, max=1)
y = x1 + 2*x2^2 +(x1-0.5)*(x3-0.5) + rnorm(N, 0, 0.1)
DAT = data.frame(y, x1, x2, x3)
nnet.DAT<-nnet(y~., data=DAT, linout=TRUE, skip=FALSE, size=10, decay=0.01, 
	maxit=1000, trace=FALSE)

## Define the predictive function
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata, type="raw"))

## Calculate and plot the ALE main and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
ALE.1=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=1, K=50, NA.plot = TRUE)
ALE.2=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=2, K=50, NA.plot = TRUE)
ALE.3=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=3, K=50, NA.plot = TRUE)
ALE.12=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,2), K=20, NA.plot = TRUE)
ALE.13=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,3), K=20, NA.plot = TRUE)
ALE.23=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(2,3), K=20, NA.plot = TRUE)

########################################################################
## A binary classification example in which the supervised learning model is 
## a neural network (\code{nnet}), and the log-odds of the predicted class probability 
## is the function to be plotted
########################################################################

## Generate some data and fit a \code{nnet} supervised learning model

library(nnet)
N=5000
x1 <- runif(N, min=0, max=1)
x2 <- runif(N, min=0, max=1)
x3 <- runif(N, min=0, max=1)
z = -3.21 + 2.81*x1 + 5.62*x2^2 + 2.81*(x1-0.5)*(x3-0.5) #true log-odds
p = exp(z)/(1+exp(z))
u = runif(N)
y = u < p
DAT = data.frame(y, x1, x2, x3)
nnet.DAT<-nnet(y~., data=DAT, linout=FALSE, skip=FALSE, size=10, decay=0.05, 
	maxit=1000, trace=FALSE)

## Define the ALE function to be the log-odds of the predicted probability that y = TRUE
yhat <- function(X.model, newdata) {
    p.hat = as.numeric(predict(X.model, newdata, type="raw"))
    log(p.hat/(1-p.hat))
}

## Calculate and plot the ALE main and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
ALE.1=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=1, K=50, NA.plot = TRUE)
ALE.2=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=2, K=50, NA.plot = TRUE)
ALE.3=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=3, K=50, NA.plot = TRUE)
ALE.12=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,2), K=20, NA.plot = TRUE)
ALE.13=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,3), K=20, NA.plot = TRUE)
ALE.23=ALEPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(2,3), K=20, NA.plot = TRUE)

}
}
\keyword{models}