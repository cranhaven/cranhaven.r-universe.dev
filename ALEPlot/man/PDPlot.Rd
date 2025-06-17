\name{PDPlot}
\alias{PDPlot}
\title{
Partial Dependence (PD) Plots
}

\description{
Computes and plots partial dependence (PD) plots for a fitted supervised learning model. The effects can be either a main effect for an individual predictor (\code{length(J) = 1}) or a second-order interaction effect for a pair of predictors (\code{length(J) = 2}).
}

\usage{
PDPlot(X, X.model, pred.fun, J, K)
}

\arguments{

  \item{X}{The data frame of predictor variables to which the supervised learning model was fit. The names of the predictor variables must be the same as when the model was fit. The response variable should not be included in \code{X}.}

  \item{X.model}{The fitted supervised learning model object (e.g., a tree, random forest, neural network, etc.), typically an object to which a built-in \code{predict} command associated with that object can be applied.}

  \item{pred.fun}{A user-supplied function that will be used to predict the response for \code{X.model} for some specified inputs. \code{pred.fun} has two arguments. The first argument is named \code{X.model} and must be the same object as the \code{X.model} argument to the \code{ALEPlot} function. The second argument is named \code{newdata} and is a data frame of predictor values at which the object \code{X.model} is to be predicted. The output of \code{pred.fun} must be a numeric vector of predictions having length equal to the number of rows of \code{newdata}. For most \code{X.model} objects, \code{pred.fun} can simply call the \code{predict} function that was written as part of that modeling object package, assuming the package contains a \code{predict} function. An example of where a more customized \code{pred.fun} would be used is a multi (> 2) class classification problem for which the built-in predict function returns a vector of predicted probabilities, one for each response class. In this case it may make sense to have \code{pred.fun} return the predicted probabilities (or its log-odds, etc.) for one particular class of interest.}

  \item{J}{A numeric scalar or two-length vector of indices of the predictors for which the PD plot will be calculated. \code{J} is either a single index (for a main effects plot) or a pair of indices (for a second-order interaction plot). For a single index, the corresponding predictor must be either numeric or a factor. For a pair of indices, the corresponding predictors must be either both numeric or the first a factor and the second numeric.}

  \item{K}{A numeric scalar that represents the number of discrete points at which the PD plot will be calculated. If \code{length(J) = 2}, the same \code{K} will be used for both predictors, resulting in a two-dimensional grid of \code{K^2} predictor values at with the PD plot will be calculated. \code{K} is only used if the predictor is numeric. For factor predictors, the equivalent of \code{K} is the number of used levels of the factor, which is automatically determined internally.}

}

\details{
This function calculates and plots the partial dependence (PD) plots first introduced in Friedman (2001). See the Apley (2016) reference paper listed below for details. For \code{J = j} (i.e., if the index for a single predictor \eqn{x_j}{x_j} is specified), the function calculates and returns the PD main effect of \eqn{x_j}{x_j}, which is denoted by \eqn{f_{j,PD}(x_j)}{f_j,PD(x_j)} in Apley (2016). It also plots \eqn{f_{j,PD}(x_j)}{f_j,PD(x_j)}. For \code{J = c(j1,j2)} (i.e., if the indices for a pair of predictors \eqn{(x_{j1},x_{j2})}{(x_j1,x_j2)} are specified), the function calculates and returns the PD second-order interaction effect of \eqn{(x_{j1},x_{j2})}{(x_j1,x_j2)}, which is denoted by \eqn{f_{{j1,j2},PD}(x_{j1},x_{j2})}{f_{j1,j2},PD(x_j1,x_j2)} in Apley (2016). It also plots \eqn{f_{{j1,j2},PD}(x_{j1},x_{j2})}{f_{j1,j2},PD(x_j1,x_j2)}. 

}

\value{
  \item{f.values}{If \code{length(J) = 1}, a vector of PD plot function values at the predictor values in \code{x.values}. If \code{length(J) = 2}, \code{f.values} is a \code{K1xK} matrix of the PD plot function values at the grid of predictor values defined by the \code{X[,J[1]]} and \code{X[,J[2]]} values in \code{x.values}. For \code{X[,J[1]]} numeric, \code{K1 = K}. For \code{X[,J[2]]} a factor, \code{K1} is the number of used levels (empty levels are dropped). The rows of \code{f.values} correspond to \code{X[,J(1)]}, and the columns to \code{X[,J(2)]}.}

  \item{x.values}{For numeric predictors, if \code{length(J) = 1}, a \code{K}-length vector specifying the ordered predictor values at which the PD plot function is calculated. If \code{length(J) = 2}, a list of two such vectors, the first containing the \code{X[,J(1)]} values and the second containing the \code{X[,J(2)]} values at which the PD plot function is calculated. \code{x.values} is the same for factor predictors, except it is a \code{K1}-length character vector of the levels of the predictor, where \code{K1} is determined internally as the number of unique levels of the predictor (empty levels are dropped).}
}

\references{
Friedman, J. H., (2001), "Greedy function approximation: A gradient boosting machine," Annals of Statistics, 29(5), pp. 1189-1232.

Apley, D. W. (2016), "Visualizing the Effects of Predictor Variables in Black Box Supervised Learning Models," submitted for publication.

}
\author{
\packageAuthor{ALEPlot}
}

\seealso{
See \code{\link{ALEPlot}} for partial dependence plots.
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

## Define the predictive function (easy in this case, since \code{lm} has 
## a built-in predict function that suffices)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))

## Calculate and plot the PD main effects and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
PD.1=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=1, K=50)
PD.2=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=2, K=50)
PD.3=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=3, K=50)
PD.12=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(1,2), K=30)
PD.13=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(1,3), K=30)
PD.23=PDPlot(DAT[,2:4], lm.DAT, pred.fun=yhat, J=c(2,3), K=30)

## The following manually recreates the same plots produced by the above PDPlot function calls
par(mfrow = c(2,3))
plot(PD.1$x.values, PD.1$f.values, type="l", xlab="x1", ylab="PD main effect for x1")
plot(PD.2$x.values, PD.2$f.values, type="l", xlab="x2", ylab="PD main effect for x2")
plot(PD.3$x.values, PD.3$f.values, type="l", xlab="x3", ylab="PD main effect for x3")
image(PD.12$x.values[[1]], PD.12$x.values[[2]], PD.12$f.values, xlab = "x1", ylab = "x2")
contour(PD.12$x.values[[1]], PD.12$x.values[[2]], PD.12$f.values, add=TRUE, drawlabels=TRUE)
image(PD.13$x.values[[1]], PD.13$x.values[[2]], PD.13$f.values, xlab = "x1", ylab = "x3")
contour(PD.13$x.values[[1]], PD.13$x.values[[2]], PD.13$f.values, add=TRUE, drawlabels=TRUE)
image(PD.23$x.values[[1]], PD.23$x.values[[2]], PD.23$f.values, xlab = "x2", ylab = "x3")
contour(PD.23$x.values[[1]], PD.23$x.values[[2]], PD.23$f.values, add=TRUE, drawlabels=TRUE)

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

## Calculate and plot the PD main and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
PD.1=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=1, K=50)
PD.2=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=2, K=50)
PD.3=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=3, K=50)
PD.12=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,2), K=20)
PD.13=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,3), K=20)
PD.23=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(2,3), K=20)
       
########################################################################
## A binary classification example in which the supervised learning model is 
## a neural network (\code{nnet}), and the log-odds of the predicted class 
## probability is the function to be plotted
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

## Calculate and plot the PD main and second-order interaction effects of x1, x2, x3
par(mfrow = c(2,3))
PD.1=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=1, K=50)
PD.2=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=2, K=50)
PD.3=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=3, K=50)
PD.12=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,2), K=20)
PD.13=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(1,3), K=20)
PD.23=PDPlot(DAT[,2:4], nnet.DAT, pred.fun=yhat, J=c(2,3), K=20)
}
}
\keyword{models}
