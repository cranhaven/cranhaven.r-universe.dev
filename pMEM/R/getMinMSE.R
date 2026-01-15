## **************************************************************************
##
##    (c) 2023-2024 Guillaume Guénard
##        Department de sciences biologiques,
##        Université de Montréal
##        Montreal, QC, Canada
##
##    **Simple orthogonal term selection regression**
##
##    This file is part of pMEM
##
##    pMEM is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    pMEM is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with pMEM. If not, see <https://www.gnu.org/licenses/>.
##
##    R source code file
##
## **************************************************************************
##
#' Simple Orthogonal Term Selection Regression
#' 
#' A simple orthogonal term selection regression function for minimizing out of
#' the sample mean squared error (MSE).
#' 
#' @param U A matrix of spatial eigenvectors to be used as training data.
#' @param y A numeric vector containing a single response variable to be used as
#' training labels.
#' @param Up A numeric matrix of spatial eigenvector scores to be used as
#' testing data.
#' @param yy A numeric vector containing a single response variable to be used
#' as testing labels.
#' @param complete A boolean specifying whether to return the complete data of
#' the selection procedure (\code{complete=TRUE}; the default) or only the
#' resulting mean square error and beta threshold (\code{complete=FALSE}).
#' 
#' @returns If \code{complete = TRUE}, a list with the following members:
#' \describe{
#'   \item{ betasq }{ The squared standardized regression coefficients. }
#'   \item{ nullmse }{ The null MSE value: the mean squared out of the sample
#'   error using only the mean of the training labels as the prediction. }
#'   \item{ mse }{ The mean squared error of each incremental model. }
#'   \item{ ord }{ The order of the squared standardized regression
#'   coefficients. }
#'   \item{ wh }{ The index of the model with the smallest mean squared error. 
#'   The value 0 means that the smallest MSE is the null MSE. }
#' }
#' 
#' If \code{complete = FALSE} a two element list with the following members:
#' \describe{
#'   \item{ betasq }{ The squared standardized regression coefficient
#'   associated with the minimum means squared error value. }
#'   \item{ mse }{ The minimum means squared error value. }
#' }
#' 
#' @details This function allows one to calculate a simple model, involving only
#' the spatial eigenvectors and a single response variable. The coefficients
#' are estimated on a training data set; the ones that are retained are chosen
#' on the basis of minimizing the mean squared error on the testing data set. As
#' such, both a training and a testing data set are mandatory for this procedure
#' to be carried on. The procedure goes as follows:
#' \enumerate{
#'   \item{ The regression coefficients are calculated as the cross-product
#'   \code{b = t(U)y} and are sorted in decreasing order of their absolute
#'   values. }
#'   \item{ The mean of the training labels is calculated, then the residuals
#'   training labels are calculated, and the null MSE is calculated from the
#'   testing labels. }
#'   \item{ For each regression coefficient, the partial predicted value is
#'   calculated and subtracted from the testing labels, and the new MSE value is
#'   calculated. }
#'   \item{ The minimum MSE value is identified. }
#'   \item{ The regression coefficients are standardized ans squared and the
#'   results are returned. }
#' }
#' 
#' For this procedure, the training data must be are orthonormal, a condition
#' met design by spatial eigenvectors.
#' 
#' 
#' @author \packageAuthor{pMEM}
#' 
#' Maintainer: \packageMaintainer{pMEM}
#' 
#' @examples ## Loading the 'salmon' dataset
#' data("salmon")
#' seq(1,nrow(salmon),3) -> test      # Indices of the testing set.
#' (1:nrow(salmon))[-test] -> train   # Indices of the training set.
#' 
#' ## A set of locations located 1 m apart:
#' xx <- seq(min(salmon$Position) - 20, max(salmon$Position) + 20, 1)
#' 
#' ## Lists to contain the results:
#' mseRes <- list()
#' sel <- list()
#' lm <- list()
#' prd <- list()
#' 
#' ## Generate the spatial eigenfunctions:
#' genSEF(
#'   x = salmon$Position[train],
#'   m = genDistMetric(),
#'   f = genDWF("Gaussian",40)
#' ) -> sefTrain
#' 
#' ## Spatially-explicit modelling of the channel depth:
#' 
#' ## Calculate the minimum MSE model:
#' getMinMSE(
#'   U = as.matrix(sefTrain),
#'   y = salmon$Depth[train],
#'   Up = predict(sefTrain, salmon$Position[test]),
#'   yy = salmon$Depth[test]
#' ) -> mseRes[["Depth"]]
#' 
#' ## This is the coefficient of prediction:
#' 1 - mseRes$Depth$mse[mseRes$Depth$wh]/mseRes$Depth$nullmse
#' 
#' ## Storing graphical parameters:
#' tmp <- par(no.readonly = TRUE)
#' 
#' ## Changing the graphical margins:
#' par(mar=c(4,4,2,2))
#' 
#' ## Plot of the MSE values:
#' plot(mseRes$Depth$mse, type="l", ylab="MSE", xlab="order", axes=FALSE,
#'      ylim=c(0.005,0.025))
#' points(x=1:length(mseRes$Depth$mse), y=mseRes$Depth$mse, pch=21, bg="black")
#' axis(1)
#' axis(2, las=1)
#' abline(h=mseRes$Depth$nullmse, lty=3)  # Dotted line: the null MSE
#' 
#' ## A list of the selected spatial eigenfunctions:
#' sel[["Depth"]] <- sort(mseRes$Depth$ord[1:mseRes$Depth$wh])
#' 
#' ## A linear model build using the selected spatial eigenfunctions:
#' lm(
#'   formula = y~.,
#'   data = cbind(
#'     y = salmon$Depth[train],
#'     as.data.frame(sefTrain, wh=sel$Depth)
#'   )
#' ) -> lm[["Depth"]]
#' 
#' ## Calculating predictions of depth at each 1 m intervals:
#' predict(
#'   lm$Depth,
#'   newdata = as.data.frame(
#'     predict(
#'       object = sefTrain,
#'       newdata = xx,
#'       wh = sel$Depth
#'     )
#'   )
#' ) -> prd[["Depth"]]
#' 
#' ## Plot of the predicted depth (solid line), and observed depth for the
#' ## training set (black markers) and testing set (red markers):
#' plot(x=xx, y=prd$Depth, type="l", ylim=range(salmon$Depth, prd$Depth), las=1,
#'      ylab="Depth (m)", xlab="Location along the transect (m)")
#' points(x = salmon$Position[train], y = salmon$Depth[train], pch=21,
#'        bg="black")
#' points(x = salmon$Position[test], y = salmon$Depth[test], pch=21, bg="red")
#' 
#' ## Spatially-explicit modelling of the water velocity:
#' 
#' ## Calculate the minimum MSE model:
#' getMinMSE(
#'   U = as.matrix(sefTrain),
#'   y = salmon$Velocity[train],
#'   Up = predict(sefTrain, salmon$Position[test]),
#'   yy = salmon$Velocity[test]
#' ) -> mseRes[["Velocity"]]
#' 
#' ## This is the coefficient of prediction:
#' 1 - mseRes$Velocity$mse[mseRes$Velocity$wh]/mseRes$Velocity$nullmse
#' 
#' ## Plot of the MSE values:
#' plot(mseRes$Velocity$mse, type="l", ylab="MSE", xlab="order", axes=FALSE,
#'      ylim=c(0.010,0.030))
#' points(x=1:length(mseRes$Velocity$mse), y=mseRes$Velocity$mse, pch=21,
#'        bg="black")
#' axis(1)
#' axis(2, las=1)
#' abline(h=mseRes$Velocity$nullmse, lty=3)
#' 
#' ## A list of the selected spatial eigenfunctions:
#' sel[["Velocity"]] <- sort(mseRes$Velocity$ord[1:mseRes$Velocity$wh])
#' 
#' ## A linear model build using the selected spatial eigenfunctions:
#' lm(
#'   formula = y~.,
#'   data = cbind(
#'     y = salmon$Velocity[train],
#'     as.data.frame(sefTrain, wh=sel$Velocity)
#'   )
#' ) -> lm[["Velocity"]]
#' 
#' ## Calculating predictions of velocity at each 1 m intervals:
#' predict(
#'   lm$Velocity,
#'   newdata = as.data.frame(
#'     predict(
#'       object = sefTrain,
#'       newdata = xx,
#'       wh = sel$Velocity
#'     )
#'   )
#' ) -> prd[["Velocity"]]
#' 
#' ## Plot of the predicted velocity (solid line), and observed velocity for the
#' ## training set (black markers) and testing set (red markers):
#' plot(x=xx, y=prd$Velocity, type="l",
#'      ylim=range(salmon$Velocity, prd$Velocity),
#'      las=1, ylab="Velocity (m/s)", xlab="Location along the transect (m)")
#' points(x = salmon$Position[train], y = salmon$Velocity[train], pch=21,
#'        bg="black")
#' points(x = salmon$Position[test], y = salmon$Velocity[test], pch=21,
#'        bg="red")
#' 
#' ## Spatially-explicit modelling of the mean substrate size (D50):
#' 
#' ## Calculate the minimum MSE model:
#' getMinMSE(
#'   U = as.matrix(sefTrain),
#'   y = salmon$Substrate[train],
#'   Up = predict(sefTrain, salmon$Position[test]),
#'   yy = salmon$Substrate[test]
#' ) -> mseRes[["Substrate"]]
#' 
#' ## This is the coefficient of prediction:
#' 1 - mseRes$Substrate$mse[mseRes$Substrate$wh]/mseRes$Substrate$nullmse
#' 
#' ## Plot of the MSE values:
#' plot(mseRes$Substrate$mse, type="l", ylab="MSE", xlab="order", axes=FALSE,
#'      ylim=c(1000,6000))
#' points(x=1:length(mseRes$Substrate$mse), y=mseRes$Substrate$mse, pch=21,
#'        bg="black")
#' axis(1)
#' axis(2, las=1)
#' abline(h=mseRes$Substrate$nullmse, lty=3)
#' 
#' ## A list of the selected spatial eigenfunctions:
#' sel[["Substrate"]] <- sort(mseRes$Substrate$ord[1:mseRes$Substrate$wh])
#' 
#' ## A linear model build using the selected spatial eigenfunctions:
#' lm(
#'   formula = y~.,
#'   data = cbind(
#'     y = salmon$Substrate[train],
#'     as.data.frame(sefTrain, wh=sel$Substrate)
#'   )
#' ) -> lm[["Substrate"]]
#' 
#' ## Calculating predictions of D50 at each 1 m intervals:
#' predict(
#'   lm$Substrate,
#'   newdata = as.data.frame(
#'     predict(
#'       object = sefTrain,
#'       newdata = xx,
#'       wh = sel$Substrate
#'     )
#'   )
#' ) -> prd[["Substrate"]]
#' 
#' ## Plot of the predicted D50 (solid line), and observed D50 for the training
#' ## set (black markers) and testing set (red markers):
#' plot(x=xx, y=prd$Substrate, type="l",
#'      ylim=range(salmon$Substrate, prd$Substrate), las=1, ylab="D50 (mm)",
#'      xlab="Location along the transect (m)")
#' points(x = salmon$Position[train], y = salmon$Substrate[train], pch=21,
#'        bg="black")
#' points(x = salmon$Position[test], y = salmon$Substrate[test], pch=21,
#'        bg="red")
#' 
#' ## Spatially-explicit modelling of Atlantic salmon parr abundance using
#' ## x=channel depth + water velocity + D50 + pMEM:
#' 
#' ## Requires suggested package glmnet to perform elasticnet regression:
#' library(glmnet)
#' 
#' ## Calculation of the elastic net model (cross-validated):
#' cv.glmnet(
#'   y = salmon$Abundance[train],
#'   x = cbind(
#'     Depth = salmon$Depth[train],
#'     Velocity = salmon$Velocity[train],
#'     Substrate = salmon$Substrate[train],
#'     as.matrix(sefTrain)
#'   ),
#'   family = "poisson"
#' ) -> cvglm
#' 
#' ## Calculating predictions for the test data:
#' predict(
#'   cvglm,
#'   newx = cbind(
#'     Depth = salmon$Depth[test],
#'     Velocity = salmon$Velocity[test],
#'     Substrate = salmon$Substrate[test],
#'     predict(sefTrain, salmon$Position[test])
#'   ),
#'   s="lambda.min",
#'   type = "response"
#' ) -> yhatTest
#' 
#' ## Calculating predictions for the transect (1 m seperated data):
#' predict(
#'   cvglm,
#'   newx = cbind(
#'     Depth = prd$Depth,
#'     Velocity = prd$Velocity,
#'     Substrate = prd$Substrate,
#'     predict(sefTrain, xx)
#'   ),
#'   s = "lambda.min",
#'   type = "response"
#' ) -> yhatTransect
#' 
#' ## Plot of the predicted Atlantic salmon parr abundance (solid line, with the
#' ## depth, velocity, and D50 also predicted using spatially-explicit
#' ## submodels), the observed abundances for the training set (black markers),
#' ## the observed abundances for the testing set (red markers), and the
#' ## predicted abundances for the testing set calculated on the basis of
#' ## observed depth, velocity, and D50:
#' plot(x=xx, y=yhatTransect, type="l",
#'      ylim=range(salmon$Abundance,yhatTransect), las=1,
#'      ylab="Abundance (fish)", xlab="Location along the transect (m)")
#' points(x=salmon$Position[train], y=salmon$Abundance[train], pch=21,
#'        bg="black")
#' points(x=salmon$Position[test], y=salmon$Abundance[test], pch=21, bg="red")
#' points(x=salmon$Position[test], y=yhatTest, pch=21, bg="green")
#' 
#' ## Restoring previous graphical parameters:
#' par(tmp)
#' 
#' @importFrom Rcpp evalCpp
#' 
#' @useDynLib pMEM, .registration = TRUE
#' 
#' @export
getMinMSE <- function(U, y, Up, yy, complete = TRUE) {
  if(is.complex(U)) {
    stop("Not yet implemented for complex numbers!")
  } else
    .Call("pMEM_getMinMSEReal", PACKAGE="pMEM", U, y, Up, yy, complete)
}
#'
