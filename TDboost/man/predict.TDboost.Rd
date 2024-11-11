\name{predict.TDboost}
\alias{predict.TDboost}
\title{ Predict method for TDboost Model Fits }
\description{
  Predicted values based on an TDboost Tweedie regression model object
}
\usage{
\method{predict}{TDboost}(object,
        newdata,
        n.trees,
        single.tree=FALSE,
		  type=c("response","link"),
        ...)
}
\arguments{
  \item{object}{ Object of class inheriting from (\code{\link{TDboost.object}}) }
  \item{newdata}{ Data frame of observations for which to make predictions }
  \item{n.trees}{ Number of trees used in the prediction. \code{n.trees} may
                  be a vector in which case predictions are returned for each
                  iteration specified}
  \item{single.tree}{If \code{single.tree=TRUE} then \code{predict.TDboost} returns
                     only the predictions from tree(s) \code{n.trees}} 
	\item{type}{
	type of prediction required. \itemize{
			\item Type \code{"response"} gives predicted response mu(x) = E(Y|X=x) for the regression problems. It is the default.
			\item Type \code{"link"} gives the linear predictors x*b = log(mu(x)) = log(E(Y|X=x)) for the regression problems.}}
  \item{\dots}{ further arguments passed to or from other methods }
}
\details{
\code{predict.TDboost} produces predicted values for each observation in \code{newdata} using the the first \code{n.trees} iterations of the boosting sequence. If \code{n.trees} is a vector than the result is a matrix with each column representing the predictions from TDboost models with \code{n.trees[1]} iterations, \code{n.trees[2]} iterations, and so on.

The predictions from \code{TDboost} do not include the offset term. The user may add the value of the offset to the predicted value if desired.

If \code{object} was fit using \code{\link{TDboost.fit}} there will be no
\code{Terms} component. Therefore, the user has greater responsibility to make
sure that \code{newdata} is of the same format (order and number of variables)
as the one originally used to fit the model.
}
\value{

Returns a vector of predictions. By default the predictions are on the scale of f(x).
}
\author{Yi Yang \email{yi.yang6@mcgill.ca}, Wei Qian \email{wxqsma@rit.edu} and Hui Zou \email{hzou@stat.umn.edu}}
\seealso{
\code{\link{TDboost}}, \code{\link{TDboost.object}}
}

\keyword{ models }
\keyword{ regression }
