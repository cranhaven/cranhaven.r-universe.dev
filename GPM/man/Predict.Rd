\name{Predict}
\alias{Predict}
\title{The Prediction Function of \code{GPM} Package}
\usage{
Predict(XF, Model, MSE_on = 0, YgF_on = 0, grad_dim = rep(1, ncol(XF)))
}
\arguments{
\item{XF}{Matrix containing the locations (settings) where the predictions are desired. The rows and columns of \code{XF} denote individual observation settings and input dimension, respectively.}

\item{Model}{The GP model fitted by \code{\link[GPM]{Fit}}.}

\item{MSE_on}{Flag (a scalar) indicating whether the uncertainty (i.e., mean squared error \code{MSE}) associated with prediction of the response(s) should be calculated. Set to a non-zero value to calculate \code{MSE}.}

\item{YgF_on}{Flag (a scalar) indicating whether the gradient(s) of the response(s) are desired. Set to a non-zero value to calculate the gradient(s). See \code{note} below.}

\item{grad_dim}{A binary vector of length \code{ncol(XF)}. The gradient of the response(s) will be calculated along the dimensions where the corresponding element of \code{grad_dim} is \code{1}. \code{grad_dim} is ignored if \code{YgF_on == 0}.}
}
\value{
Output A list containing the following components:
\itemize{
\item{\code{YF}} {A matrix with \code{n} rows (the number of prediction points) and \code{dy} columns (the number of responses).}
\item{\code{MSE}} {A matrix with \code{n} rows and \code{dy} columns where each element represents the prediction uncertainty (i.e., the expected value of the squared difference between the prediction and the true response) associated with the corresponding element in \code{YF}.}
\item{\code{YgF}} {An array of size \code{n} by \code{sum{grad_dim}} by \code{dx}.}
}
}
\description{
Predicts the reponse(s), associated prediction uncertainties, and gradient(s) of the GP model fitted by \code{\link[GPM]{Fit}}.
}
\note{
\enumerate{
\item The gradient(s) can be calculated if \code{CorrType='G'} or \code{CorrType='LBG'}. If \code{CorrType='PE'} or \code{CorrType='LB'}, the gradient(s) can only be calculated if \code{Power = 2} and \code{Gamma = 1}, respectively.
\item For efficiency, make sure the inputs are vecotrized and then passed to \code{\link[GPM]{Predict}}. Avoid passing inputs individually in a \code{for} loop.
}
}
\examples{
# See the examples in the fitting function.
}
\references{
\enumerate{
\item Bostanabad, R., Kearney, T., Tao, S., Apley, D. W. & Chen, W. (2018) Leveraging the nugget parameter for efficient Gaussian process modeling. Int J Numer Meth Eng, 114, 501-516.
\item Plumlee, M. & Apley, D. W. (2017) Lifted Brownian kriging models. Technometrics, 59, 165-177.
}
}
\seealso{
\code{\link[GPM]{Fit}} to see how a GP model can be fitted to a training dataset.\cr
\code{\link[GPM]{Draw}} to plot the response via the fitted model.
}
