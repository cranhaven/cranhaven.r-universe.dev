\name{NLogL_G}
\alias{NLogL_G}
\title{The Function for calculating the gradient of Negative Log-Likelehood in \code{GPM} Package}
\usage{
NLogL_G(Omega, X, Y, CorrType, MinEig, Fn, n, dy)
}
\arguments{
\item{Omega}{The vector storing all the hyperparameters of the correlation function. The length of \code{Omega} depends on the \code{CorrType}. See \code{reference 1}.}

\item{X}{Matrix containing the training (aka design or input) data points. The rows and columns of \code{X} denote individual observation settings and input dimension, respectively.}

\item{Y}{Matrix containing the output (aka response) data points. The rows and columns of \code{Y} denote individual observation responses and output dimension, respectively.}

\item{CorrType}{The correlation function of the GP model. Choices include \code{'G'} (default), \code{'PE'}, \code{'LBG'}, and \code{'LB'}. See \code{\link[GPM]{Fit}} and the \code{references}.}

\item{MinEig}{The smallest eigen value that the correlation matrix is allowed to have, which in return determines the appraopriate nugget that should be added to the correlation matrix.}

\item{Fn}{A matrix of \code{1}'s with \code{nrow(X)} rows and \code{1} column. See \code{reference 1}.}

\item{n}{Number of observations, \code{nrow(X)}.}

\item{dy}{Number of responses, \code{ncol(Y)}.}
}
\value{
NLogL_G The gradient of negative log-likelihood wrt \code{Omega}. See the \code{references}.
}
\description{
Calculates the gradient of negative log-likelihood wrt \code{Omega}.
}
\details{This function is used in \code{\link[GPM]{Fit}} if \code{AnaGr != 0}.}
\examples{# see the examples in the fitting function.}
\references{
\enumerate{
\item Bostanabad, R., Kearney, T., Tao, S., Apley, D. W. & Chen, W. (2018) Leveraging the nugget parameter for efficient Gaussian process modeling. Int J Numer Meth Eng, 114, 501-516.
\item Plumlee, M. & Apley, D. W. (2017) Lifted Brownian kriging models. Technometrics, 59, 165-177.
}
}
\seealso{
\code{\link[GPM]{Fit}} to see how a GP model can be fitted to a training dataset.\cr
\code{\link[GPM]{Predict}} to use the fitted GP model for prediction.\cr
\code{\link[GPM]{Draw}} to plot the response via the fitted model.
}