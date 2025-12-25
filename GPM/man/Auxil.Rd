\name{Auxil}
\alias{Auxil}
\title{An auxiliary function used in calculating the negative log-likelehood and its gradient}
\usage{
Auxil(Omega, X, Y, CorrType, MinEig, Fn, n, dy)
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
ALL A list containing the following components (based on \code{CorrType}, some other parameters are also stored in \code{ALL}):
\itemize{
\item{\code{R}} {The correlation matrix whose smallest eigen value is \code{>= MinEig}.}
\item{\code{L}} {Cholesky decomposition of \code{R}.}
\item{\code{Raw_MinEig}} {The smallest eigen value of \code{R} before adding \code{Nug_opt}.}
\item{\code{Nug_opt}} {The added nugger to \code{R}.}
\item{\code{B}}
}

}
\description{
Calculates some auxiliary paramters to obtain the negative log-likelehood and its gradient.
}
\details{
Since \code{Auxil} is shared between \code{NLogL} and \code{NLogL_G} during optimization, ideally it should be run only once (e.g., via memoisation). Such an implementation is left for future editions.
}
\examples{
# see the examples in the fitting function.
}
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
