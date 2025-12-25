\name{CorrMat}
\alias{CorrMat_Sym}
\alias{CorrMat_Vec}
\title{Two Functions for Constructing the Correlation Matrix in \code{GPM} Package}
\usage{
CorrMat_Sym(X, CorrType, Omega)
CorrMat_Vec(X1, X2, CorrType, Omega)
}
\arguments{
\item{X, X1, X2}{Matrices containing the numeric data points. The rows and columns of both \code{X1} and \code{X2} denote individual observation settings and dimension, respectively.}

\item{CorrType}{The correlation function of the GP model. Choices include \code{'G'} (default), \code{'PE'}, \code{'LBG'}, and \code{'LB'}. See the \code{references} for the details.}

\item{Omega}{The vector storing all the scale (aka roughness) parameters of the correlation function. The length of \code{Omega} depends on the \code{CorrType}. See \code{reference 1}.}
}
\value{
R The Correlation matrix with size \code{nrow(X1)}-by-\code{nrow(X2)}. See \href{https://en.wikipedia.org/wiki/Correlation_matrix}{here}.
}
\description{
The \code{CorrMat_Sym()} function builds the auto-correlation matrix corresponding to dataset \code{X} while the \code{CorrMat_Vec()} function builds the correlation matrix between datasets \code{X1} and \code{X2}.
}
\note{
This function is \strong{NOT} exported once the GPM package is loaded.
}
\examples{
# see the examples in \\code{\\link[GPM]{Fit}}
}
\references{
\enumerate{
\item Bostanabad, R., Kearney, T., Tao, S. Y., Apley, D. W. & Chen, W. (2018) Leveraging the nugget parameter for efficient Gaussian process modeling. International Journal for Numerical Methods in Engineering, 114, 501-516.
\item Plumlee, M. & Apley, D. W. (2017) Lifted Brownian kriging models. Technometrics, 59, 165-177.
}
}
\seealso{
\code{\link[GPM]{Fit}} to see how a GP model can be fitted to a training dataset.\cr
\code{\link[GPM]{Predict}} to use the fitted GP model for prediction.\cr
\code{\link[GPM]{Draw}} to plot the response via the fitted model.
}
