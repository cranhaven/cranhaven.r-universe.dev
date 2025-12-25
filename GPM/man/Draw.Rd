\name{Draw}
\alias{Draw}
\title{The Plotting Function of \code{GPM} Package}
\usage{
Draw(Model, Plot_wrt, LB = NULL, UB = NULL, Values = NULL,
  Response_ID = NULL, res = 15, X1Label = NULL, X2Label = NULL,
  YLabel = NULL, Title = NULL, PI95 = NULL)
}
\arguments{
\item{Model}{The GP model fitted by \code{\link[GPM]{Fit}}.}

\item{Plot_wrt}{A binary vector of length \code{p} where \code{p} is the dimension of the inputs in \code{Model}. A maximum (minimum) of \code{2} (\code{1}) elements can be \code{1}. The elemenets set to \code{1}, would correspond to the plotting axes.}

\item{LB, UB}{Vectors of length \code{sum(Plot_wrt)} indicating the lower and upper bounds used for plotting. The first (second) element corresponds to the first (second) non-zero element of \code{Plot_wrt}.}

\item{Values}{A vector of length \code{p-sum(Plot_wrt)}. The values are assigned to the variables NOT used in plotting and correspond to the zeros in \code{Plot_wrt}.}

\item{Response_ID}{A positive integer indicating the response that should be plotted if \code{Model} is multi-response.}

\item{res}{A positive integer indicating the number of points used in plotting. Higher values will result in smoother plots.}

\item{X1Label}{A string for the label of axis \code{1}.}

\item{X2Label}{A string for the label of axis \code{2}, if plotting a surface.}

\item{YLabel}{A string for the label of the response axis.}

\item{Title}{A string for the title of the plot.}

\item{PI95}{Flag (a scalar) indicating whether the \code{95\%} prediction interval should be plotted. Set it to a non-zero value to turn the flag "on".}
}
\description{
Plots the predicted response along with the assocaited uncertainty via the GP model fitted by \code{\link[GPM]{Fit}}. Accepts multi-input and multi-output models. See \code{Arguments} for more details on the options.
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
\code{\link[GPM]{Predict}} to use the fitted GP model for prediction.\cr
}
