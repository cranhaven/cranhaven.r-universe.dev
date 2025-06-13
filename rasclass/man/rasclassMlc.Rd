\name{rasclassMlc}
\alias{rasclassMlc}
\alias{rasclassMlc,rasclass-method}
\title{Maximum likelihood classifier}

\description{
This function is the rasclass implementation of the Maximum Likelihood Classifier.
}

\details{
This function is used in the wrapping function \code{\link{classifyRasclass}} to perform Maximum Likelihood Classification. It is provided for review and possible alterations, but it is not recommended for direct use, since this function does not include accuracy assessment or in-sample verification. Therefore it is recomended to perform Maximum Likelihood Classification through the function \code{\link{classifyRasclass}} using the 'methods' argument.

The Maximum Likelihood Classification assumes that for each class, the probability of class membership can be described by a multidimensional Gaussian probability density function. Under this assumption, the maximum likelihood estimates of the probability density function parameters are simply the mean vector and covariance matrix of the subset of each class of the training data. The probability of class membership for prediction is then determined by these estimated parameters and the classification is done by selecting the most probable class for each observation. For details see Paola & Schowengerdt (2005) or statistical textbooks.
}

\usage{rasclassMlc(rasclassObj)}

\arguments{
	\item{rasclassObj}{
	A \code{\link{rasclass}} object.
	}
}

\value{
A \code{\link{rasclass-class}} object with the results of the Maximum Likelihood Classification.
}

\references{
Paola, J. D., Schowengerdt, R. A. (1995). A detailed comparison of backpropagation neural network and maximum-likelihood classifiers for urban land use classification. \emph{IEEE Transactions on Geoscience and Remote Sensing},  33(4), 981-996.
}


\seealso{
\code{\link{rasclass-package}},
\code{\link{rasclass-class}},
\code{\link{rasclassRaster-class}},

\code{\link{readRaster}},
\code{\link{writeRaster}},

\code{\link{readRasterFolder}},
\code{\link{setRasclassData}},

\code{\link{buildFormula}},
\code{\link{checkRasclass}},

\code{\link{classifyRasclass}}
}
