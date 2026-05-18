\name{predict.larf}
\alias{predict.larf}

\title{Predictions Based on the Estimated LARF}
\description{
Predict new outcomes based on the model fitted by \code{\link{larf}}.
}
\usage{
\method{predict}{larf}(object, newCov, newTreatment, \dots)
}

\arguments{
  \item{object}{an object of class \code{larf} as fitted by \code{\link{larf}}.}
  \item{newCov}{A matrix containing the new covariates.}
  \item{newTreatment}{A vector containing the new binary treatment.}
  \item{\dots}{currently not used.}
}

\details{
Predicted outcomes are based on the estimated coefficients and new covariates and/or new treatment. The predicted outcomes are probabilities when the outcome is binary.
}

\value{
\item{predicted.values}{The function returns a vector of the predicted outcomes.}
}

\author{
Weihua An and Xuefu Wang, Departments of Statistics and Sociology, Indiana University Bloomington
}

\seealso{
\code{\link{larf}}, \code{\link{larf.fit}}
}
