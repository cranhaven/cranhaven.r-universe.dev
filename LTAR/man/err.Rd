\name{err}
\alias{err}
\title{Forecast error calculations}

\description{
Determines the Frobenius norm between true tensor and the actual tensor.
}

\usage{
err(true_tensor, forecast_tensor)
}

\arguments{
\item{true_tensor}{: The true tensor from the test set.}
\item{forecast_tensor}{: The predicted values from LTARpred()}
}
\value{
Error: The Frobenius norm between the actual and predictor tensor slices at each forecast step.
}


\author{
Kyle Caudle

Randy Hoover

Jackson Cates
}
\examples{
require(rTensor)
data(tensor)
tnsr <- as.tensor(tensor)
tensorTest <- tnsr[,1:2,]
tensorTrain <- tnsr[,3:2000,]
forecast <- LTARpred(p=5,tensorTrain,h=2)
predTensor <- forecast$ypred
errors <- err(tensorTest,predTensor)
errors
}

