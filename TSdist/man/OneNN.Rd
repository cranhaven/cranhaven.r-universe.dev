\name{OneNN}
\alias{OneNN}

\title{
1NN classification for a pair of train/test time series datasets.
}

\description{
Given a specific distance measure, this function provides the 1NN classification values and the associated error for a specific train/test pair of time series databases.}

\usage{
OneNN(train, trainc, test, testc, distance, ...)
}

\arguments{
  \item{train}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object.}
  
    \item{trainc}{
 Numerical vector which indicates the class of each of the series in the training set.}

  \item{test}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object.}
  
      \item{testc}{
 Numerical vector which indicates the class of each of the series in the testing set.}

  \item{distance}{
 Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infnorm"}, \code{"ccor"}, \code{"sts"}, \code{"dtw"}, \code{"keogh_lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissimfull"}, \code{"dissimapprox"}, \code{"acf"}, \code{"pacf"}, \code{"ar.lpc.ceps"}, \code{"ar.mah"}, \code{"ar.mah.statistic"}, \code{"ar.mah.pvalue"}, \code{"ar.pic"}, \code{"cdm"}, \code{"cid"}, \code{"cor"}, \code{"cort"}, \code{"wav"}, \code{"int.per"}, \code{"per"}, \code{"mindist.sax"}, \code{"ncd"}, \code{"pred"}, \code{"spec.glk"}, \code{"spec.isd"},
                        \code{"spec.llr"}, \code{"pdc"}, \code{"frechet"})
}
\item{...}{
  Additional parameters required by the chosen distance measure.
}
}

\details{
This function is useful to evaluate the performance of different distance measures in the task of classification of time series.
}

\value{
\item{classes}{
  Numerical vector providing the predicted class values for the series in the test set.
}

\item{error}{
  Error obtained in the 1NN classification process.
}
}


\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\seealso{
 To calculate the distance matrices of time series databases the \code{\link{TSDatabaseDistances}} is used.
}

\examples{

# The example.database2 synthetic database is loaded
data(example.database2)

# Create train/test by dividing the dataset 70%-30%
set.seed(100)
trainindex <- sample(1:100, 70, replace=FALSE)
train <- example.database2[[1]][trainindex, ]
test <- example.database2[[1]][-trainindex, ]
trainclass <- example.database2[[2]][trainindex]
testclass <- example.database2[[2]][-trainindex]

# Apply the 1NN classifier for different distance measures

OneNN(train, trainclass, test, testclass, "euclidean")
OneNN(train, trainclass, test, testclass, "pdc")


}
