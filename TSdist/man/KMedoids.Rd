\name{KMedoids}
\alias{KMedoids}

\title{
K medoids clustering for a time series database using the selected distance measure.
}

\description{
Given a specific distance measure and a time series database, this function provides the K-medoids clustering result. Furthermore, if the ground truth clustering is provided,  and the associated F-value is also provided.}

\usage{
KMedoids(data,  k, ground.truth, distance, ...)
}

\arguments{
  \item{data}{
  Time series database saved in a numeric matrix, a list, an \code{mts} object, a \code{zoo} object  or \code{xts} object.}

      \item{k}{
 Integer value which represents the number of clusters.}
  
    \item{ground.truth}{
 Numerical vector which indicates the ground truth clustering of the database.}

  \item{distance}{
 Distance measure to be used. It must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"infnorm"}, \code{"ccor"}, \code{"sts"}, \code{"dtw"}, \code{"keogh_lb"}, \code{"edr"}, \code{"erp"}, \code{"lcss"}, \code{"fourier"}, \code{"tquest"}, \code{"dissimfull"}, \code{"dissimapprox"}, \code{"acf"}, \code{"pacf"}, \code{"ar.lpc.ceps"}, \code{"ar.mah"}, \code{"ar.mah.statistic"}, \code{"ar.mah.pvalue"}, \code{"ar.pic"}, \code{"cdm"}, \code{"cid"}, \code{"cor"}, \code{"cort"}, \code{"wav"}, \code{"int.per"}, \code{"per"}, \code{"mindist.sax"}, \code{"ncd"}, \code{"pred"}, \code{"spec.glk"}, \code{"spec.isd"},
                        \code{"spec.llr"}, \code{"pdc"}, \code{"frechet"})
}
\item{...}{
  Additional parameters required by the chosen distance measure.
}
}

\details{
This function is useful to evaluate the performance of different distance measures in the task of clustering time series.
}

\value{
\item{clustering}{
  Numerical vector providing the clustering result for the database.
}

\item{F}{
  F-value corresponding to the clustering result.
}
}


\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\seealso{
 To calculate the distance matrices of time series databases the \code{\link{TSDatabaseDistances}} is used.
}

\examples{

# The example.database3 synthetic database is loaded
data(example.database3)
tsdata <- example.database3[[1]]
groundt <- example.database3[[2]]

# Apply K-medoids clusterning for different distance measures

KMedoids(data=tsdata, ground.truth=groundt, k=5, "euclidean")
KMedoids(data=tsdata, ground.truth=groundt, k=5, "cid")
KMedoids(data=tsdata, ground.truth=groundt, k=5, "pdc")


}
