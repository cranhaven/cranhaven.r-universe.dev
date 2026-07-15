\name{dataSimilarity}
\alias{dataSimilarity}


\title{ Evaluate statistical similarity of two data sets}
\description{
   Use mean, standard deviation, skewness, kurtosis, Hellinger distance and KS test to compare 
   similarity of two data sets.
    }
\usage{
dataSimilarity(data1, data2, dropDiscrete=NA)
}

\arguments{
  \item{data1}{ A \code{data.frame} containing the reference data.}
  \item{data2}{ A \code{data.frame} with the same number and names of columns as \code{data1}.}
  \item{dropDiscrete}{ A vector discrete attribute indices to skip in comparison. Typically we might skip class, 
        because its distribution was forced by the user.}
  }
\details{
   The function compares data stored in \code{data1} with \code{data2} on per attribute basis by
    computing several statistics:
   mean, standard deviation, skewness, kurtosis, Hellinger distance and KS test. 
 }
\value{
  The method returns a list of statistics computed on both data sets:
\item{equalInstances}{The number of instances in \code{data2} equal to the instances in \code{data1}.}
\item{stats1num}{A matrix with rows containing statistics (mean, standard deviation, skewness, and kurtosis) computed on numeric attributes of \code{data1}.}
\item{stats2num}{A matrix with rows containing statistics (mean, standard deviation, skewness, and kurtosis) computed on numeric attributes of \code{data2}.}
\item{ksP}{A vector with p-values of Kolmogorov-Smirnov two sample tests, performed on matching attributes from \code{data1} and \code{data2}.}
\item{freq1}{A list with value frequencies for discrete attributes in \code{data1}.}
\item{freq2}{A list with value frequencies for discrete attributes in \code{data2}.}
\item{dfreq}{A list with differences in frequencies of discrete attributes' values between  \code{data1} and \code{data2}.}
\item{dstatsNorm}{A matrix with rows containing difference between statistics (mean, standard deviation, skewness, and kurtosis) 
                  computed on [0,1] normalized numeric attributes for \code{data1} and \code{data2.}}
\item{hellingerDist}{A vector with Hellinger distances between matching attributes from \code{data1} and \code{data2}.} 
  }

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{newdata.RBFgenerator}}.
}
\examples{
# use iris data set, split into training and testing data
set.seed(12345)
train <- sample(1:nrow(iris),size=nrow(iris)*0.5)
irisTrain <- iris[train,]
irisTest <- iris[-train,]

# create RBF generator
irisGenerator<- rbfDataGen(Species~.,irisTrain)

# use the generator to create new data
irisNew <- newdata(irisGenerator, size=100)

# compare statistics of original and new data
dataSimilarity(irisTest, irisNew)

}


\keyword{ datagen }
\keyword{ multivariate }