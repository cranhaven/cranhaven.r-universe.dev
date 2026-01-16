\name{TquestDistance}
\alias{TquestDistance}

\title{
Tquest distance. 
}
\description{
Computes the Tquest distance between a pair of numeric vectors.
}
\usage{
TquestDistance(x, y, tx, ty, tau)
}
\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}
 \item{tx}{
  If not constant, temporal index of series \code{x}.
}
\item{ty}{
   If not constant, temporal index of series \code{y}.
}
  \item{tau}{
  Parameter (threshold) used to define the threshold passing intervals.
}
}
\details{
The TQuest distance represents the series based on a set of intervals that fulfill the following conditions: 

\enumerate{
\item All the values that the time series takes during these time intervals must be strictly above a user specified threshold \code{tau}.
\item They are the largest possible intervals that satisfy the previous condition.
}

The final distance between two series is defined in terms of the similarity between their threshold passing interval sets. For more information, see references.

%\bold{Note:} This distance can not be used directly within kernel machines  in all cases (for example, by inserting it in the Gaussian RBF kernel), because it may provide a non positive semi-definite Gram matrix, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. As such, the positive definiteness of the obtained Gram matrix should be analyzed in advance. More information and some solutions to this problem can be found in (Pree, 2014)
}

\value{
\item{d}{
   The computed distance between the pair of series.
}

}

\references{
\enc{Aßfalg}{Abffalg}, J., Kriegel, H., \enc{Kröger}{Kroger}, P., Kunath, P., Pryakhin, A., & Renz, M. (2006). \emph{Similarity Search on Time Series based on Threshold Queries}. In Proceedings of the 10th international conference on Advances in Database Technology (pp. 276-294).

Esling, P., & Agon, C. (2012). Time-series data mining. ACM Computing Surveys (CSUR), 45(1), 1–34.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}



\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}
\examples{
# The objects example.series1 and example.series2 are two 
# numeric series of length 100 contained in the TSdist package. 

data(example.series1)
data(example.series2)

# For information on their generation and shape see help 
# page of example.series.

help(example.series)


# Calculate the Tquest distance assuming even sampling:

TquestDistance(example.series1, example.series2, tau=2.5)

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the TSdist 
# package.

data(example.series3)
data(example.series4)

# Calculate the Tquest distance for two series of different length:

TquestDistance(example.series3, example.series4, tau=2.5)

}

