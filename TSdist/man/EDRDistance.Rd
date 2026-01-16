\name{EDRDistance}
\alias{EDRDistance}

\title{
Edit Distance for Real Sequences (EDR). 
}
\description{
Computes the Edit Distance for Real Sequences  between a pair of 
numeric time series.
}

\usage{
EDRDistance(x, y, epsilon, sigma)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}
  \item{y}{
  Numeric vector containing the second time series.
}

\item{epsilon}{
  A positive threshold value that defines the distance.
}

\item{sigma}{
  If desired, a Sakoe-Chiba windowing contraint can be added by specifying 
  a positive integer representing the window size.
}

}
\details{
The basic Edit Distance for Real Sequences between two numeric series is calculated. The idea is to count the number of edit operations (insert, delete, replace) that are necessary to transform one series into the other. 

For that, if the Euclidean distance between two points \eqn{x_i} and \eqn{y_i} is smaller that \code{epsilon} they will be considered equal (\eqn{d=0}) and if they are farther apart, they will be considered different (\eqn{d=1}).
As a last detail, this distance permits gaps or sequences of points that are not matched with any other point.

The length of series \code{x} and \code{y} may be different. Furthermore, if desired, a temporal constraint may be added to the EDR 
distance. In this package, only the most basic windowing function, introduced 
by H.Sakoe and S.Chiba (1978), is implemented. This function sets a band around the main diagonal of the distance matrix and avoids the matching of the points that are farther in time than a specified \eqn{\sigma}. 

The size of the window must be a positive integer value. Furthermore, the following condition must be fulfilled:

\deqn{|length(x)-length(y)| < sigma}

%\bold{Note:} This distance measure is not a metric (Chen and Ng, 2004), and, is not negative definite (Lei and Sun, 2007). This means that it can not be used directly within kernel machines  in all cases (for example, by inserting it in the Gaussian RBF kernel), because it may provide a non positive semi-definite Gram matrix, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. As such, the positive definiteness of the obtained Gram matrix should be analyzed in advance. More information and some solutions to this problem can be found in (Cuturi, 2011; Gaidon et al. 2011; Marteau and Gibet, 2014; Lei and Sun, 2007; Pree, 2014)


}

\value{
\item{d}{
  The computed distance between the pair of series.
}
}

\references{
Chen, L., Ozsu, M. T., & Oria, V. (2005). \emph{Robust and Fast Similarity Search for Moving Object Trajectories}. In Proceedings of the 2005 ACM SIGMOD International Conference on Management of Data (pp. 491-502).

Chen, L., & Ng, R. (2004). On The Marriage of Lp-norms and Edit Distance. In Proceedings of the Thirtieth International Conference on Very Large Data Bases (pp. 792–803).

Cuturi, M. (2011). Fast Global Alignment Kernels. In Proceedings of the 28th International Conference on Machine Learning (pp. 929–936). 

Gaidon, A., Harchaoui, Z., & Schmid, C. (2011). A time series kernel for action recognition. In BMVC 2011 - British Machine Vision Conference (pp. 63.1–63.11).

Marteau, P.-F., & Gibet, S. (2014). On Recursive Edit Distance Kernels With Applications To Time Series Classification. IEEE Transactions on Neural Networks and Learning Systems, PP(6), 1–13.

Lei, H., & Sun, B. (2007). A Study on the Dynamic Time Warping in Kernel Machines. In 2007 Third International IEEE Conference on Signal-Image Technologies and Internet-Based System (pp. 839–845). 

Pree, H., Herwig, B., Gruber, T., Sick, B., David, K., & Lukowicz, P. (2014). On general purpose time series similarity measures and their use as kernel functions in support vector machines. Information Sciences, 281, 478–495.
}

\author{
Usue Mori, Alexander Mendiburu, Jose A. Lozano.
}

\seealso{
To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}

\examples{

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the TSdist
# package. 

data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the EDR distance for two series of different length
# with no windowing constraint:

EDRDistance(example.series3, example.series4, epsilon=0.1)

# Calculate the EDR distance for two series of different length
# with a window of size 30:

EDRDistance(example.series3, example.series4, epsilon=0.1, sigma=30)

}
