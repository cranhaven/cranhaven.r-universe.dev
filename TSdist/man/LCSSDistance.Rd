\name{LCSSDistance}
\alias{LCSSDistance}

\title{
Longest Common Subsequence distance for Real Sequences. 
}
\description{
Computes the Longest Common Subsequence distance between a pair of 
numeric time series.
}

\usage{
LCSSDistance(x, y, epsilon, sigma)
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
The Longest Common Subsequence for two real sequences is computed. 

For this purpose, 
the distances between the points of \code{x} and \code{y} are reduced to 0 or 1. If the Euclidean distance between two points \eqn{x_i} and \eqn{y_j} is smaller than \code{epsilon} they are considered
equal and their distance is reduced to 0. In the opposite case, 
the distance between them is represented with a value of 1. 

Once the distance matrix is defined in this manner, the maximum common subsequence is seeked. Of course, as in other Edit Based Distances, gaps or unmatched regions are permitted and they are penalized with a value proportional to their length.

Based on its definition, the length of series \code{x} and \code{y} may be 
different.

If desired, a temporal constraint may be added to the LCSS 
distance. In this package, only the most basic windowing function, introduced 
by H.Sakoe and S.Chiba (1978), is implemented. This function sets a band around the 
main diagonal of the distance matrix and avoids the matching of the points that are farther in time than a specified \eqn{\sigma}. 

The size of the window must be a positive integer value. Furthermore, the following condition must be fulfilled:

\deqn{|length(x)-length(y)| < sigma}

%\bold{Note:} This distance measure is not a metric (Chen and Ng, 2004), and is not negative definite (Lei and Sun, 2007). This means that it can not be used directly within kernel machines  in all cases (e.g. by inserting it in the Gaussian RBF kernel), because it may provide a non positive semi-definite Gram matrix, which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. As such, the positive definiteness of the obtained Gram matrix should be analyzed in advance. More information and some solutions to this problem can be found in (Cuturi, 2011; Gaidon et al. 2011; Marteau and Gibet, 2014; Lei and Sun, 2007; Pree, 2014)
}

\value{
\item{d}{
  The computed distance between the pair of series.
}

}

\references{
Vlachos, M., Kollios, G., & Gunopulos, D. (2002). \emph{Discovering similar multidimensional trajectories}. In Proceedings 18th International Conference on Data Engineering (pp. 673-684). IEEE Comput. Soc. doi:10.1109/ICDE.2002.994784

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

# Calculate the LCSS distance for two series of different length
# with no windowing constraint:

LCSSDistance(example.series3, example.series4, epsilon=0.1)

# Calculate the LCSS distance for two series of different length
# with a window of size 30:

LCSSDistance(example.series3, example.series4, epsilon=0.1, sigma=30)

}
