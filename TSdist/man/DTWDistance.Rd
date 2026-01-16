\name{DTWDistance}
\alias{DTWDistance}

\title{
Dynamic Time Warping distance. 
}
\description{
Computes the Dynamic Time Warping distance between a pair of
numeric time series.
}

\usage{
DTWDistance(x, y, ...)
}

\arguments{
  \item{x}{
  Numeric vector containing the first time series.
}

  \item{y}{
  Numeric vector containing the second time series.
}

  \item{...}{
  Additional parameters for the function. See  \code{\link[dtw]{dtw}} for more 
  information.
}
}

\details{
This is simply a wrapper for the \code{\link[dtw]{dtw}} function of package \pkg{dtw}. As such, all the functionalities of the \code{\link[dtw]{dtw}} function are also available when using this function. 

%\bold{Note:} This distance measure cannot be used directly within kernel machines in all cases (for example, by inserting it in the Gaussian RBF kernel), because it may provide a non positive semi-definite Gram matrix (Lei and Sun, 2007), which is a requirement for kernel based classifiers such as Support Vector Machines or Gaussian Processes. As such, the positive definiteness of the obtained Gram matrix should be analyzed in advance. More information and some solutions to this problem can be found in (Cuturi, 2011; Gaidon et al. 2011; Marteau and Gibet, 2014; Lei and Sun, 2007; Pree, 2014)
}

\value{
\item{d}{
  The computed distance between the pair of series.
}


}

\references{
Giorgino T (2009). \emph{Computing and Visualizing Dynamic Time Warping Alignments in R:
The dtw Package.} Journal of Statistical Software, 31(7), pp. 1-24. URL:http://www.jstatsoft.org/v31/i07/

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
To calculate a lower bound of the DTW distance see \code{\link{LBKeoghDistance}}.

To calculate this distance measure using \code{ts}, \code{zoo} or \code{xts} objects see \code{\link{TSDistances}}. To calculate distance matrices of time series databases using this measure see \code{\link{TSDatabaseDistances}}.
}
\examples{

# The objects example.series3 and example.series4 are two 
# numeric series of length 100 and 120 contained in the TSdist 
# package 


data(example.series3)
data(example.series4)

# For information on their generation and shape see 
# help page of example.series.

help(example.series)

# Calculate the basic DTW distance for two series of different length.

DTWDistance(example.series3, example.series4)

# Calculate the DTW distance for two series of different length
# with a sakoechiba window of size 30:

DTWDistance(example.series3, example.series4, window.type="sakoechiba", window.size=30)

# Calculate the DTW distance for two series of different length
# with an assymetric step pattern

DTWDistance(example.series3, example.series4, step.pattern=asymmetric)

}
