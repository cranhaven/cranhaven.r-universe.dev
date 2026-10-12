\name{nbfactors}
\alias{nbfactors}
\title{
Determination of the number of factors in high dimensional factor models.}
\description{
This function is not supposed to be called directly by users. It implements the variance inflation method proposed by Friguet et al. (2009) 
to choose the number of factors in high dimensional factor models.}
\usage{
nbfactors(dta,maxnbfactors=15,diagnostic.plot=FALSE,min.err=0.001,verbose=FALSE,
svd.method=c("fast.svd","irlba"))
}
\arguments{
  \item{dta}{n x m scaled matrix which rows are multivariate m-profiles for n individuals. n can be much smaller than m. 
  Columns are all centered with standard deviations 1.}
  \item{maxnbfactors}{Maximum number of factors. It has to be a positive integer, smaller than the rank of \code{dta}.}
  \item{diagnostic.plot}{Logical value. If TRUE, a plot dispalying the variance inflation curve is produced, with recommendations for the optimal number of factors.}
  \item{min.err}{stopping criterion for the iterative algorithm. Maximum difference between the estimated parameters in the last two iterations.}
  \item{verbose}{logical value. If \code{verbose}=TRUE, then some information is printed along the calculation.}
  \item{svd.method}{the EM algorithm starts from an SVD estimation of the factor model parameters. The default option to implement this 
      SVD is \code{\link[corpcor]{fast.svd}}. An alternative option is an approximate but faster SVD by function 
      \code{\link[irlba]{irlba}}.}
}
\details{
It is highly recommended to run the function first with diagnostic.plot=TRUE and to choose the number of factors based on the plot that will be produced.
Two recommendations are provided: a conservative one obtaned using a kind of elbow criterion, and a liberal one that minimizes the variance inflation curve.
}
\value{
\item{sdt}{Variance inflation values for a number of factors going from 0 to \code{maxnbfactors}.}
\item{nbf}{Recommendation for an optimal number of factors (the more liberal, see the details Section for more information).}
}
\references{
Friguet, C., Kloareg, M. and Causeur, D. (2009). A factor model approach to multiple testing under dependence. 
Journal of the American Statistical Association. 104 (488), 1406-1415.
}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link{emfa}}
}
\examples{
data(impulsivity)
erpdta = as.matrix(impulsivity[,5:505])   # erpdta contains the whole set of ERP curves   
nbf = nbfactors(scale(erpdta),maxnbfactors=10,diagnostic.plot=TRUE)  
}
