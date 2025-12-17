\name{select}
\alias{select}
\title{
select from a "fanc" object for fixed value of gamma.
}
\description{
This functions give us the loadings from a "fanc" object for fixed value of gamma.  
}
\usage{
select(x, criterion=c("BIC","AIC","CAIC","EBIC"), 
	gamma,  scores=FALSE, df.method="active")
}
\arguments{
  \item{x}{
Fitted \code{"fanc"} model object.
}
  \item{criterion}{
The criterion by which to select the tuning parameter rho. One of "AIC", "BIC", "CAIC", or "EBIC". Default is "BIC".
}
  \item{gamma}{
The value of gamma.
}
  \item{scores}{
Logical flag for outputting the factor scores. Defalut is FALSE.
}
  \item{df.method}{
Two types of degrees of freedom are supported.  If \code{"active"}, the degrees of freedom of are the number of nonzero parameters.  If \code{"reparametrization"}, the degrees of freedom of the MC+ are reparametrized based on the degrees of freedom of the lasso.  
}
}
\value{
\item{loadings}{factor loadings}
\item{uniquenesses}{unique variances}
\item{Phi}{factor correlation}
\item{scores}{factor scores}
\item{df}{degrees of freedom (number of non-zero parameters for the lasso estimation)}
\item{criteria}{values of AIC, BIC and CAIC}
\item{goodness.of.fit}{values of GFI and AGFI}
\item{rho}{a value of rho}
\item{gamma}{a value of gamma}
}
\references{
Hirose, K. and Yamamoto, M. (2014). 
\emph{Sparse estimation via nonconcave penalized likelihood in a factor analysis model},\cr
  \emph{Statistics and Computing, in press}
}
\author{
Kei Hirose\cr
\email{mail@keihirose.com}
}

\seealso{\code{fanc} and \code{plot.fanc} objects.}

