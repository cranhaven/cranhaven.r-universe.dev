\name{emfa}
\alias{emfa}
\title{Expectation-Maximization (EM) estimation of a factor model.}
\description{This function implements the EM algorithm by Thayer and Rubin (1982) for the ML estimation of a factor model. It is an 
internal function used to estimate the factor model parameters in the factor-adjustment methods.}
\usage{
emfa(dta, nbf, min.err = 1e-06, verbose = FALSE, svd.method = c("fast.svd", "irlba"))
}
\arguments{
  \item{dta}{n x m matrix which rows are multivariate m-profiles for n individuals. n can be much smaller than m.}
  \item{nbf}{number of factors. It has to be a positive integer, smaller than the rank of \code{dta}.}
  \item{min.err}{stopping criterion for the iterative algorithm. Maximum difference between the estimated parameters in the last two iterations.}
  \item{verbose}{logical value. If \code{verbose}=TRUE, then some information is printed along the calculation.}
  \item{svd.method}{the EM algorithm starts from an SVD estimation of the factor model parameters. The default option to implement this 
      SVD is \code{\link[corpcor]{fast.svd}}. An alternative option is an approximate but faster SVD by function 
      \code{\link[irlba]{irlba}}.}
}
\details{
Data are centered but not scaled. If a factor model for a correlation matrix is to be estimated, then a scaled dataset is required as 
an input of the function.
}
\value{
\item{B}{m x \code{nbf} matrix of loadings}
\item{Psi}{m-vector of specific variances}
\item{Factors}{n x \code{nbf} matrix of factor scores}
\item{Objective}{Final value of the stopping criterion, after convergence.}
}
\references{
Friguet, C., Kloareg, M. and Causeur, D. (2009). A factor model approach to multiple testing under dependence. 
Journal of the American Statistical Association. 104 (488), 1406-1415.

Rubin, D. B., and Thayer, D. T. (1982), EM Algorithms for ML Factor Analysis, Psychometrika, 47 (1), 69-76.
}
\author{
David Causeur, IRMAR, UMR 6625 CNRS, Agrocampus Ouest, Rennes, France.
}
\seealso{
\code{\link[stats]{factanal}}
}
\examples{

data(impulsivity)

erpdta = as.matrix(impulsivity[,5:505]) # erpdta contains the whole set of ERP curves  
   
fa = emfa(erpdta,nbf=1) # 1-factor modelling of the ERP curves
fa$Objective            # Final difference between the last two iterations
Semp = var(erpdta)      # Sample estimation of the variance of ERP curves
Sfa = diag(fa$Psi)+tcrossprod(fa$B) # Factorial estimation of the variance 
max(abs(Semp-Sfa))      # Distance between the two estimates 

fa = emfa(erpdta,nbf=20) # 20-factor modelling of the ERP curves in erpdta
fa$Objective             # Final difference between the last two iterations
Semp = var(erpdta)       # Sample estimation of the variance of ERP curves
Sfa = diag(fa$Psi)+tcrossprod(fa$B) # Factorial estimation of the variance 
max(abs(Semp-Sfa))       # Distance between the two estimates

}