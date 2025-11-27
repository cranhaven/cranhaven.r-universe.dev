\name{ordPCA}
\alias{ordPCA}
\title{Penalized nonlinear PCA for ordinal variables

}

\description{This function performs nonlinear principal components analysis when the variables of interest have ordinal level scale using a second-order difference penalty.

}

\usage{
ordPCA(H, p, lambda = c(1), maxit = 100, crit = 1e-7, qstart = NULL, 
       Ks = apply(H,2,max), constr = rep(FALSE, ncol(H)), trace = FALSE,
       CV = FALSE, k = 5, CVfit = FALSE)
} 

\arguments{

  \item{H}{a matrix or data frame of of integers 1,2,... giving the observed levels
  of the ordinal variables; provides the data for the principal components analysis.}
  \item{p}{the number of principal components to be extracted.}
  \item{lambda}{a numeric value or a vector (in decreasing order) defining the amount of shrinkage; defaults to 1.}
  \item{maxit}{the maximum number of iterations; defaults to 100.}
  \item{crit}{convergence tolerance; defaults to 1e-7.}
  \item{qstart}{optional list of quantifications for the initial linear PCA.}
  \item{Ks}{a vector containing the highest level of each variable.}
  \item{constr}{a logical vector specifying whether monotonicity constraints should be applied to the variables.}
  \item{trace}{logical; if \code{TRUE}, tracing information on the progress of the optimization is produced in terms of VAF in each iteration.}
  \item{CV}{a logical value indicating whether k-fold cross-validation should be performed in order to evaluate the
  performance and/or select an optimal smoothing parameter.}
  \item{k}{the number of folds to be specified; only if \code{CV} is set to \code{TRUE}.}
  \item{CVfit}{logical; to be specified only if \code{CV = TRUE}. If \code{CVfit = TRUE} and \code{lambda} is a vector of length > 5, additional yes/no dialog appears;
  if \code{FALSE}, only VAF values are provided (recommended); else, also lists of matrices of PCA results are produced and stored.} 
  
}

\details{
In order to respect the ordinal scale of the data, principal components analysis is not applied to data matrix \code{H} itself, but to newly constructed variables by assigning numerical values -- the quantifications -- to the categories via penalized, optimal scaling/scoring. 
The calculation is done by alternately cycling through data scoring and PCA until convergence. 

The penalty parameter controls the amount of shrinkage: For \code{lambda = 0}, purely nonlinear PCA via standard, optimal scaling is obtained. As \code{lambda} becomes very large, the quantifications are shrunken towars linearity, i.e., usual PCA is applied to levels 1,2,... ignoring the ordinal scale level of the variables. 

Note that optimization starts with the first component of \code{lambda}. Thus, if \code{lambda} is not in decreasing order, the vector will be sorted internally and so will be corresponding results.

In case of cross-validation, for each \code{lambda} the proportion of variance accounted for (VAF) is given for both the training and test data (see below).
}

\value{
A List with components:
  \item{qs}{a list of quantifications, if \code{lambda} is specified as a single value. Otherwise, a list of matrices,
  each column corresponding to a certain \code{lambda} value.}
  \item{Q}{data matrix after scaling, if \code{lambda} is scalar. Otherwise, a list of matrices with each list entry corresponding to a certain \code{lambda} value.}
  \item{X}{matrix of factor values resulting from \code{\link{prcomp}}, if \code{lambda} is scalar. Otherwise, list of matrices.}
  \item{A}{loadings matrix as a result from \code{\link{prcomp}}, if \code{lambda} is scalar. 
  Otherwise, list of matrices.}
  \item{iter}{number of iterations used.}
  \item{pca}{object of class \code{"prcomp"} returned by \code{\link{prcomp}}.}
  \item{trace}{vector of VAF values in each iteration, if \code{lambda} is specified as a single value. Otherwise, a list of vectors, each entry corresponding to a certain \code{lambda} value.}
  \item{VAFtrain}{matrix with columns corresponding to \code{lambda} and rows corresponding to the folds \code{k}.
  Contains corresponding proportions of variance accounted for (VAF) on the training data within cross-validation. VAF here is defined in terms of the proportion of variance explained by the first \code{p} PCs.}
  \item{VAFtest}{VAF matrix for the test data within cross-validation.} 

If cross-validation is desired, the pca results are stored in a list called \code{fit} with each list entry corresponding to a certain fold. Within such a list entry, all sub entries can be accessed as described above. 
However, VAF values are stored in \code{VAFtrain} or \code{VAFtest} and can be accessed directly. 
  
}
 
\references{
Hoshiyar, A. (2020). \emph{Analyzing Likert-type data using penalized non-linear principal components analysis}, in: Proceedings of the 35th International Workshop on Statistical Modelling, Vol. I, 337-340.

Hoshiyar, A., H.A.L. Kiers, and J. Gertheiss (2021). \emph{Penalized non-linear principal components analysis for ordinal variables with an application to international classification of functioning core sets}, British Journal of Mathematical and Statistical Psychology, 76, 353-371.

Linting, M., J.J. Meulmann, A.J. von der Kooji, and P.J.F. Groenen (2007). 
\emph{Nonlinear principal components analysis: Introduction and application}, 
Psychological Methods, 12, 336-358.
}

\author{Aisouda Hoshiyar, Jan Gertheiss}

\seealso{
\code{\link{prcomp}}  
}

\examples{
\dontrun{
## load ICF data 
data(ICFCoreSetCWP)

# adequate coding to get levels 1,..., max 
H <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1, 50), rep(5, 16), 1),
                                    nrow(ICFCoreSetCWP), 67,
                                    byrow = TRUE)
xnames <- colnames(H)                                    
                                    
# nonlinear PCA
icf_pca1 <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0.0001), maxit = 1000, 
                   Ks = c(rep(5, 50), rep(9, 16), 5), 
                   constr = c(rep(TRUE, 50), rep(FALSE, 16), TRUE))

# estimated quantifications 
icf_pca1$qs[[55]]

plot(1:9, icf_pca1$qs[[55]][,1], type="b", 
xlab="category", ylab="quantification", col=1, main=xnames[55], 
ylim=range(c(icf_pca1$qs[[55]][,1],icf_pca1$qs[[55]][,2],icf_pca1$qs[[55]][,3])))
lines(icf_pca1$qs[[55]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd=2)
lines(icf_pca1$qs[[55]][,3], type = "b", col = 3, lty = 3, pch = 3, lwd=2)

# compare VAF 
icf_pca2 <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0.0001), maxit = 1000, 
                   Ks = c(rep(5, 50), rep(9, 16), 5), 
                   constr = c(rep(TRUE, 50), rep(FALSE, 16), TRUE),
                   CV = TRUE, k = 5)
icf_pca2$VAFtest

## load ehd data 
require(psy)
data(ehd)

# recoding to get levels 1,..., max 
H <- ehd + 1

# nonlinear PCA
ehd1 <- ordPCA(H, p = 5, lambda = 0.5, maxit = 100,
               constr = rep(TRUE,ncol(H)),
               CV = FALSE)

# resulting PCA on the scaled variables
summary(ehd1$pca)

# plot quantifications
oldpar <- par(mfrow = c(4,5))
for(j in 1:length(ehd1$qs))
  plot(1:5, ehd1$qs[[j]], type = "b", xlab = "level", ylab = "quantification",
  main = colnames(H)[j])
par(oldpar)

# include cross-validation
lambda <- 10^seq(4,-4, by = -0.1)
set.seed(456)
cvResult <- ordPCA(H, p = 5, lambda = lambda, maxit = 100,
                    constr = rep(TRUE,ncol(H)),
                    CV = TRUE, k = 5, CVfit = FALSE)
# optimal lambda                    
lambda[which.max(apply(cvResult$VAFtest,2,mean))]
}
}
 