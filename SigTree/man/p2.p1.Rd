\name{p2.p1}
\alias{p2.p1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to convert two-tailed p-values to one-tailed, for use by other SigTree functions.
}
\description{
\code{p2.p1} takes vectors \code{p} (representing two-sided p-values of null: Mean2=Mean1) and \code{diff} 
 (representing Mean2-Mean1) and computes one-tailed p-values.  One-tailed p-values are used by
 other SigTree functions, primarily \code{plotSigTree}, \code{export.figtree}, and \code{export.inherit}.
}
\usage{
p2.p1(p,diff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
vector of two-tailed p-values, corresponding to a test of null: Mean2=Mean1.
}
  \item{diff}{
vector of differences Mean2-Mean1, or a vector of the signs of the Mean2-Mean1 differences.
}
}
\details{
This function has application when multiple tests (as at multiple OTUs) of some intervention have been performed,
such as comparing the mean of a treatment 2 with the mean of a treatment 1.  The resulting two-sided p-values
can be converted to one-sided p-values, so that the tools of the SigTree package are applicable.

To access the tutorial document for this package (including this function), type in R: \code{vignette("SigTree")}

}
\value{
This function produces a vector of one-sided p-values, corresponding to a test of null: Mean2=Mean1 vs. alternative: Mean2>Mean1.
}
\references{

Stevens J.R., Jones T.R., Lefevre M., Ganesan B., and Weimer B.C. (2017) "SigTree: A Microbial Community Analysis Tool to Identify and Visualize Significantly Responsive Branches in a Phylogenetic Tree."  Computational and Structural Biotechnology Journal 15:372-378.

Jones T.R. (2012) "SigTree: An Automated Meta-Analytic Approach to Find Significant Branches in a Phylogenetic Tree" (2012). MS Thesis, Utah State University, Department of Mathematics and Statistics.  http://digitalcommons.usu.edu/etd/1314

}
\author{
John R. Stevens and Todd R. Jones
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
### To access the tutorial document for this package, type in R (not run here): 
# vignette('SigTree')

## Assume 10 OTUs are measured in each of
## 20 subjects receiving treatment 2, and
## 15 subjects receiving treatment 1.
## For each OTU, test null: Mean2=Mean1 
## using a Wilcoxon Rank Sum test.
## Simulate data, and obtain p-values and differences
set.seed(1234)
library(MASS)
X2 <- mvrnorm(n=20, mu=runif(10), Sigma=diag(10))
X1 <- mvrnorm(n=15, mu=runif(10), Sigma=diag(10))
p1.orig <- p2 <- diff <- rep(NA,10)
for(i in 1:10)
 {
    p1.orig[i] <- wilcox.test(X1[,i],X2[,i],
       alt='less', exact=FALSE)$p.value
    p2[i] <- wilcox.test(X1[,i],X2[,i],
       exact=FALSE)$p.value
    diff[i] <- mean(X2[,i]) - mean(X1[,i])
  }

## Convert two-sided p-values to one-sided
p1.new <- p2.p1(p2,diff)

## Compare with 'original' one-sided p-values
plot(p1.new,p1.orig); abline(0,1)

}


\keyword{ tree }
\keyword{ dplot }
