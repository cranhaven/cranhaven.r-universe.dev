\name{ordGene}
\alias{ordGene}
\title{Testing for differentially expressed genes}
\description{This function can be used to test for genes that are differentially
expressed between levels of an ordinal factor, such as dose levels or ordinal
phenotypes.}

\usage{
ordGene(xpr, lvs, type = c("RLRT", "LRT"), nsim = 1e6,
null.sample=NULL, ...)
}
\arguments{
  \item{xpr}{a matrix or data frame of gene expression data with Probe IDs as
  row names.}
  \item{lvs}{a numeric vector containing the factor levels (e.g., dose levels)
  corresponding to the columns of \code{xpr}.}
  \item{type}{the type of test to carry out: likelihood ratio ("LRT") or
  restricted likelihood ratio ("RLRT").}
  \item{nsim}{number of values to simulate from the null distribution.}
  \item{null.sample}{a vector containing values already simulated from the null 
  distribution (overrides \code{nsim})}
  \item{...}{additional arguments to \code{\link{LRTSim}} and
  \code{\link{RLRTSim}}, respectively.}
}
\details{
For each gene in the dataset, \code{\link{ordAOV}} is applied to test for
differences between levels given in \code{lvs}. See \code{\link{ordAOV}} for
further information on the testing procedure. Simulation studies by Gertheiss (2014)
suggest that a restricted likelihood test (RLRT) should rather be used than
a likelihood ratio test (LRT).

In addition to (R)LRT, results of usual one-way ANOVA (not taking the factor's
ordinal scale level into account) and a t-test assuming a linear trend across
factor levels are reported. Note that the t-test does not assume linearity in the
doses (such as 0, 0.5, 2.0, 5.0, ...), if given, but in the levels, i.e., 1, 2, 3, etc.
}

\value{
A matrix containing the raw p-values for each gene (rows) when using (R)LRT,
ANOVA or a t-test (columns).
}

\references{
Crainiceanu, C. and D. Ruppert (2004). \emph{Likelihood ratio tests in linear
mixed models with one variance component}, Journal of the Royal Statistical
Society B, 66, 165-185.

Gertheiss, J. (2014). \emph{ANOVA for factors with ordered levels}, Journal of
Agricultural, Biological and Environmental Statistics, 19, 258-277.

Gertheiss, J. and F. Oehrlein (2011). \emph{Testing relevance and linearity of
ordinal predictors}, Electronic Journal of Statistics, 5, 1935-1959.

Sweeney, E., C. Crainiceanu, and J. Gertheiss (2015). \emph{Testing
differentially expressed genes in dose-response studies and with ordinal
phenotypes}, Statistical Applications in Genetics and Molecular Biology, 15, 213-235.
}

\author{Jan Gertheiss}


\seealso{\code{\link{ordAOV}}}

\examples{
\dontrun{
# generate toy gene expression data
set.seed(321) 
ni <- 5
n <- sum(5*ni)
xpr <- matrix(NA, ncol = n, nrow = 100)
mu_lin <- 3:7  
mu_sq2 <- (-2:2)^2 * 0.5 + 3   
a <- seq(0.75, 1.25, length.out = 10)

for(i in 1:10){ 
  xpr[i,] <- a[i] * rep(mu_lin, each = ni) + rnorm(n)
  xpr[i+10,] <- a[i] * rep(mu_sq2, each = ni) + rnorm(n) 
} 
for(i in 21:100) xpr[i,] <- 3 + rnorm(n)

dose <- rep(c(0,0.01,0.05,0.2,1.5), each = ni)

# continuous representation
oldpar <- par(mfrow = c(2,2))
plot(dose, xpr[4,], col = as.factor(dose), lwd = 2, ylab = "expression", main = "gene 4") 
lines(sort(unique(dose)), mu_lin * a[4], lty = 1, col = 1) 
plot(dose, xpr[14,], col = as.factor(dose), lwd = 2, ylab = "expression", main = "gene 14") 
lines(sort(unique(dose)), mu_sq2 * a[4], lty = 1, col = 1) 

# dose on ordinal scale
plot(1:length(sort(unique(dose))), ylim = range(xpr[4,]), pch = "", ylab = "expression", 
     xlab = "levels", xaxt="n")
axis(1, at = 1:length(sort(unique(dose))) ) 
points(as.factor(dose), xpr[4,], col=as.factor(dose), lwd = 2) 
lines(1:length(sort(unique(dose))), mu_lin * a[4], lty = 1)
plot(1:length(sort(unique(dose))), ylim = range(xpr[14,]), pch = "", ylab = "expression", 
     xlab = "levels", xaxt="n")
axis(1, at = 1:length(sort(unique(dose))) ) 
points(as.factor(dose), xpr[14,], col=as.factor(dose), lwd = 2) 
lines(1:length(sort(unique(dose))), mu_sq2 * a[4], lty = 1)
par(oldpar)

# calculate p-values
library(ordPens)
pvals <- ordGene(xpr = xpr, lvs = dose, nsim = 1e6)

# compare distribution of (small) p-values
plot(ecdf(pvals[,1]), xlim = c(0,0.05), ylim = c(0, 0.25),
     main = "", xlab = "p-value", ylab = "F(p-value)")
plot(ecdf(pvals[,2]), xlim = c(0, 0.05), add = TRUE, col = 2)
plot(ecdf(pvals[,3]), xlim = c(0, 0.05), add = TRUE, col = 3)
legend('topleft', colnames(pvals), col = 1:3, lwd = 2, lty = 1) 
}
}


\keyword{anova}
\keyword{gene expression}
