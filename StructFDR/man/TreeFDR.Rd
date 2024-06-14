\name{TreeFDR}
\alias{TreeFDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
False Discovery Rate (FDR) Control Integrating Prior Tree Structure for Microbiome Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The procedure is based on an empirical Bayes hierarchical model, where a structure-based prior distribution is designed to utilize the phylogenetic tree. A moderated
statistic based on posterior mean is used for permutation-based FDR control. By borrowing information from neighboring bacterial species, it is able to 
improve the statistical power of detecting associated  bacterial species while controlling the FDR at desired levels. 
}
\usage{
TreeFDR(X, Y, tree, test.func, perm.func, eff.sign = TRUE, B = 20, q.cutoff = 0.5,
       alpha = 1, adaptive = c('Fisher', 'Overlap'), alt.FDR = c('BH', 'Permutation'), 
        ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
a data matrix, rows are the features and columns are the samples.
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
a vector of the phenotypic values, where association tests are being assessed
}
  \item{tree}{
%%     ~~Describe \code{tree} here~~
an object of "phylo" class 
}
  \item{test.func}{
%%     ~~Describe \code{test.func} here~~
a function that performs the actual tests. It takes \code{X}, \code{Y} and ... as the inputs, and returns a list with two slots \code{p.value} and \code{e.sign}, which are vectors of p-values 
and signs of the effects.
}
  \item{perm.func}{
%%     ~~Describe \code{perm.func} here~~
a function that performs the permutation tests. It takes \code{X}, \code{Y} and ... as the inputs, and returns a list with two slots \code{X} and \code{Y}, which contain the permuted data.
}
  \item{eff.sign}{
%%     ~~Describe \code{eff.sign} here~~
a logical value indicating whether the direction of the effects should be considered. If it is true (default), negative and positive effects provide conflicting 
information. 
}
  \item{B}{
%%     ~~Describe \code{B} here~~
the number of permutations.  The default is 20. If computation time is not a big concern, \code{B=100} is suggested to achieve excellent reproducibility between different runs.
}
  \item{q.cutoff}{
%%     ~~Describe \code{c.cutoff} here~~
the quantile cutoff to determine the feature sets to estimate the number of false positives under the null. This cutoff is to protect the signal part of the distributions. The default is 0.5.
}
  \item{alpha}{
%%     ~~Describe \code{c.cutoff} here~~
the exponent applied to the distance matrix. Large values have more smoothing effects for closely related species. The default is 1. If the underlying structure assumption is considered to be very strong, robustness
can be improved by decreasing the value to 0.5.
}
  \item{adaptive}{
%%     ~~Describe \code{c.cutoff} here~~
the proposed procedure is most powerful when the signal is clustered on the tree.  When this assumption is seriously violated, it loses power.  We provide two heuristic adaptive approaches
to compensate the power loss in such situations. 'Fisher' approach compares the number of hits by our method to the alternative FDR approach at an FDR of 20\% and uses the alternative FDR approach 
if the number of hits is significantly less based on Fisher's exact test; 'Overlap' method selects the alternative approach when it fails to identify half of the hits from the alternative approach 
at an FDR of 20\%. The default is 'Fisher'.
}
  \item{alt.FDR}{
%%     ~~Describe \code{c.cutoff} here~~
the alternative FDR control used when the proposed approach is powerless. The default is 'BH' procedure and another option is the permutation-based FDR control ('Permutation')
}
  \item{...}{
%%     ~~Describe \code{c.cutoff} here~~
further arguments such as covariates to be passed to \code{test.func}
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{p.adj }{TreeFDR adjusted p-values.}
\item{p.unadj }{raw p-values.}
\item{z.adj }{moderated z-values. The scale may be different from the raw z-values.}
\item{z.unadj }{raw z-values.}
\item{k, rho }{the estimates of the hyperparameters.  The values indicate the informativeness of the prior structure.}

}

\references{
%% ~put references to the literature/web site here ~
Xiao, J., Cao, H., Chen, J. (2017). False discovery rate control incorporating phylogenetic tree increases detection power in microbiome-wide multiple testing. Bioinformatics, 33(18), 2873-2881.
}
\author{
%%  ~~who you are~~
Jun Chen
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{StructFDR}}
}
\examples{
require(ape) 
require(nlme)
require(cluster)
require(StructFDR)

# Generate a caelescence tree and partition into 10 clusters
set.seed(1234)
n <- 20
p <- 200
tree <- rcoal(p)
D <- cophenetic(tree)
clustering <- pam(D, k=10)$clustering

# Simulate case-control data, assuming cluster 2 is differential  
X.control <- matrix(rnorm(n*p), p, n)
X.case <- matrix(rnorm(n*p), p, n)
eff.size <- rnorm(sum(clustering == 2), 0.5, 0.2)     
X.case[clustering == 2, ] <- X.case[clustering == 2, ] + eff.size
X <- cbind(X.control, X.case)
Y <- gl(2, n) 

# Define testing and permutation function
test.func <- function (X, Y) {
	obj <- apply(X, 1, function(x) {
				ttest.obj <- t.test(x ~ Y)
				c(ttest.obj$p.value, sign(ttest.obj$statistic))
			})
    return(list(p.value=obj[1, ], e.sign=obj[2, ]))
}

perm.func <- function (X, Y) {
	return(list(X=X, Y=sample(Y)))
}

# Call TreeFDR
tree.fdr.obj <- TreeFDR(X, Y, tree, test.func, perm.func)

# Compare TreeFDR and BH
tree.fdr.obj$p.adj
tree.fdr.obj$p.adj[clustering == 2]
BH.p.adj <- p.adjust(tree.fdr.obj$p.unadj, 'fdr')
BH.p.adj[clustering == 2]

# Adjusted statistics vs clustering
par(mfrow=c(1, 2))
plot(clustering, tree.fdr.obj$z.unadj)
plot(clustering, tree.fdr.obj$z.adj)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Multiple testing }
\keyword{ False discovery rate}
\keyword{ Genetics } 
\keyword{ Metagenomics }
\keyword{ Genomics }% __ONLY ONE__ keyword per line
