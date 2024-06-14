\name{MicrobiomeSeqTreeFDR}
\alias{MicrobiomeSeqTreeFDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
False Discovery Rate (FDR) Control Integrating Prior Tree Structure for Microbiome Data Based on F-test and Residual Permutation.
}


\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function is a wrapper of \code{TreeFDR} with a pre-defined test based on F-statistic and residual permutation. 
}
\usage{
MicrobiomeSeqTreeFDR (otu.tab, tree, meta.dat, grp.name, adj.name = NULL, 
raw.count = FALSE, B = 100, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{otu.tab}{
%%     ~~Describe \code{X} here~~
a data matrix, rows are the features and columns are the samples.
}
  \item{tree}{
%%     ~~Describe \code{tree} here~~
an object of "phylo" class. The tree of OTUs.
}
  \item{meta.dat}{
%%     ~~Describe \code{Y} here~~
a data frame containing the variables specified by \code{grp.name} and \code{adj.name}.
}
  \item{grp.name}{
%%     ~~Describe \code{Y} here~~
a character string indicating the variable of major interest. Can be categorical or numerical.
}

  \item{adj.name}{
%%     ~~Describe \code{Y} here~~
a character vector indicating the variables to be adjusted. Can be categorical or numerical.
}

  \item{raw.count}{
%%     ~~Describe \code{test.func} here~~
a logical value indicating whether \code{X} are raw counts. If raw counts are supplied, internal normalization/transformation (GMPR/sqrt) will be performed. 
The default is FALSE. The user should be responsible for selecting the appropriate normalization/transformation methods.
}

  \item{B}{
%%     ~~Describe \code{B} here~~
the number of permutations.  The default is 100 since the permutation test is very fast.
}
   \item{...}{
%%     ~~Describe \code{c.cutoff} here~~
further arguments such as covariates to be passed to \code{TreeFDR}
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
  Xiao, J., Cao, H., Chen, J. (2017). False discovery rate control incorporating phylogenetic tree increases detection power in microbiome-wide multiple testing. Bioinformatics, 33(18), 2873-2881.
}

\author{
%%  ~~who you are~~
Jun Chen
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{TreeFDR}}
}
\examples{
require(StructFDR)

# Generate data
data(throat.parameter)
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S2', signal.strength = 4)
meta.dat <- data.frame(group = factor(data.obj$y), sex = sample(gl(2, 50)))
beta.true <- data.obj$beta.true

# Call TreeFDR
tree.fdr.obj <- MicrobiomeSeqTreeFDR(data.obj$X, data.obj$tree, meta.dat, 'group', 'sex', B = 20)
tree.p.adj <- tree.fdr.obj$p.adj

# Empirical power and FDR
(tree.emp.pwr <- sum(tree.p.adj <= 0.05 & beta.true != 0) / sum(beta.true != 0)) 
(tree.emp.fdr <- sum(tree.p.adj <= 0.05 & beta.true == 0) / sum(tree.p.adj <= 0.05)) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Multiple testing }
\keyword{ False discovery rate}
\keyword{ Genetics } 
\keyword{ Metagenomics }
\keyword{ Genomics }% __ONLY ONE__ keyword per line
