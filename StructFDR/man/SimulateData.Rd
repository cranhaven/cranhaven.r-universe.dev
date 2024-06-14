\name{SimulateData}
\alias{SimulateData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Data Simulation Function to Study the Performance of TreeFDR.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
We include five scenarios ('S1-S5'). S1-S3 are phylogeny-informative/clade-consistent scenarios while S4-S5 are
phylogeny-noninformative/clade-inconsistent scenarios. In 'S1', we simulate two large clusters of differentially
abundant OTUs. The fold changes (effect sizes) for OTUs from the same cluster are the same. In 'S2', we weaken the
assumption, and generate variable fold changes for OTUs from the same cluster. In 'S3', we simulate many small
clusters (10) of differentially abundant OTUs with the same effect sizes. In 'S4', we still simulate two large 
clusters of differentially abundant OTUs but we allow opposite effects for OTUs from the same cluster. This violates
the assumption of similar effects for closely related OTUs. In 'S5', we pick 10\% random OTUs without respect to 
the underlying phyologeny. 
}

\usage{
SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, depth = 10000,
		     p.est, theta, scene, signal.strength = 4, otu.no.min = 40, 
		     otu.no.max = 80, zero.pct = 0, balanced = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nCases, nControls}{
%%     ~~Describe \code{X} here~~
the number of case and control samples.
}
  \item{nOTU}{
%%     ~~Describe \code{Y} here~~
the number of OTUs simulated.
}
  \item{nCluster}{
%%     ~~Describe \code{X} here~~
the number of clusters to be clustered. 
}
  \item{depth}{
%%     ~~Describe \code{tree} here~~
mean library sizes/sequencing depth. The library sizes are simulated from negative binomial distribution of size=25.
}
  \item{p.est, theta}{
%%     ~~Describe \code{test.func} here~~
the parameters (proportion vector and dispersion parameter) of the Dirichlet distribution.
}
  \item{scene}{
%%     ~~Describe \code{perm.func} here~~
simulation scenarios. 'S1', 'S2', 'S3', 'S4', 'S5' denote five scenarios, respectively. 
}
  \item{signal.strength}{
%%     ~~Describe \code{eff.sign} here~~
the strength of signal (related to the mean and sd of the log fold change). 
}
  \item{otu.no.min, otu.no.max}{
%%     ~~Describe \code{B} here~~
the minimum and maximum numbers of differentially abundant OTUs. Defaults are 40 and 80.
}
  \item{zero.pct}{
%%     ~~Describe \code{B} here~~
the percentage of non-differential OTUs within the cluster/clade. Applicable to 'S1' and 'S2'
}
  \item{balanced}{
%%     ~~Describe \code{B} here~~
a logical value indicating whether the fold changes should be multiplied to cases samples (FALSE, increase/decrease in cases, 
no change for controls) only or to both case and control samples (TRUE, increase in case and control samples). Balanced design will
have similar power for all OTUs.
}

}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{y}{a vector of group membership. 0 = control, 1 = case.}
\item{X}{a matrix of normalized OTU counts. row: OTUs; column: samples.}
\item{beta.true}{a vector of the true log fold changes for all OTUs. 0s for non-differential OTUs.}
\item{D}{a matrix of the cophenetic distances among the simulated OTUs.}
\item{tree}{a simulated coalescent tree of the 'phylo' class.}
\item{clustering}{a vector of cluster memberships for the OTUs based on PAM.}
}

\references{
  Xiao, J., Cao, H., Chen, J. (2017). False discovery rate control incorporating phylogenetic tree increases detection power in microbiome-wide multiple testing. Bioinformatics, 33(18), 2873-2881.
}
\author{
%%  ~~who you are~~
Jun Chen
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Generate data set for different scenarios S1-S5
require(StructFDR)
data(throat.parameter)
# Scene 1
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S1', signal.strength = 4)
# Scene 2
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S2', signal.strength = 4)
# Scene 3
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 100, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S3', signal.strength = 4)
# Scene 4
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S4', signal.strength = 2)
# Scene 5
data.obj <- SimulateData(nCases = 50, nControls = 50, nOTU = 400, nCluster = 20, 
		depth = 10000, p.est = throat.parameter$p.est, theta = throat.parameter$theta,
        scene = 'S5', signal.strength = 4)       
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Multiple testing }
\keyword{ False discovery rate}
\keyword{ Genetics } 
\keyword{ Metagenomics }
\keyword{ Genomics }% __ONLY ONE__ keyword per line

