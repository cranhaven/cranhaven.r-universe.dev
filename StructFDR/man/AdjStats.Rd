\name{AdjStats}
\alias{AdjStats}
\title{Prior Structure-Adjusted Statistic}
\description{Produce the prior structure-adjusted statistic.}
\usage{
AdjStats(y, V, k, mu, fudge=0.005)
}
\arguments{
  \item{y}{a vector or a matrix of unadjusted z-values}

  \item{V}{a correlation matrix defined based on the prior structure}

  \item{k}{a numeric value representing the ratio of variance components}

  \item{mu}{a numeric value of the mean of the prior distribution}
  
  \item{fudge}{a small numeric value added to the diagonal of the correlation matrix to improve stability}

}
\value{a vector or a matrix of adjusted z-values}

\references{
%% ~put references to the literature/web site here ~
  Xiao, J., Cao, H., Chen, J. (2017). False discovery rate control incorporating phylogenetic tree increases detection power in microbiome-wide multiple testing. Bioinformatics, 33(18), 2873-2881.
}
\author{
%%  ~~who you are~~
Jun Chen
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Multiple testing }
\keyword{ False discovery rate}
\keyword{ Genetics } 
\keyword{ Metagenomics }
\keyword{ Genomics }% __ONLY ONE__ keyword per line

