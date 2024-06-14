\name{EstHyper}
\alias{EstHyper}
\title{Estimate the Hyper-parameter Using Generalized Least Squares}
\description{Estimate the hyper-parameter by calling the \code{gls} function. The errors are allowed to be correlated and/or have unequal variances.}
\usage{
EstHyper(y, D, init.val)
}
\arguments{
  \item{y}{a vector of z-values.}

  \item{D}{a distance matrix defined based on the prior structure. Diagonal have to be zeros.}
  
    \item{init.val}{initial values for the transformed hyper-parameter. Default is 0.}
    }
\value{
a vector of estimated hyper-parameter values plus log likelihood.
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



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Multiple testing }
\keyword{ False discovery rate}
\keyword{ Genetics } 
\keyword{ Metagenomics }
\keyword{ Genomics }% __ONLY ONE__ keyword per line
