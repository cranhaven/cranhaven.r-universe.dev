\name{Ztransform}
\alias{Ztransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transform P-values to Z-values
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Transform the p-values to z-values. Both two-sided and one-sided transformations are implemented.
}
\usage{
Ztransform(p.value, e.sign, eff.sign = TRUE, tol = 1e-15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p.value}{
%%     ~~Describe \code{p.value} here~~
a vector of p-values
}
  \item{e.sign}{
%%     ~~Describe \code{e.sign} here~~
a vector of signs of the effects, taken on value -1 and 1. (In effects when 'eff.sign = TRUE')
}
  \item{eff.sign}{
%%     ~~Describe \code{eff.sign} here~~
a logical value indicating whether the direction/sign of the effect should be considered
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
a numeric value at which the p-value (both ends) will truncate. 
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a vector of z-values
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
