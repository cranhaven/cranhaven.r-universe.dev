\name{create.MK}
\alias{create.MK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequential knockoff generator for genetic data
}
\description{
Generate single/multiple knockoffs for genetic variants for customized analysis.
}
\usage{
create.MK(X,pos,M=5,corr_max=0.75,maxN.neighbor=Inf,maxBP.neighbor=100000,
n.AL=floor(10*nrow(X)^(1/3)*log(nrow(X))),thres.ultrarare=25,
R2.thres=1,method='shrinkage',bigmemory=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A n*p genotype matrix, where n is the sample size and p is the number of genetic variants.
}
  \item{pos}{
A vector of length p. Location of the p genetic variants.
}
  \item{M}{
Number of knockoffs per variant. The default is 5.
}
  \item{corr_max}{
The correlation threshold for hierarchical clustering, such that variants from two different clusters do not have a correlation greater than corr_max. The hierarchical clustering step is a practical strategy to improve the power for tightly linked variants. The default is 0.75.
}
  \item{maxN.neighbor}{
The maximum number of neighoring variables used to generate knockoffs. The default is Inf. Smaller number will inprove the computational efficiency, but the knockoffs will be less accurate.
}
  \item{maxBP.neighbor}{
The size of neighboring region (in base pairs) used to generate knockoffs. The default is 100000.
}
  \item{n.AL}{
The sample size for the algorithmic leveraging. The default is 10*n^(1/3)*log(n)).
}
  \item{thres.ultrarare}{
The minor allele count threshold that defines ultrarare variants. The knockoff generation for variants with minor allele counts below the threshold will be based on permutaton. The default is 25.
}
  \item{R2.thres}{
The maximum R2 allowed in the auto-regressive model. More liberal values (<1) lead to higher power for tightly linked variants, but the knockoffs will be less accurate.  The default is 1.
}
  \item{method}{
The method for subsampling. The default is "shrinkage", corresponding to "shrinkage algorithmic leveraging".
}
  \item{bigmemory}{
Whether "bigmemory" operation is applied. Default is TRUE.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{X_k}{
An M dimentions list, where each dimention is an n*p matrix as a knockoff copy of original data.
}
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{

library(KnockoffScreen)

# load example vcf file from package "seqminer"
vcf.filename = system.file("vcf/1000g.phase1.20110521.CFH.var.anno.vcf.gz", package = "seqminer")

## this is how the actual genotype matrix from package "seqminer" looks like
example.G <- t(readVCFToMatrixByRange(vcf.filename, "1:196621007-196716634",annoType='')[[1]])

# filter out constant variants
s<-apply(example.G,2,sd)
example.G<-example.G[,s!=0]
pos<-as.numeric(gsub("^.*:","",colnames(example.G)))

# generate multiple knockoffs
example.G_k<-create.MK(example.G,pos,M=5,corr_max=0.75)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{KnockoffGenerator}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
