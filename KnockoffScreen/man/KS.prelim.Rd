\name{KS.prelim}
\alias{KS.prelim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Preliminary data management for KnockoffScreen
}
\description{
This function does the preliminary data management and fit the model under null hypothesis. The output will be passed to the other functions.
}
\usage{
KS.prelim(Y, X=NULL, id=NULL, out_type="C")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The outcome variable, an n*1 matrix where n is the total number of observations
}
  \item{X}{
An n*d covariates matrix where d is the total number of covariates.
}
  \item{id}{
The subject id. This is used to match phenotype with genotype. The default is NULL, where the matched phenotype and genotype matrices are assumed.
}
  \item{out_type}{
Type of outcome variable. Can be either "C" for continuous or "D" for dichotomous. The default is "C".
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  It returns a list that will be passed to function KS.chr().
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
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

# simulated outcomes, covariates and inidividual id.
Y<-as.matrix(rnorm(nrow(example.G),0,1))
X<-as.matrix(rnorm(nrow(example.G),0,1))
id<-rownames(example.G)

# fit null model
result.prelim<-KS.prelim(Y,X=X,id=id,out_type="C")

}

\keyword{preliminary work}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
