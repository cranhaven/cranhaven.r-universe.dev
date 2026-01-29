\name{GenoScan.Region}
\alias{GenoScan.Region}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scan the association between an quantitative/dichotomous outcome variable and a region by score type statistics allowing for multiple functional annotation scores.
}
\description{
Once the preliminary work is done by "GenoScan.prelim()", this function scan a target region. This function is often used for candidate region analyses.
}
\usage{
GenoScan.Region(result.prelim,G,pos,Gsub.id=NULL,Z=NULL,MAF.weights='beta',
test='combined',window.size=c(5000,10000,15000,20000,25000,50000),MAF.threshold=1,
impute.method='fixed')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result.prelim}{
The output of function "GenoScan.prelim()"
}
  \item{G}{
Genetic variants in the target region, an n*p matrix where n is the subject ID and p is the total number of genetic variants.
}
  \item{pos}{
The positions of genetic variants, an p dimensional vector. Each position corresponds to a column in the genotype matrix.
}
  \item{Gsub.id}{
The subject id corresponding to the genotype matrix, an n dimensional vector. Each ID corresponds to a row in the genotype matrix. This is used to match phenotype with genotype. The default is NULL, where the matched phenotype and genotype matrices are assumed.
}
  \item{Z}{
Weight matrix for functional annotations, an p*q matrix where p is the total number of genetic variables and q is the number of weights. This is used to incorperate functional annotations. The default is NULL, where minor allele frequency weighted (see MAF.weights) dispersion and/or burden tests are applied.
}
  \item{MAF.weights}{
Minor allele frequency based weight. Can be 'beta' to up-weight rare variants or 'equal' for a flat weight. The default is 'beta'.
}
  \item{test}{
Can be 'dispersion', 'burden' or 'combined'. The test is 'combined', both dispersion and burden tests are applied. The default is 'combined'.
}
  \item{window.size}{
Candidate window sizes in base pairs. The default is c(5000,10000,15000,20000,25000,50000). Note that extemely small window size (e.g. 1) requires large sample size.
}
  \item{MAF.threshold}{
Threshold for minor allele frequency. Variants above MAF.threshold are ignored. The default is 1.
}
  \item{impute.method}{
Choose the imputation method when there is missing genotype. Can be "random", "fixed" or "bestguess". Given the estimated allele frequency, "random" simulates the genotype from binomial distribution; "fixed" uses the genotype expectation; "bestguess" uses the genotype with highest probability.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{n.marker}{
Number of tested variants in the window (heterozygous variants below MAF threshold).
}
  \item{window.summary}{
Results for all windows. Each row presents a window, including chromosome number, start position, end position,dispersion p-value(s), burden p-values(s).
}
  \item{M}{
Estimated number of effective tests.
}
  \item{threshold}{
Estimated threshold, 0.05/M. This threshold is for windows tested in this particular region.
}
  \item{p.value}{
P-value of entire region.
}
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
## GenoScan.prelim does the preliminary data management.
# Input: Y, X (covariates)
## GenoScan.Region scans a region.
# Input: G (genetic variants), pos (position) Z (weights) and result of GenoScan.prelim

library(GenoScan)

# Load data example
# Y: outcomes, n by 1 matrix where n is the total number of observations
# X: covariates, n by d matrix
# G: genotype matrix, n by p matrix where n is the total number of subjects
# pos: positions of genetic variants, p dimention vector
# Z: functional annotation matrix, p by q matrix

data(GenoScan.example)
Y<-GenoScan.example$Y;X<-GenoScan.example$X
G<-GenoScan.example$G;pos<-GenoScan.example$pos
Z<-GenoScan.example$Z

# Preliminary data management
result.prelim<-GenoScan.prelim(Y,X=X,out_type="C",B=5000)

# Scan the region with functional annotations defined in Z
result<-GenoScan.Region(result.prelim,G,pos,Z=Z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{region}
\keyword{scan}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
