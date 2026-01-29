\name{GenoScan.prelim}
\alias{GenoScan.prelim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The preliminary data management for GenoScan
}
\description{
This function does the preliminary data management and fit the model under null hypothesis. The output will be used in the other GenoScan functions.
}
\usage{
GenoScan.prelim(Y, X=NULL, id=NULL, out_type="C", B=5000)
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
  \item{B}{
Number of resampling replicates. The default is 5000. A larger value leads to more accurate and stable p-value calculation, but requires more computing time.
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  It returns a list used for function GenoScan.Region(), GenoScan.SingleWindow() and GenoScan.VCF.chr().
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
library(GenoScan)

# Load data example
# Y: outcomes, n by 1 matrix where n is the total number of observations
# X: covariates, n by d matrix
# G: genotype matrix, n by p matrix where n is the total number of subjects
# Z: functional annotation matrix, p by q matrix

data(GenoScan.example)
Y<-GenoScan.example$Y;X<-GenoScan.example$X;G<-GenoScan.example$G;Z<-GenoScan.example$Z

# Preliminary data management
result.prelim<-GenoScan.prelim(Y,X=X,out_type="C",B=5000)

}

\keyword{preliminary work}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
