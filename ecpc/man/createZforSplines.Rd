\name{createZforSplines}
\alias{createZforSplines}
\title{
%%  ~~function to do ... ~~
Create a co-data matrix Z of splines 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a co-data matrix Z of spline basis functions for a continuous co-data variable.
}
\usage{
createZforSplines(values, G=10, bdeg=3, index=NULL, p=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
  A vector with continuous co-data values.
}
\item{G}{
  Number of B-splines.
}
\item{bdeg}{
  Degree of the B-spline basis functions.
}
\item{index}{
  Index of the covariates corresponding to the values supplied. Useful when 
  part of the co-data is missing/seperated and only the non-missing/remaining 
  part should be modelled with splines.
}
\item{p}{
Number of covariates in total. If not given, taken as length of `values'. But in cases where some covariates are left unpenalised, the total number of covariates may be larger.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A (pxG)-dimensional co-data matrix.
}

\references{
See for an introduction on p-splines:

Eilers, P. H., & Marx, B. D. (2021). Practical Smoothing: The Joys of P-splines. Cambridge University Press.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
Use \code{\link{createS}} to create a difference penalty for p-splines.
}
\examples{
#create co-data with random normally distributed values for 100 covariates
values <- rnorm(n=100)
#suppose that there is one additional covariate (the first) that should not be modelled
ind <- 2:101
p<-101
Z <- createZforSplines(values=values,G=10,index=ind,p=p)
}

