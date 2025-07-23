\name{createS}
\alias{createS}
\title{
%%  ~~function to do ... ~~
Create a generalised penalty matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a generalised penalty matrix which can be used as hypershrinkage for 
co-data matrix Z. 
}
\usage{
createS(orderPen=2, G=10, categorical=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orderPen}{
  The order of the difference penalty. If 0, then a ridge penalty matrix is returned.
}
\item{G}{
Number of co-data variables to be penalised.
}
\item{categorical}{
If TRUE, a block correlation matrix is returned.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A (GxG)-dimensional penalty matrix.
}

\references{
See for an introduction on p-splines and difference penalties:

Eilers, P. H., & Marx, B. D. (2021). Practical Smoothing: The Joys of P-splines. Cambridge University Press.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
A difference penalty may be applied for p-spline basis functions created with \code{\link{createZforSplines}} or for categorical co-data created with \code{\link{createZforGroupset}}.
}
\examples{
S1 <- createS(orderPen=2,G=10) #second difference penalty matrix
S2 <- createS(orderPen=0,G=10) #zeroth order defined as ridge penalty matrix
S3 <- createS(G=10,categorical=TRUE) #difference penalty for unordered categorical groups
}

