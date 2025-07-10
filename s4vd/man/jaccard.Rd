\name{jaccardmat}
\alias{jaccardmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
jaccard matrix
}
\description{
The function calculates the pairwise jaccard coefficients between the biclusters of two biclustering results 
}
\usage{
jaccardmat(res1,res2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res1}{
A biclustering result as an object of class Biclust  
}
  \item{res2}{
A biclustering result as an object of class Biclust
}
}
\details{
The result is matrix of pairwise jaccard coefficents between the biclusters of res1 and res2. 
}
%\value{
%}
%\references{
%
%kommt noch
%}
\author{
Martin Sill \
\email{m.sill@dkfz.de}
}
%\note{
%kommt noch
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
jaccardind
}
\examples{
\donttest{
#lung cancer data set   Bhattacharjee et al. 2001
data(lung200)
set.seed(12)
res1 <- biclust(lung200,method=BCs4vd(),pcerv=.5,pceru=0.01,ss.thr=c(0.6,0.65)
,start.iter=3,size=0.632,cols.nc=TRUE,steps=100,pointwise=TRUE
,merr=0.0001,iter=100,nbiclust=10,col.overlap=FALSE)
res2 <- biclust(lung200,method=BCPlaid())
jaccardmat(res1,res2)
}
}