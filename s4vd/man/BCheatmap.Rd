\name{BCheatmap}
\alias{BCheatmap}
\alias{OverlapHeat}
%\alias{s4vd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Overlap Heatmap for the visualization of overlapping biclusters
}
\description{ Heatmap function to plot biclustering results. Overlapping biclusters are indicated by colored rectangles.
}
\usage{
BCheatmap(X, res, cexR = 1.5, cexC = 1.25, axisR = FALSE, axisC= TRUE,
heatcols = maPalette(low="blue",mid="white",high="red", k=50),
clustercols = c(1:5), allrows = FALSE, allcolumns = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
the data matrix  
}
  \item{res}{
the biclustering result
}
  \item{cexR}{
relativ font size of the row labels 
}
  \item{cexC}{
relativ font size of the column labels
}
  \item{axisR}{
if TRUE the row labels will be plotted
}
  \item{axisC}{
if TRUE the column labels will be plotted
}
  \item{heatcols}{
a character vector specifing the heatcolors 
}
  \item{clustercols}{
a character vector specifing the colors of the rectangles that indicate the rows and columns that belong to a bicluster
}
  \item{allrows}{
if FALSE only the rows assigned to any bicluster will be plotted
}
  \item{allcolumns}{
if FALSE only the columns assigned to any bicluster will be plotted
}
%\details{
%
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

%\seealso{
%kommt noch
%}
\examples{
\donttest{
#lung cancer data set   Bhattacharjee et al. 2001
data(lung200)
set.seed(12)
res1 <- biclust(lung200,method=BCs4vd(),pcerv=.5,pceru=0.01,ss.thr=c(0.6,0.65)
,start.iter=3,size=0.632,cols.nc=TRUE,steps=100,pointwise=TRUE
,merr=0.0001,iter=100,nbiclust=10,col.overlap=FALSE)
BCheatmap(lung200,res1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
