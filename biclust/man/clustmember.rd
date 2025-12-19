\name{biclustmember}
\alias{clustmember}
\alias{biclustmember}
\alias{bicorder}

\title{Bicluster Membership Graph}
\description{ Draws a membership graph cluster x columns}
\usage{
biclustmember(bicResult, x, mid = T, cl_label = "", which=NA, 
  main = "BiCluster Membership Graph", xlab="Cluster", 
  color=diverge_hcl(101, h = c(0, 130)), ...)

clustmember(res, x, mid = T, cl_label = "", which=NA, 
  main = "Cluster Membership Graph", xlab="Cluster", 
  color=diverge_hcl(101, h = c(0, 130)), ...)

bicorder(bicResult, cols=TRUE, rev=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix}
  \item{bicResult}{BiclustResult object with a bicluster result set.}
  \item{res}{Cluster Result (is converted into a kcca object)}
  \item{mid}{If TRUE, shows the value of the remaining objects inside the cluster value, else shows both aside each other.}
  \item{cl_label}{Ticks of x-axis}
  \item{which}{If specified gives the ploting order of the columns from bottom to top}
  \item{main}{Gives the title of the plot}
  \item{xlab}{Label of x-axis}
  \item{color}{Range of colors for the plot}
  \item{...}{Additional plot options or if neccessary option for as.kcca}
  \item{cols}{If TRUE orders the column by appearance in the bicluster, else orders the rows.}
  \item{rev}{If TRUE reverses the order}
  }
%\details{}
%\value{}
%\references{}

\author{
  Sebastian Kaiser
  \email{sebastian.kaiser@stat.uni-muenchen.de}
  }

\seealso{
\code{\link{bubbleplot}} for simultaneous representation of biclusters,
\code{\link{parallelCoordinates}}for single representation of biclusters as lines of gene or condition profiles,
\code{\link{drawHeatmap}}for Heatmap representation of biclusters and
\code{\link{biclustbarchart}} for a barchart.
  }
\examples{
  set.seed(1)
  x=matrix(rnorm(900),30,30)
  x[1:5,1:5]=rnorm(25,3,0.3)
  x[11:15,11:15]=rnorm(25,-3,0.3)
  x[21:25,21:25]=rnorm(25,6,0.3)
  colnames(x)<-paste("Var.",1:30)
  bics <- biclust(x,BCPlaid(), back.fit = 2, shuffle = 3, fit.model = ~m + a + b,
  iter.startup = 5, iter.layer = 30,  verbose = TRUE)
  
  biclustmember(bics,x)
  
  ord<-bicorder(bics, cols=TRUE, rev=TRUE)
  
  biclustmember(bics,x,which=ord)
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{cluster}
