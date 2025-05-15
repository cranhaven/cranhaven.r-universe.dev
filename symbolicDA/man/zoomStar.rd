\name{zoomStar}
\alias{zoomStar}
\title{zoom star chart for symbolic data}
\description{plot in a form of zoom star chart for symbolic object described by interval-valued, multivalued and modal variables}
\usage{
zoomStar(table.Symbolic, j, variableSelection=NULL, offset=0.2, 
firstTick=0.2, labelCex=.8, labelOffset=.7, tickLength=.3, histWidth=0.04, 
histHeight=2, rotateLabels=TRUE, variableCex=NULL)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
\item{j}{symbolic object number in symbolic data table used to create the chart}
\item{variableSelection}{numbers of symbolic variables describing symbolic object used to create the chart, if NULL all variables are used}
\item{offset}{relational offset of chart (margin size)}
\item{firstTick}{place of first tick (relational to lenght of axis)}
\item{labelCex}{labels cex parameter of labels}
\item{labelOffset}{relational offset of labels}
\item{tickLength}{relational length of single tick of axis}
\item{histWidth}{histogram (for modal variables) relational width}
\item{histHeight}{histogram (for modal variables) relational heigth}
\item{rotateLabels}{if TRUE labels are rotated due to rotation of axes}
\item{variableCex}{cex parameter of names of variables}
}
\value{
zoom star chart for selected symbolic object in which each axis represents a symbolic variable. Depending on the type of symbolic variable their implementations are presented as:

a) rectangle - interval range of interval-valued variable), 

b) circles - categories of multinominal (or multinominal with weights) variable from among coloured circles means categories of the variable observed for the selected symbolic object

bar chart - additional chart for multinominal with weights variable in which each bar represents a weight (percentage share) of a category of the variable
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\seealso{
\code{plotInterval} in \code{clusterSim}
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
# Example 1
#data("cars",package="symbolicDA")
#sdt<-cars
#zoomStar(sdt, j=12)

# Example 2
#data("cars",package="symbolicDA")
#sdt<-cars
#variables<-as.matrix(sdt$variables)
#indivN<-as.matrix(sdt$indivN)
#dist<-as.matrix(dist_SDA(sdt))
#classes<-DClust(dist, cl=5, iter=100)
#for(i in 1:max(classes)){
  #getOption("device")()  
  #zoomStar(sdt, .medoid2(dist, classes, i))}
}
\keyword{cluster}
