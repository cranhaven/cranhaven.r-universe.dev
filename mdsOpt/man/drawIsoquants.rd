\name{drawIsoquants}
\alias{drawIsoquants}
\title{draw series of isoquants}
\usage{
drawIsoquants(x,y=NULL,number=6,steps=NULL)
}
\arguments{
\item{x}{two dimensional point (center)}
\item{y}{optional - second point, used for calculations of step size if \code{steps} is null}
\item{number}{number of isoquants}
\item{steps}{distance between following isoquants starting from x, if length of this arguments is lower than \code{number} argument last item is repeated}
}

\description{
function draw series of isoquants  (a contour line drawn through the set of points at which the same quantity of output is produced while changing the quantities of two or more inputs)
}
\value{ This is a plotting function, thus does not return any value}
\author{
Marek Walesiak \email{marek.walesiak@ue.wroc.pl}, Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, Wroclaw University of Economics and Business, Poland
}
\references{
Walesiak, M., (2016), Visualization of Linear Ordering Results for Metric Data with the Application of Multidimensional Scaling, Ekonometria, 2(52), 9-21. Available at: \doi{10.15611/ekt.2016.2.01}.

Walesiak, M. (2017), The application of multidimensional scaling to measure and assess changes in the level of social cohesion of the Lower Silesia region in the period 2005-2015, Ekonometria, 3(57), 9-25. Available at: \doi{10.15611/ekt.2017.3.01}.

Walesiak, M., Dudek, A. (2017), \emph{Selecting the Optimal Multidimensional Scaling Procedure for Metric Data with R Environment}, STATISTICS IN TRANSITION new series, September, Vol. 18, No. 3, pp. 521-540. 
}
\examples{
#Example 1
library(mdsOpt)
library(smacof)
library(clusterSim)
data(data_lower_silesian)
z<-data.Normalization(data_lower_silesian, type="n1")
d<-dist.GDM(z, method="GDM1")
res <- smacofSym(delta=d,ndim=2,type="interval")
print("Objects configuration", quote=FALSE)
plot(res, plot.type="confplot")
r1<-res$conf[nrow(z),1]
r2<-res$conf[nrow(z),2]
r3<-res$conf[nrow(z)-1,1]
r4<-res$conf[nrow(z)-1,2]
arrows(r1,r2,r3,r4,length=0.1,col="black")
res_up<-as.matrix(dist(res$conf,method="euclidean"))
drawIsoquants(res$conf[nrow(z)-1,],steps=max(res_up)/6)
# or 
# drawIsoquants(res$conf[nrow(z)-1,],steps=c(0.3,0.2),number=8)

#Example 2
library(mdsOpt)
library(smacof)
library(clusterSim)
data(data_lower_silesian)
z<-data.Normalization(data_lower_silesian, type="n1")
d<-dist.GDM(z, method="GDM1")
res<-smacofSym(delta=d,ndim=2,type="interval")
res1<-res$conf
#write.table(res1,"conf_2d.csv",dec=",",sep=";",col.names=NA,row.names=TRUE)
alfa<- 1.05*pi
a<- cos(alfa)
b<- -sin(alfa)
c<- sin(alfa)
d<- cos(alfa)
D<-array(c(a,b,c,d), c(2,2))
#res1<-read.csv2("conf_2d.csv", header=TRUE, row.names=1)
res1<-as.matrix(res1)
res2<-res1%*%D
plot(res2, xlab="Dimension 1",ylab="Dimension 2",main="",asp=1)
points(res2[1:31,],pch=1,font=2)
text(res2[c(1:31),],pos=3,cex=0.7,row.names(z[c(1:31),]))
r1<-res2[nrow(z),1]
r2<-res2[nrow(z),2]
r3<-res2[nrow(z)-1,1]
r4<-res2[nrow(z)-1,2]
arrows(r1,r2,r3,r4,length=0.1,col="black")
res_up<-as.matrix(dist(res2,method="euclidean"))
drawIsoquants(res2[nrow(z)-1,],steps=max(res_up)/6)
}
\keyword{isoquants}
\keyword{drawing isoquants}
