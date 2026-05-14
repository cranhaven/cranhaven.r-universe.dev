\name{assign.data}
\alias{assign.data}
\title{
Assign data to clusters
}
\description{
After clustering assign additional data from a data frame with columns indicated as latitude and longitude.
}
\usage{
assign.data(cluster, points, dist=1000)
}
\arguments{
  \item{cluster}{
  The from cca() generated data frame with cluster-information.
}
  \item{points}{
    Data frame with additional data, containing at least a "lat" and "long" column with point coordinates which will be assigned.
}
  \item{dist}{
    The assignment distance given in meters. Are the given point coordinates within this distance from an identified cluster, this point will be assigned to the cluster.
}
}
\details{
Multiple points can be assigned to the same cluster. If no cluster is within the given distance, the cluster_id  will be 0.
}
\value{
Returns the data frame given as points with an additional column "cluster_id" referring to the identified cluster. A cluster_id 0 indicates that no cluster was within the given distance.
}

\examples{
data(landcover)

# clustering urban areas
urban <- cca(landcover, cell.class=1,s=2000, unit="m")
str(urban)

# plot the result    
result <- landcover*NA
result[cellFromXY(result,urban$cluster[,c("long","lat")])]<-urban$cluster[,"cluster_id"]*(-1)
plot(result, col=rainbow(9))

# data.frame with additional information (name)    
data.points <- data.frame(
      long=c(13.26,13.28),
      lat=c(52.34,52.20),
      name=c("Pappelhausen","New Garden")
    )
    
points(data.points$long, data.points$lat, pch="X")
    
assign.data(cluster=urban$cluster, points=data.points, dist=3000)
}
\keyword{utils}
