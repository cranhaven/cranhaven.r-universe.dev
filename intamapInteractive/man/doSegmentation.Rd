\name{doSegmentation}
\alias{doSegmentation}


\title{Spatial Segmentation - Clustering for Scattered Observations }

\description{This function performs segmentation of scattered 2D
data based on  sampling density and location.}

\usage{doSegmentation(object)}

\arguments{
\item{object}{An Intamap type object containing the element (list)
\code{observations}, which includes the coordinates of the
observation locations}}

\details{This function performs segmentation of scattered 2D data
based on  sampling density and location. Let us assume that
\code{No} is the number of observation locations. If \code{No}< 200,
then a single cluster is returned. 

(1) The segmentation algorithm
first  removes isolated distant points, if there are any, from the
observation locations.
 Points $(xi,yi)$ are characterized as 'isolated' and 'distant' if they satisfy the following conditions :
    $abs(xi-mean(x)) > 4 *std(x) or abs(yi-mean(y)) > 4 *std(y)$
and distance from closest neighbor $>
sqrt((std(x)/2)^2+(std(y)/2)^2)$. After the first step the size of
the original dataset is reduced to N (N= No - isolated points)
points. 

(2) A sampling density matrix (lattice) consisting of N
cells that cover the study area is constructed. Each cell is
assigned  a density value equal to the number of  observation points
inside the cell. In addition, each observation point is assigned the
sampling density value of the containing cell. 

(3) Unsupervised
clustering edge detection is used to determine potential cluster
perimeters.  

(4) Each closed region's perimeter is labeled with a
different cluster (segment) number. 

(5) All observation points
internal to a cluster perimeter are assigned to the specific
cluster. 

(6) Each cluster that contains fewer than 50 observation
points is rejected. 

(7) The observation points that have not
initially been assigned to a cluster and those belonging to rejected
(small) clusters are assigned at this stage. The assignment takes
into account both the distance of the points from the centroids of
the accepted clusters as well as the mean sampling density of the
clusters.


Note: The \code{No}< 200 empirical constraint is used to avoid
extreme situations in which the sampling density is concentrated
inside a few cells of the background lattice, thereby inhibiting the
edge detection algorithm.


} \value{A modified Intamap object which additionally includes the
list element \code{clusters}. This element is a list that contains
(i) the indices of  removed points from \code{observations}; (ii)
the indices of the clusters to which the remaining observation
points are assigned
 and (iii) the number of clusters detected.
\item{clusters}{list element added to the original object containing the segmentation results.
				\describe{	
        \item{rmdist}{Indices of  removed points.}
        \item{index}{Index array identifying the cluster in which each observation point belongs.}
        \item{clusterNumber}{Number of clusters detected.}
        }
		}
}

\references{ [1] D. T. Hristopulos, M. P. Petrakis, G.
Spiliopoulos, A. Chorti (2009). Non-parametric estimation of
geometric anisotropy from environmental sensor network measurements,
StatGIS 2009: Geoinformatics for Environmental Surveillance
Proceedings (ed. G. Dubois).
}

\author{A. Chorti, Spiliopoulos Giannis, Hristopulos Dionisis}

\examples{
library(gstat)

data(walker)
# coordinates(walker)=~X+Y
object=createIntamapObject(observations=walker)
object=doSegmentation(object)

print(summary(object$clusters$index))



}
\keyword{spatial }
