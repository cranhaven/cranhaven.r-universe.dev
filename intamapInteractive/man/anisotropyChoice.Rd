\name{anisotropyChoice}
\alias{anisotropyChoice}

\title{anisotropyChoice}

\description{This function combines segmentation of scattered 2D
data and estimation of anisotropy parameters using the CTI method.}

\usage{anisotropyChoice(object)}

\arguments{
\item{object}{An Intamap type object containing one 
   \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} with \code{observations}.}
}
 
\details{The function \code{AnisotropyChoice} function employs the
\code{\link{doSegmentation}} function to
 automatically separate the original dataset into clusters based on the sampling density and the spatial
locations of the data (see \code{\link{doSegmentation}} for
details). The results of the segmentation procedure and the
anisotropy analysis per cluster are returned in  a matrix of
dimension [cl]x5, where [cl] is the number of clusters . Each row of
the matrix contains the cluster index, the anisotropy ratio, the
anisotropy direction, the number of cluster points and the area
inside the convex hull of the cluster. In addition, a single set of
anisotropy parameters is returned in the element \code{anisPar}.
These parameters are calculated using weighted averages of the
covariance Hessian matrix estimates in each cluster. The weights are
based on the area enclosed by the convex hull of each cluster. }



\value{ \code{object}: A modified Intamap type object is returned,
which contains  the results of the anisotropy parameter estimation.
The anisotropy parameters are returned in the element \code{anisPar}
as described below.
\item{anisPar}{ List element in \code{object} that contains a list with the following
elements:
        \describe{
        \item{\code{ratio}}{A coarse-grained  anisotropy ratio for all the data}
        \item{\code{direction}}{A coarse-grained  anisotropy  orientation for all the data}
        \item{\code{clusters}}{A matrix of dimension [cl]x5 which determines the anisotropy per cluster.
        Each row of \code{clusters} gives the
            (cluster id, anisotropy ratio, anisotropy direction, number of points, area) for each cluster detected.}
        }
}
\item{clusters}{list element added to the original object containing the segmentation results.
				\describe{
        \item{index}{Index array identifying the cluster in which each observation point belongs. 
                    Zero value means that the observations has been removed.}
        \item{clusterNumber}{Number of clusters detected.}
        }
				}

}

\references{
[1] http://www.intamap.org

[2] A. Chorti and D. T. Hristopulos (2008). Non-parametric
Identification of Anisotropic (Elliptic) Correlations in Spatially
Distributed Data Sets, IEEE Transactions on Signal Processing,
56(10), 4738-4751 (2008).

[3] D. T. Hristopulos, M. P. Petrakis, G. Spiliopoulos, A. Chorti
(2009). Non-parametric estimation of geometric anisotropy from
environmental sensor network measurements, StatGIS 2009:
Geoinformatics for Environmental Surveillance Proceedings (ed. G.
Dubois).

 }

\author{ D.T. Hristopulos, G.Spiliopoulos, A.Chorti
} \note{ This function uses the \code{akima} package to perform
"bilinear" and "bicubic" interpolation for the estimation of spatial
derivatives}

\examples{
library(gstat)
data(walker)
object=createIntamapObject(observations=walker)
object=anisotropyChoice(object)

print(summary(object$clusters$index))
print(object$anisPar)
}

\keyword{spatial}
\keyword{cluster}
\keyword{nonparametric}
\keyword{htest}
