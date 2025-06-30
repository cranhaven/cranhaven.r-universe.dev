\name{rotateAnisotropicData}
\alias{rotateAnisotropicData}

\title{rotateAnisotropicData}

\description{This function applies an isotropic transformation of
the coordinates specified in \code{object}.}

\usage{rotateAnisotropicData(object,anisPar)}

\arguments{
\item{object}{   (i) An Intamap type  object (see \code{\link{intamap-package}}) 
   containing one \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}
   data frame named  \code{observations} which includes the observed values (ii) or a 
   \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}
   which includes both coordinates and observations or (iii) \code{\link[sp:SpatialPoints]{SpatialPoints}} which includes only
   coordinates to be rotated.}
\item{anisPar}{An array containing the anisotropy parameters (anisotropy ratio and axes orientation)
(see \code{\link{estimateAnisotropy}}) for the rotation.
  If \code{object} is the output of \code{\link{estimateAnisotropy}} function, these parameters are part of \code{object}.
  In cases (ii) and (iii) \code{anisPar} defines the two anisotropy
  parameters. For the definition of the anisotropy parameters see \code{\link{estimateAnisotropy}}.}
} \details{This function performs a rotation and rescaling of the
coordinate axes in order to obtain a new coordinate system, in which
the observations become statistically isotropic. This assumes that
the estimates of the anisotropy ratio and the orientation angle
provided in \code{anisPar} are accurate.

}

\value{(i) A modified object with transformed coordinates if 
rotateAnisotropicData is called with an Intamap object as input (see
\code{\link{intamap-package}}) or (ii) the transformed coordinates if a
\code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}} is used as input or (iii)
the transformed coordinates if a \code{\link[sp:SpatialPoints]{SpatialPoints}}
object is the input. }

\references{
[1] Pebesma, E., Cornford, D., Dubois, G., Heuvelink, G.B.M., Hristopulos, D., Pilz, J., Stohlker, U., Morin, G., Skoien, J.O. INTAMAP: The design and implementation f an interoperable automated interpolation Web Service. Computers and Geosciences 37 (3), 2011. 


[2] A. Chorti and D. T. Hristopulos (2008). Non-parametric Identification of Anisotropic (Elliptic) 
Correlations in Spatially Distributed Data Sets, IEEE Transactions on Signal Processing, 56(10), 4738-4751 (2008).}

\author{ Hristopulos Dionisis, Spiliopoulos Giannis
}

\seealso{\code{estimateAnisotropy}}
\examples{

    library(gstat)
    data(sic2004)
    coordinates(sic.val)=~x+y
    sic.val$value=sic.val$dayx

    anisPar <- estimateAnisotropy(sic.val)
    print(anisPar)  

    rotatedObs <- rotateAnisotropicData(sic.val,anisPar)

    newAnisPar <- estimateAnisotropy(rotatedObs)
    print(newAnisPar)  


}
\keyword{spatial}
