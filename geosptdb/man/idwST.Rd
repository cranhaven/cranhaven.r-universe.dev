\name{idwST}
\alias{idwST}

\title{
Inverse Distance Weighting (IDW) function for spatio-temporal prediction.
}
\description{
This function performs spatio-temporal interpolation. Here \emph{idwST} is in a local neighborhood. This interpolation method considers the value of a point can be obtained from the weighted sum of values of the regionalized variable of closest neighbors. The general formula for the IDW is given by:
\deqn{
\hat{z}_0(st)=\sum_{i=1}^n \lambda_i z_i(st)
}
The expression for determining the weights is:
\deqn{
\lambda_i = \frac{d_{i0}^{-p}}{\sum_{i=1}^n d_{i0}^{-p}}
}
The weight is controlled by a factor \emph{p} with each increment of the distance, \eqn{d_{i0}} is the distance between the prediction position and each of the measured positions.

The expression \eqn{d_{i0}} can be obtained by:
\deqn{
d_{i0}=\sqrt{(x_{i}-x_{0})^2+(y_{i}-y_{0})^2+C\cdot (t_{i}-t_{0})^2}
}
\eqn{x}, \eqn{y} and \eqn{t} correspond to the spatio-temporal coordinates, \emph{p} (factor.p) and \emph{C} factors defined below.


}
\usage{
idwST(formula, data, newdata, n.neigh, C, factor.p, progress)
}

\arguments{
\item{formula}{formula that defines a detrended linear model, use \eqn{z_{st}}\code{~1}.}
\item{data}{SpatialPointsDataFrame: should contain the spatio-temporal dependent variable, independent variables (statics and/or dynamics), spatial coordinates and the time as an integer or numerical variable.}
\item{newdata}{data frame or spatial object with prediction/simulation spatio-temporal locations; should contain attribute columns with the independent variables (if present) and (if locations is a formula) the coordinates and time with names, as defined in locations where you want to generate new predictions}
\item{n.neigh}{number of nearest observations that should be used for a \emph{idwST}
prediction, where nearest is defined in terms of the spatio-temporal locations}
\item{C}{numeric; associated to time factor, we recommend using the parameter found by
minimizing the root-mean-square prediction errors using cross-validation. Using \emph{idwST.cv} and \code{\link[stats]{optimize}}}
\item{factor.p}{numeric; specify the inverse distance weighting power (\emph{p} is the exponent that influences the weighting or optimal smoothing parameter)}
\item{progress}{whether a progress bar shall be printed for spatio-temporal inverse-distance weighted function; default=TRUE}
}
\details{
idwST function generates individual spatio-temporal predictions from IDW spatio-temporal interpolation. IDW is a type of deterministic method for interpolation, the assigned values to unknown points are calculated with a weighted average of the values available at the known points.
}
\value{
Attributes columns contain coordinates, time, predictions, and the variance
column contains NA's
}

\references{
Li L, Losser T, Yorke C, Piltner R. (2014). \emph{Fast inverse distance weighting-based spatiotemporal interpolation: a web-based application of interpolating daily fine particulate matter PM2:5 in the contiguous U.S. using parallel programming and k-d tree.} Int. J. Environ. Res. Public Health, 11: 9101-9141. \href{https://www.mdpi.com/journal/ijerph}{[link]}
}


\examples{
# Loading Croatia data
data(croatia2008)
coordinates(croatia2008) <- ~x+y

# prediction case: one point
point <- data.frame(670863,5043464,5)
names(point) <- c("x","y","t")

coordinates(point) <- ~x+y
idwST(MTEMP~1, data=croatia2008, newdata=point, n.neigh=60, C=1, factor.p=2)

\dontrun{
# prediction case: a grid of points Croatia (year 2008)
data(croatia)
points <- spsample(croatia, n=5000, type="regular")

data(croatia2008)
coordinates(croatia2008)<-~x+y

GridsT <- vector(mode = "list", length = 12)

for(i in 1:12){
GridsT[[i]] <- data.frame(points@coords,i)
names(GridsT[[i]]) <- c("x","y","t")
}

idw.croatia <- data.frame(matrix(NA, ncol = 14, nrow=nrow(GridsT[[1]])))
pb <- txtProgressBar(min = 0, max = 12, char = "=", style = 3)
for(i in 1:12){
coordinates(GridsT[[i]]) <- c("x", "y")
idw.croatia[,i+2] <- idwST(MTEMP~1, croatia2008, newdata=GridsT[[i]], n.neigh=10, C=1,
                          factor.p=2, progress=FALSE)[,4]
setTxtProgressBar(pb, i)
}
close(pb)

idw.croatia[,1:2] <- GridsT[[1]]@coords
nam <- paste(c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"),
             2008,sep="")
names(idw.croatia) <- c("x","y",nam)

coordinates(idw.croatia) <- c("x", "y")
gridded(idw.croatia) <- TRUE

# show prediction map
pal2 <- colorRampPalette(c("blue3", "wheat1", "red3"))

p1 <- spplot(idw.croatia[,1:12], cuts=30, col.regions=pal2(35), colorkey=F,
            scales = list(draw =T,cex=0.6, abbreviate=TRUE,minlength=1), pch=0.3,
            cex.lab=0.3, cex.title=0.3, auto.key = F, main = "Earth's average
            temperature IDW map 2008", key.space=list(space="right", cex=0.8))

split.screen( rbind(c(0, 1,0,1), c(1,1,0,1)))
split.screen(c(1,2), screen=1)-> ind
screen( ind[1])
p1
screen( ind[2])
image.plot(legend.only=TRUE, legend.width=0.5, col=pal2(100),
           smallplot=c(0.7,0.75, 0.3,0.7), zlim=c(min(idw.croatia@data),
           max(idw.croatia@data)), axis.args = list(cex.axis = 0.7))
close.screen( all=TRUE)
}
}
\keyword{ spatial }
