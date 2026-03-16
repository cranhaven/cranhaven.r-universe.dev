\name{spline_interp}
\alias{spline_interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bezier spline interpolation of observations.
}
\description{
Calculate a Bezier spline interpolation of irregular observations to regular-length time intervals.
}
\usage{
spline_interp(di, area_map=NULL, t_reg=NULL, reg_dt=120,
              max_dt_wo_obs=60*30, maxStep=NULL, 
              centroids=matrix(c(0,0), ncol=2),
              nstates=2, spline_deg=3, split_logv=-3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{di}{
Object of class \code{data.frame} containing irregular-spaced observations.  Dataset must contain the following fields: 1) \code{"X"} and \code{"Y"}: X and Y location coordinates, 
2) \code{"date_as_sec"}: time/date of observation as seconds since an epoch.  \code{"time_to_next"} should also be included, but will be calculated if not.  Also \code{"tag"} (animal
identifier) should be included as well.  If not, all observations are assumed to be of the same animal.
This can be generated from \code{sim_trajectory_joint} with \code{gen_irreg=TRUE}. 
}
 \item{area_map}{
Shapefile that all interpolated points should be inside of.
}
  \item{t_reg}{
Desired time steps (must have a constant difference) to interpolate to.  If is given, the default value of
\code{reg_dt} is overridden.  Will be truncated to the set of values within the range of observed values of
\code{di$date_as_sec}.
}
  \item{reg_dt}{
Length in seconds of each regular interval.
}
  \item{max_dt_wo_obs}{
When interpolating, the maximum time length without observations for a given shark that we will interpolate.  If this is exceeded, algorithm will wait until next observation and start from there.  
}
  \item{maxStep}{
Maximum number of regular steps to interpolate.
}
  \item{centroids}{
Matrix with two columns specifying the centroids of regions.  If \code{NULL}, only one region will be used.
}
  \item{nstates}{
Number of behavioral states.  For now restricted to a maximum of 2.
}
  \item{spline_deg}{
Degree of spline.  The default is 3, or a cubic.  Every \code{spline_deg}+1 observations will be used to construct one spline segment.
}
  \item{split_logv}{
If \code{nstates}=2, state 1 from the interpolated values will be designated by the logvelocity being < \code{split_logv}.
}
}
\value{
\item{d}{Array of regular step locations.}
\item{di}{Original irregular-step dataset.}
\item{shark_names}{Vector of the names of sharks in the dataset.}
\item{d_ds}{Output regular-step dataset \code{d} in form \code{data.frame}.}
}
\references{
Bezier R package.  Aaron Olsen.
}
\author{
	Samuel Ackerman
}
%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#can also be 'di' output of sim_trajectory_joint (set gen_irreg=TRUE)

di <- data.frame(X=runif(n=9), Y=runif(n=9),
                 time_to_next=c(2,4,15,8,5,18,3,5,NA))
di$date_as_sec <- c(0, cumsum(di$time_to_next[-9]))
region_centroids <- cbind(X=runif(2), Y=runif(2))

#one log observation with dt =18 > 16 will be omitted
spl <- spline_interp(di=di, area_map=NULL, reg_dt=3, max_dt_wo_obs=16, maxStep=NULL, 
                     centroids=region_centroids, nstates=2, spline_deg=3, split_logv=-3)

plot(di[,c("X","Y")], xlim=c(0,1), ylim=c(0,1), type="b", las=1,
     "Observations interpolated by regular interval spline")
lines(spl$d_ds[,c("X","Y")], type="l", col="red")
legend("topleft", col=1:2, legend=c("observations","spline"), lty=1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ optimize }% __ONLY ONE__ keyword per line
