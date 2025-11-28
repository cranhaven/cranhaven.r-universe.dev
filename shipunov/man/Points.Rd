\name{Points}
\alias{Points}
\alias{PPoints}

\title{Number of cases in each location reflected in the point size}
\description{Number of cases in each location reflected in the point size}

\usage{
Points(x, y, pch=1, centers=FALSE, scale=1, cex.min=1, col=1,
 na.omit=TRUE, plot=TRUE, ...)
PPoints(groups, x, y, cols=as.numeric(groups), pchs=as.numeric(groups),
 na.omit.all=TRUE, ...)
}

\arguments{
\item{x, y}{Coordinates}
\item{pch}{Point type}
\item{pchs}{Types of point groups}
\item{centers}{If TRUE, show centers of each location as a pixel-size dot (pch=".")}
\item{cex.min}{Minimal point size}
\item{col}{Color of points}
\item{cols}{Color of point groups}
\item{na.omit}{If TRUE (default), skip data points with NAs}
\item{plot}{If FALSE, does not plot}
\item{na.omit.all}{If TRUE (default), skip data points and corresponding factor
 values with NAs, then make 'na.omit' for internal Points() FALSE}
\item{scale}{Scale factor for point size}
\item{groups}{Factor defining groups}
\item{...}{Points() passes other arguments to points(), PPoints() passes other arguments to Points()}
}

\details{
Frequently, more then one data point is located in one coordinate place (so called
"overplotting"). How to show overplotting? One way is 'jitter()', these is also
(really advanced) 'sunflowerplot()'. 'Points()' does it in its own way: number of
cases in each point will be reflected in the point size. 'Points()' is a low-level
graphic function, analogous to 'points()'.

'PPoints()' is the same as 'Points()' but for multiple subgroups.

To prettify plot, it is recommended to change 'scale' and optionally also 'cex.min'.

Alternative is the base R 'sunflowerplot()' but it is hard to read and there is no possibility
to show multiple groups in data. Another alternative might be points with transparent color.
}

\value{
Invisibly returns vector of "multiplication indexes", in case of PPoints() it is group-wise
so overplotting between groups does not count. Please keep in mind that these
indexes only indicate how many times the point is overplotted, but do not show groups of duplicates.
Use Alldups() for groups.
}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{jitter}}, \code{\link{sunflowerplot}}}

\examples{
## colors modified via palette()
plot(iris[, 1:2], type="n")
palette(rainbow(3))
PPoints(iris[, 5], iris[, 1], iris[, 2], pchs=0, scale=0.7)
palette("default")
## now with centers, colors default, pch by group, and one NA
iris[1, 1] <- NA
plot(iris[, 1:2], type="n")
PPoints(iris[, 5], iris[, 1], iris[, 2], scale=0.7, centers=TRUE)
data(iris) ## to restore default embedded object
}

\keyword{aplot}
