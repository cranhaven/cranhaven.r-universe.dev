\name{LinePlot}
\alias{LinePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Trajectory Plots
}
\description{
Compare groups based on trajectory plots. Trajectories belonging to different groups will have different colors.
}
\usage{
LinePlot(
    data, 
    x, 
    y, 
    sid,
    group = NULL, 
    xFlag = FALSE,
    points = TRUE, 
    point.size = 1, 
    theme_classic = TRUE, 
    xlab = x, 
    ylab = y, 
    title = "Trajectory plot", 
    xLevel = NULL,
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{x}{
character. The column name of \code{data} that indicates the time.
}
  \item{y}{
character. The column name of \code{data} that indicates the variable on y axis
}
  \item{sid}{
	  character. The column name of \code{data} that indicates the subject id.
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The trajectories of subjects
	 in the same group will have the same color.
}
  \item{xFlag}{
  logical. Indicate if \code{x} should be treated as continuous (\code{xFlag=TRUE})
}
  \item{points}{
	  logical. Indicates if points will be added to the trajectories on the coordinate (x, y).
}
  \item{point.size}{
	  numeric. size of the data points on the trajectories
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
}
  \item{xlab}{
character. x axis label
}
  \item{ylab}{
character. y axis label
}
  \item{title}{
character. title of plot
}
  \item{xLevel}{
character. A character vector indicating the order of the elements of \code{x} to be shown on x-axis if \code{is.null(x)==FALSE}.
}
  \item{addThemeFlag}{
	  logical. Indicates if light blue background and white grid should be added to the figure.
}
  \item{\dots}{
other input parameters for facet & theme
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with the following 9 elements: \code{data}, \code{layers}, \code{scales}, \code{mapping}, \code{theme}, \code{coordinates}, \code{facet},
\code{plot_env}, and \code{labels}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Wenfei Zhang <Wenfei.Zhang@sanofi.com>,
Weiliang Qiu <Weiliang.Qiu@sanofi.com>,
Xuan Lin <Xuan.Lin@sanofi.com>,
Donghui Zhang <Donghui.Zhang@sanofi.com>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(longDat)

print(dim(longDat))
print(longDat[1:3,])

print(table(longDat$time, useNA = "ifany"))
print(table(longDat$grp, useNA = "ifany"))
print(table(longDat$sid, useNA = "ifany"))

print(table(longDat$time, longDat$grp))

statVisual(type = "LinePlot",
  data = longDat,
  x = 'time',
  y = 'y',
  sid = 'sid',
  group = 'grp')

LinePlot(
  data = longDat,
  x = 'time',
  y = 'y',
  sid = 'sid',
  group = 'grp')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

