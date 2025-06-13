\name{Box}
\alias{Box}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Boxplots Across Time
}
\description{
This function is to compare groups using boxplots at each time point. 
In addition, line segments are used to connect the mean/median of each boxplot 
of the same group across time to show the differences between 
the mean trajectories.
}
\usage{
Box(
    data, 
    x = NULL, 
    y, 
    group = NULL, 
    fill = NULL, 
    theme_classic = TRUE, 
    fill.alpha = 0.7, 
    box.width = 0.5, 
    dodge.width = 0.8, 
    jitter = TRUE, 
    jitter.alpha = 0.7, 
    jitter.width = 0.2, 
    point.size = 1, 
    xlab = x, 
    ylab = y, 
    group.lab = group, 
    fill.lab = group, 
    title = "Boxplot", 
    line = "mean", 
    line.color = "black", 
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
character. The column name of \code{data} that indicates the first grouping variable (e.g. observation time)
}
  \item{y}{
character. The column name of \code{data} that indicates the variable on y axis
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups (e.g., treatment group). The boxplots will be drawn for each of the subject group within each category of \code{x}. 
}
  \item{fill}{
boxplot inside color indicated by the categories of \code{group}
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
}
  \item{fill.alpha}{
boxplot transparency
}
  \item{box.width}{
	  boxplot width
}
  \item{dodge.width}{
dodge width for boxplot and jitter (prevent overlapping)
}
  \item{jitter}{
logical. plot jitter or not, default TRUE
}
  \item{jitter.alpha}{
jitter transparency
}
  \item{jitter.width}{
jitter width in boxplot
}
  \item{point.size}{
	  size of a jitter point
}
  \item{xlab}{
character. x axis label
}
  \item{ylab}{
character. y axis label
}
  \item{group.lab}{
label of group variable
}
  \item{fill.lab}{
label of fill variable
}
  \item{title}{
character. title of plot
}
  \item{line}{
line connect boxes, default plot mean, can be set as 'median', or NULL (no line)
}
  \item{line.color}{
connection line color, only available when group = NULL
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
library(dplyr)

data(longDat)

print(dim(longDat))
print(longDat[1:3,])

print(table(longDat$time, useNA = "ifany"))
print(table(longDat$grp, useNA = "ifany"))
print(table(longDat$sid, useNA = "ifany"))

print(table(longDat$time, longDat$grp))

statVisual(type = 'Box', 
           data = longDat, 
           x = 'time', 
           y = 'y', 
           group = 'grp',
	   title = "Boxplots across time") 

Box( 
    data = longDat, 
    x = 'time', 
    y = 'y', 
    group = 'grp',
    title = "Boxplots across time") 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

