\name{barPlot}
\alias{barPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Barplots Across Time
}
\description{
This function is to compare groups using barplots at each time point. 
In addition, line segments are used to connect the mean/median of each barplot 
of the same group across time to show the differences between 
the mean trajectories. 
Also, for each barplot 
the barplot of mean \eqn{+/-}{+/-} standard error will be plot.
}
\usage{
barPlot(
    data, 
    x = NULL, 
    y, 
    group = NULL,
    semFlag = TRUE,
    xFlag = FALSE, 
    bar.width = 0.5, 
    dodge.width = 0.8, 
    jitter = FALSE, 
    jitter.alpha = 0.7, 
    jitter.width = 0.1, 
    line = NULL, 
    line.color = "black", 
    xlab = x, 
    ylab = line, 
    theme_classic = TRUE, 
    group.lab = group, 
    title = "bar plots", 
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
character. The column name of \code{data} that indicates the first grouping variable
}
  \item{y}{
character. The column name of \code{data} that indicates the variable on y axis
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The barplots will be drawn for each of the subject group within each category of \code{x}. 
}
  \item{semFlag}{
  logical. Indicate if sem or se should be used to draw error bar
  }
  \item{xFlag}{
  logical. Indicate if \code{x} should be treated as continuous (\code{xFlag=TRUE})
  }
  \item{bar.width}{
numeric. error bar width
}
  \item{dodge.width}{
numeric. dodge width for error bar and jitter (prevent overlapping)
}
  \item{jitter}{
logical, plot jitter or not, default TRUE
}
  \item{jitter.alpha}{
numeric. jitter transparency
}
  \item{jitter.width}{
numeric. jitter width in error bar
}
  \item{line}{
character. line connect error bar, default uses mean, can be set as 'median', NULL (no line)
}
  \item{line.color}{
character. connection line color, only available when group = NULL
}
  \item{xlab}{
character. x axis label
}
  \item{ylab}{
character. y axis label
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
}
  \item{group.lab}{
	  character. label of group variable
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
A list of the following 9 elements:
\dQuote{data}, \dQuote{layers}, \dQuote{scales}, \dQuote{mapping},
\dQuote{theme}, \dQuote{coordinates}, \dQuote{facet}, \dQuote{plot_env},
\dQuote{labels}.
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

statVisual(type = 'barPlot', 
  data = longDat, 
  x = 'time', 
  y = 'y', 
  group = 'grp',
  title = "Bar plots across time") 


barPlot(
  data = longDat, 
  x = 'time', 
  y = 'y', 
  group = 'grp',
  title = "Bar plots across time") 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

