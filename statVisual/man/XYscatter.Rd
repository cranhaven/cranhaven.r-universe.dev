\name{XYscatter}
\alias{XYscatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Scatter Plots
}
\description{
Compare groups based on scatter plots.
}
\usage{
XYscatter(
    data, 
    x, 
    y, 
    group = NULL, 
    alpha = 1, 
    point.size = 3, 
    xlab = x, 
    ylab = y, 
    group.lab = group, 
    title = "Scatter plot", 
    theme_classic = TRUE, 
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{x}{
	  character. The column name of \code{data} that indicates the variable on the x axis of the scatter plot 
}
  \item{y}{
	  character. The column name of \code{data} that indicates the variable on the y axis of the scatter plot 
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The scatter plot will be drawn for each of the subject group. It also indicates the colors of the data points in the scatter plots.
}
  \item{alpha}{
	  Transparency of histogram inside color.
}
  \item{point.size}{
	  numeric. Indicate the size of the data points 
}
  \item{xlab}{
	  x axis label
}
  \item{ylab}{
	  y axis label
}
  \item{group.lab}{
	  label of group variable
}
  \item{title}{
	  title of the plot
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
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
		A list with 9 elements.
	\code{data}, \code{layers}, \code{scales}, \code{mapping},
   	\code{theme}, \code{coordinates},
\code{facet}       \code{plot_env}, and \code{labels}.
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
data(diffCorDat)

print(dim(diffCorDat))
print(diffCorDat[1:2,])

statVisual(type = 'XYscatter',
  data = diffCorDat, 
  x = 'probe1', 
  y = 'probe2', 
  group = 'grp', 
  title = 'Scatter Plot: probe1 vs probe2')

XYscatter( 
  data = diffCorDat, 
  x = 'probe1', 
  y = 'probe2', 
  group = 'grp', 
  title = 'Scatter Plot: probe1 vs probe2')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

