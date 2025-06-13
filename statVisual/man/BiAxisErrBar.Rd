\name{BiAxisErrBar}
\alias{BiAxisErrBar}
\title{
Compare Patterns of Two Outcomes in One Scatter Plot
}
\description{
Compare patterns of two outcomes with different scales across the range of the common predictor using error bar plots. Each bar plot displays mean \eqn{+/-}{+/-} standard error. 
}
\usage{
BiAxisErrBar(dat, 
	     group, 
	     y.left, 
	     y.right, 
	     title = "Bi-Axis Error Bar Plot",
	     xlab = group, 
	     ylab.left = y.left, 
	     ylab.right = y.right, 
	     legendLabel = "y axis variables",
	     delta = NULL, 
	     cvThresh = 0.01, 
             Ntick = 5,
             semFlag = TRUE, #semFlag = FALSE if SE is required
	     GroupLevel = NULL,
	     addThemeFlag = FALSE
	     )
}
\arguments{
  \item{dat}{
A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{group}{
character. A categorical variable in \code{data} that indicates the predictor. 
}
  \item{y.left}{
character. The column name of \code{data} that indicates the first outcome variable, the error bar plot of which will be drawn on the left side.
}
  \item{y.right}{
character. The column name of \code{data} that indicates the second outcome variable, the error bar plot of which will be drawn on the right side.
}
  \item{title}{
character. title of the plot.
}
  \item{xlab}{
	  character. Label for the x-axis.
}
  \item{ylab.left}{
	  character. Label for the left-side y-axis.
}
  \item{ylab.right}{
	  character. Label for the right-side y-axis.
}
  \item{legendLabel}{
character. Legend label.
}
  \item{delta}{
numeric. A small number so that the second error bar plot will shift \code{delta} distance from the first error bar plot.
}
  \item{cvThresh}{
	  numeric. A small positive number. If the coefficient of variation (CV)
	  is smaller than \code{cvThresh}, then the scaling factor will
	  be set to one.
}
  \item{Ntick}{
integer. Number of ticks on the two y-axes.
  }
  \item{semFlag}{
logical. Indicating if standard error of the mean (\code{semFlag = TRUE}) or standard error (\code{semFlag = FALSE}) will be used to construct the error bars.
  }
  \item{GroupLevel}{
	  A vector of unique values of \code{group} indicating the order
	  of \code{group} shown in x-axis.
}
  \item{addThemeFlag}{
	  logical. Indicates if light blue background and white grid should be added to the figure.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	A list with 9 elements.
	\code{data}, \code{layers}, \code{scales}, \code{mapping},
   	\code{theme}, \code{coordinates},
\code{facet}, \code{plot_env}, and \code{labels}.
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

library(tidyverse)
library(ggplot2)

print(head(mtcars))

print(table(mtcars$gear, useNA="ifany"))

statVisual(type = "BiAxisErrBar",
  dat= mtcars,
  group = "gear",
  y.left = "mpg",
  y.right = "wt")



BiAxisErrBar(
  dat = mtcars,
  group = "gear",
  y.left = "mpg",
  y.right = "wt")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
