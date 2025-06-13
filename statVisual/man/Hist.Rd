\name{Hist}
\alias{Hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Histograms
}
\description{
Compare groups based on histograms.
}
\usage{
Hist(
    data, 
    y, 
    group = NULL, 
    fill = group, 
    border.color = NULL, 
    inner.color = NULL, 
    theme_classic = TRUE, 
    bins = NULL, 
    binwidth = NULL, 
    alpha = 0.8, 
    xlab = y, 
    ylab = "count", 
    group.lab = group, 
    title = "Histogram", 
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{y}{
	  character. The column name of \code{data} that indicates the variable,
	  for which the histogram will be drawn. The string \code{y} can also indicate a function of the variable, e.g., \eqn{\log(y)}{log(y)}.

}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The histogram will be drawn for each of the subject group. It also indicates the border colors of the histograms.
}
  \item{fill}{
	  character. The column name of \code{data} that indicates the subject groups. It indicates the inside colors of the histograms.
}
  \item{border.color}{
	 Histogram border color, only available when group & fill are NULL.
}
  \item{inner.color}{
Histogram inside color, only available when group & fill are NULL.
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
}
  \item{bins}{
	  integer. number of bins of histogram (default: 30).
}
  \item{binwidth}{
	  Bin width of histogram.
}
  \item{alpha}{
	  Transparency of histogram inside color.
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
	A list with the following 9 elements.
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
data(esSim)
print(esSim)

# expression data
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])

# feature data
fDat = fData(esSim)
print(dim(fDat))
print(fDat[1:2,])

# choose the first probe which is over-expressed in cases
pDat$probe1 = dat[1,]

# check histograms of probe 1 expression in cases and controls
print(table(pDat$grp, useNA = "ifany"))

statVisual(type = 'Hist', 
       data = pDat, 
       y = 'probe1', 
       group = 'grp') 

Hist(
     data = pDat, 
     y = 'probe1', 
     group = 'grp') 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

