\name{stackedBarPlot}
\alias{stackedBarPlot}
\title{
Draw Stacked Bar Plots
}
\description{
Draw stacked bar plots.
}
\usage{
stackedBarPlot(dat, 
	       catVar, 
	       group, 
	       xlab = catVar, 
	       ylab = "Count", 
	       group.lab = group, 
	       title = "Stacked barplots of counts", 
	       catVarLevel = NULL, 
	       groupLevel = NULL, 
	       addThemeFlag = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
	  A data frame object. Rows are subjects and columns are variables.
}
  \item{catVar}{
	  character. The name of the cateogrical variable to be shown in x-axis.
}
  \item{group}{
	  character. The name of variable indicating groups of subjects.
}
  \item{xlab}{
	  character. Label for x-axis.
}
  \item{ylab}{
	  character. Label for y-axis.
}
  \item{group.lab}{
	  character. Label for group in legend.
}
  \item{title}{
	  character. Figure title.
}
  \item{catVarLevel}{
	  character. A vector indicating the order of the unique elements of \code{catVar} should be shown in x-axis.
}
  \item{groupLevel}{
	  character. A vector indicating the order of the unique elements of \code{group} should be shown in figure and in legend.
}
  \item{addThemeFlag}{
	  logical. Indicates if light blue background and white grid should be added to the figure.
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
data(genoSim)

pDat = pData(genoSim)
geno = exprs(genoSim)

pDat$snp1 = geno[1,]

print(table(pDat$snp1, pDat$grp, useNA="ifany"))

stackedBarPlot(dat = pDat, 
	       catVar = "snp1", 
	       group = "grp", 
               xlab = "snp1", 
	       ylab = "Count", 
	       group.lab = "grp",
               title = "Stacked barplots of counts",
               catVarLevel = NULL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
