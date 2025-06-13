\name{ImpPlot}
\alias{ImpPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of Variable Importance
}
\description{
Plot of variable importance based on results from \code{\link{randomForest}} or \code{\link{gbm}}.
}
\usage{
ImpPlot(model, 
	theme_classic = TRUE, 
	n.trees = NULL, 
	addThemeFlag = TRUE,
	...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	  An object returned by \code{\link{randomForest}} or \code{\link{gbm}}
}
  \item{theme_classic}{
	  logical. Use classic background without grids (default: TRUE).
}
  \item{n.trees}{
	  integer. The number of trees used to generate the plot
	  used in the function \code{summary.gbm} in the R library \code{\link{gbm}}. 
	  Only the first n.trees trees will be used.
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

library(dplyr)
library(randomForest)
library(tibble)


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

# choose the first 6 probes (3 OE probes, 2 UE probes, and 1 NE probe)
pDat$probe1 = dat[1,]
pDat$probe2 = dat[2,]
pDat$probe3 = dat[3,]
pDat$probe4 = dat[4,]
pDat$probe5 = dat[5,]
pDat$probe6 = dat[6,]

print(pDat[1:2, ])

# check histograms of probe 1 expression in cases and controls
print(table(pDat$grp, useNA = "ifany"))

pDat$grp = factor(pDat$grp)


rf_m = randomForest(
  x = pDat[, c(3:8)], 
  y = pDat$grp, 
  importance = TRUE, proximity = TRUE
)


statVisual(type = 'ImpPlot', model = rf_m)

ImpPlot(model = rf_m)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

