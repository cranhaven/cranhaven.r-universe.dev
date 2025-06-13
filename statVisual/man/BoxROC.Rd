\name{BoxROC}
\alias{BoxROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Boxplots with ROC Curve
}
\description{
Compare boxplots with ROC curve.
The value of the variable \code{y} will be jittered shown on the boxplots. The area under ROC curve will also be calculated and shown in the plot of ROC curve.
}
\usage{
BoxROC(
    data, 
    group.var, 
    y, 
    box.xlab = group.var, 
    box.ylab = y, 
    box.group.lab = group.var, 
    jitter.alpha = 0.8, 
    jitter.width = 0.1, 
    point.size = 3, 
    roc.xlab = "Specificity", 
    roc.ylab = "Sensitivity",
    addThemeFlag = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{group.var}{
	  character. The column name of \code{data} that indicates the two subject groups. It also indicates
	  the color of the two boxplots.
}
  \item{y}{
character. The column name of \code{data} that indicates the variable,
	  for which the box will be drawn. 
}
  \item{box.xlab}{
character. boxplot x axis label (default: \code{group.var})
}
  \item{box.ylab}{
character. boxplot y axis label (default: \code{y})
}
  \item{box.group.lab}{
character. boxplot legend label (default: \code{group.var})
}
  \item{jitter.alpha}{
numeric. transparency of jitters
}
  \item{jitter.width}{
numeric. width of jitters
}
  \item{point.size}{
	  size of a jitter point
}
  \item{roc.xlab}{
character. roc curve x axis label (default: \code{Specificities})
}
  \item{roc.ylab}{
character. roc curve y axis label (default: \code{Sensitivities})
}
  \item{addThemeFlag}{
	  logical. Indicates if light blue background and white grid should be added to the figure.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with the following 12 elements: \code{grobs}, \code{layout},
\code{widths}, \code{heights}, \code{respect}, \code{rownames},
\code{colnames}, \code{name},
\code{gp}, \code{vp}, \code{children}, \code{childrenOrder}.
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
library(gridExtra)

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

statVisual(type = 'BoxROC', 
           data = pDat, 
           group = 'grp', 
           y = 'probe1', 
           point.size = 1)

BoxROC(
  data = pDat,
  group = 'grp', 
  y = 'probe1', 
  point.size = 1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
