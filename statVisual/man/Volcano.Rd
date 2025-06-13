\name{Volcano}
\alias{Volcano}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volcano Plot
}
\description{
Volcano plot with the option to label the significant results.
}
\usage{
Volcano(
    resFrame, 
    stats, 
    p.value, 
    group = NULL, 
    xlab = "logFC", 
    ylab = "-log10(p value)", 
    title = NULL, 
    vline.col = "orange", 
    hline.col = "dodgerblue", 
    vline = list(xintercept = c(-1, 1), label = c(-1, 1)), 
    hline = list(
        yintercept = c(-log10(0.05), 
                       -log10(0.05/nrow(resFrame)), 
                       -log10(max(resFrame[p.adjust(resFrame[, p.value], 
                                       method = "fdr") <= 0.05, p.value]))), 
        label = c("p value: 0.05", "Bonferroni: 0.05", "FDR: 0.05")), 
    rowname.var = NULL, 
    point.size = 3, 
    theme_classic = TRUE, 
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resFrame}{
	  A data frame stored information about the results, including gene id, statistic (e.g., log fold change, odds ratio), p-value, and significance of a gene. 
}
  \item{stats}{
	  character. The column name of \code{resFrame} that indicates the effect of a gene.
}
  \item{p.value}{
	  character. The column name of \code{resFrame} that indicates the p-value.
}
  \item{group}{
	  character. The column name of \code{resFrame} that indicates the significance of a gene.
}
  \item{xlab}{
	  x axis label
}
  \item{ylab}{
	  y axis label
}
  \item{title}{
	  title of the plot
}
  \item{vline.col}{
	  color of the vertical lines (default: \dQuote{orange})
}
  \item{hline.col}{
	  color of the horizontal lines (default: \dQuote{dodgerblue})
}
  \item{vline}{
	  A list with two elements: \dQuote{xintercept} and \dQuote{label},
	  where the former element is a numeric vector indicating the x-axis location to draw vertical color lines and the latter element is list of labels for the elements in \dQuote{xintercept}.
	  
}
  \item{hline}{
	  A list with two elements: \dQuote{yintercept} and \dQuote{label},
	  where the former element is a numeric vector indicating the y-axis location to draw horizontal color lines and the latter element is list of labels for the elements in \dQuote{xintercept}.
}
  \item{rowname.var}{
	  character. The column name of \code{resFrame} that indicates which variable will be used to label the significant results in the volcano plot. The elements of this column for non-significant results should be set to be \code{NA}.
}
  \item{point.size}{
	  size of data points in the plot.
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
library(ggrepel)
library(limma)

# load the simulated dataset
data(esSim)
print(esSim)

# expression levels
y = exprs(esSim)
print(dim(y))
print(y[1:2,])

# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat)

# design matrix
design = model.matrix(~grp, data = pDat)
print(design)

options(digits = 3)

# Ordinary fit
fit <- lmFit(y, design)
fit2 <- eBayes(fit)

# get result data frame
resFrame = topTable(fit2,coef = 2, number = nrow(esSim))
print(dim(resFrame))
print(resFrame[1:2,])
resFrame$sigFlag  =  resFrame$adj.P.Val < 0.05

resFrame$probe  =  rownames(resFrame)
# make sure set NA to genes non-differentially expressed
resFrame$probe[which(resFrame$sigFlag == FALSE)] = NA

print(resFrame[1:2,])
print(table(resFrame$sigFlag, useNA = "ifany"))

statVisual(type = 'Volcano',
           resFrame = resFrame, 
           stats = 'logFC', 
           p.value = 'P.Value', 
           group = 'sigFlag', 
           rowname.var = 'probe', 
           point.size = 1)

Volcano(
  resFrame = resFrame, 
  stats = 'logFC', 
  p.value = 'P.Value', 
  group = 'sigFlag', 
  rowname.var = 'probe', 
  point.size = 1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

