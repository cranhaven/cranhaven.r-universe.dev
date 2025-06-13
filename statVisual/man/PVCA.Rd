\name{PVCA}
\alias{PVCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal Variance Component Analysis (PVCA)
}
\description{
Plot of weighted average proportion variance versus effects in 
principal variance component analysis (PVCA).
}
\usage{
PVCA(
    clin_data, 
    clin_subjid, 
    gene_data, 
    pct_threshold = 0.8, 
    batch.factors, 
    theme_classic = FALSE, 
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clin_data}{
A data frame containing clinical information, including  an id variable that corresponds to rownames of \code{gene_data}
}
  \item{clin_subjid}{
	  character. The column name of \code{clin_data} that indicates subject id. It corresponds to the rowname of \code{gene_data}.
}
  \item{gene_data}{
	  A data frame with genes as rows and subjects as columns.
}
  \item{pct_threshold}{
numeric. The percentile value of the minimum amount of the variabilities that the selected principal components need to explain
}
  \item{batch.factors}{
	  character. A vector of factors that the mixed linear model will be fit on.
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
library(pvca)


data(esSim)
print(esSim)

# expression data
dat = exprs(esSim)
print(dim(dat))
print(dat[1:2,])

# create a fake Batch variable
esSim$Batch=c(rep("A", 4), rep("B", 6), rep("C", 10))
# phenotype data
pDat = pData(esSim)
print(dim(pDat))
print(pDat[1:2,])


# feature data
fDat = fData(esSim)
print(dim(fDat))
print(fDat[1:2,])


statVisual(type = 'PVCA',
           clin_data = pData(esSim), 
           clin_subjid = "sid", 
           gene_data = exprs(esSim), 
           batch.factors = c("grp", "Batch"))

PVCA( 
     clin_data = pData(esSim), 
     clin_subjid = "sid", 
     gene_data = exprs(esSim), 
     batch.factors = c("grp", "Batch"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

