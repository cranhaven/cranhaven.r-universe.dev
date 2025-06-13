\name{PCA_score}
\alias{PCA_score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatter Plot of 2 Specified Principal Components
}
\description{
Scatter plot of 2 specified principal components. The size of the data points on the PCA plot indicates the Mahalanobis distance (distance between each point and mean value).

}
\usage{
PCA_score(
    prcomp_obj, 
    data, 
    dims = c(1, 2),
    color = NULL, 
    MD = TRUE, 
    loadings = FALSE, 
    loadings.color = "black", 
    loadings.label = FALSE,
    title = "pca plot",
    addThemeFlag = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prcomp_obj}{
	  the object returned by the function \code{\link{prcomp}}.
}
  \item{data}{
  A data frame. Rows are subjects; Columns are variables describing the subjects. The object \code{prcomp_obj} is based on \code{data}
}
  \item{dims}{
a numeric vector with 2 elements indicating which two principal components
will be used to draw scatter plot.
}
  \item{color}{
   character. The column name of \code{data} that indicates the subject groups. The data points on the PCA plot will be colored by the group info.
}
  \item{MD}{
  logical. Indicate if the Mahalanobis distance (distance between each point and mean value) would be used to indicate the size of data points on the PCA plot
}
  \item{loadings}{
  logical. Indicate if loading plot would be superimposed on the PCA plot. (default: FALSE)
}
  \item{loadings.color}{
  character. Indicate the color of the loading axis.
}
  \item{loadings.label}{
  logical. Indicating if loading labels should be added to the plot. (default: FALSE)
}
  \item{title}{
  character. Figure title.
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
%note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
library(factoextra)

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

###

pca.obj = iprcomp(pDat[, c(3:8)], scale. = TRUE)

# scree plot
factoextra::fviz_eig(pca.obj, addlabels = TRUE)

# scatter plot of PC1 vs PC2
statVisual(type = 'PCA_score',
           prcomp_obj = pca.obj, 
           dims = c(1, 2),
           data = pDat, 
           color = 'grp',
           loadings = FALSE)

PCA_score(prcomp_obj = pca.obj, 
          dims = c(1, 3),
          data = pDat, 
          color = 'grp',
          loadings = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

