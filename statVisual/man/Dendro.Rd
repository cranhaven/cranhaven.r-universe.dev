\name{Dendro}
\alias{Dendro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare Groups Based on Dendrogram
}
\description{
Compare groups based on dendrogram. The nodes of the dendrogram will be colored by group.
}
\usage{
Dendro(
    x, 
    group = NULL, 
    xlab = NULL, 
    ylab = NULL, 
    title = NULL, 
    cor.use = "pairwise.complete.obs", 
    cor.method = "pearson", 
    distance = "rawdata", 
    distance.method = "euclidean", 
    hclust.method = "complete", 
    yintercept = NULL, 
    theme_classic = TRUE, 
    addThemeFlag = TRUE,
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  A data frame. Rows are subjects; Columns are variables describing the subjects.
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The nodes of the dendrogram will be colored by info provided by \code{group}.
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
  \item{cor.use}{
	  character. Indicate which data will be used to compute correlation coefficients. It can take values \dQuote{everything}, \dQuote{all.obs}, 
	  \dQuote{complete.obs}, \dQuote{na.or.complete}, \dQuote{pairwise.complete.obs}.
}
  \item{cor.method}{
	  character. Indicate which type of correlation coefficients will be calculated: \dQuote{pearson}, \dQuote{kendall}, \dQuote{spearman}.
}
  \item{distance}{
	  character. Indicate which type of data will be used to calculate distance: \dQuote{rawdata} (i.e., using raw data to calculate distance),
  \dQuote{cor} (i.e., using correlation coefficients as distance),
  \dQuote{1-cor} (i.e., using (\eqn{1-}{1-}correlation coefficients) 
  as distance),
  \dQuote{1-|cor|} (i.e., using (\eqn{1-}{1-}|correlation coefficients|) as distance).
}
  \item{distance.method}{
	  character. Available when \sQuote{distance = "rawdata"}. Indicate the definition of distance:
distance used in calculate dist
  \dQuote{rawdata} (i.e., using raw data to calculate distance),
  \dQuote{cor} (i.e., using correlation coefficients as distance),
  \dQuote{1-cor} (i.e., using (\eqn{1-}{1-}correlation coefficients) as distance),
  \dQuote{1-|cor|} (i.e., using (\eqn{1-}{1-}|correlation coefficients|) as distance).
}
  \item{hclust.method}{
	  character. Indicate which agglomeration method will be used to perform
	  hierarchical clustering.
	  This should be (an unambiguous abbreviation of) one of 
	  \dQuote{ward.D}, \dQuote{ward.D2}, \dQuote{single}, 
	  \dQuote{complete}, \dQuote{average}, \dQuote{mcquitty}, 
	  \dQuote{median}, or \dQuote{centroid}. Please refer to
	  \code{\link{hclust}}.
}
  \item{yintercept}{
	  numeric. A line indicating the height of the dendrogram, for example,
	  indicating where the dendrogram should be cut to obtain clusters.
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

statVisual(type = 'Dendro', 
           x = pDat[, c(3:8)], 
           group = pDat$grp)

Dendro(
       x = pDat[, c(3:8)], 
       group = pDat$grp)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
