\name{Heat}
\alias{Heat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heatmap with Row Names Colored by Group
}
\description{
Heatmap with row names colored by group.
}
\usage{
Heat(data, 
     group = NULL, 
     fontsize_row=10,
     fontsize_col=10, 
     scale = "none",
     cluster_rows = TRUE,
     cluster_cols = TRUE,
     color = colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100),
     angle_col = c("270", "0", "45", "90", "315"), 
     ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data frame. Rows are subjects; Columns are variables describing the subjects. Except the column indicating subject group, all columns of \code{data} should be numeric.
}
  \item{group}{
	  character. The column name of \code{data} that indicates the subject groups. The row names of the heatmap will be colored based on \code{group}.
}
  \item{fontsize_col}{
	  x axis label font size
}
  \item{fontsize_row}{
	  y axis label font size
}
  \item{scale}{
	  character. Indicate how data will be scaled: 
	  \dQuote{none} (i.e., no scaling), \dQuote{row} (i.e., row scaled), 
	  \dQuote{column} (i.e., column scaled).
}
 \item{cluster_rows}{
	  logic. Indicates if rows should be clustered.
}
  \item{cluster_cols}{
  logic. Indicates if columns should be clustered.
	  
}
  \item{color}{
  vector indicating colors used in heatmap
}
  \item{angle_col}{
  angle of the column labels. Please refer to the manual in \code{\link{pheatmap}}
}
  \item{\dots}{
	  other input parameters for facet & theme.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	A list with 10 elements:
	 \dQuote{rowInd}, \dQuote{colInd}, \dQuote{call}, \dQuote{carpet},
 	 \dQuote{rowDendrogram}, \dQuote{colDendrogram}, \dQuote{breaks},
 	 \dQuote{col}, \dQuote{colorTable}, \dQuote{layout}.
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
\note{
This function is based on the function \code{\link{pheatmap}} in \code{pheatmap} R package.
}

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

statVisual(type = 'Heat', 
           data = pDat[, c(2:8)], 
           group = 'grp')

Heat(
     data = pDat[, c(2:8)], 
     group = 'grp')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

