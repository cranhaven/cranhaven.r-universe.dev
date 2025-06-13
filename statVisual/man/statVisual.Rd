\name{statVisual}
\alias{statVisual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Wrapper Function Incorporating All Wrapper Functions in statVisual
}
\description{
The wrapper function incorporating all wrapper functions in statVisual.
}
\usage{
statVisual(type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
	  character. Indicate the functions to be called. It can take 
	  the following values:
	  \dQuote{BiAxisErrBar},
	  \dQuote{Box},
	  \dQuote{BoxROC},
	  \dQuote{cv_glmnet_plot},
	  \dQuote{Den},
	  \dQuote{Dendro},
	  \dQuote{ErrBar},
	  \dQuote{Heat},
	  \dQuote{Hist},
	  \dQuote{ImpPlot},
	  \dQuote{iprcomp},
	  \dQuote{LinePlot},
	  \dQuote{PCA_score},
	  \dQuote{PVCA},
	  \dQuote{statVisual},
	  \dQuote{Volcano},
	  \dQuote{XYscatter}.
}
  \item{\dots}{
	  input parameters for the functions specified by \code{type}.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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

\seealso{
	  \code{\link{BiAxisErrBar}},
	  \code{\link{Box}},
	  \code{\link{BoxROC}},
	  \code{\link{cv_glmnet_plot}},
	  \code{\link{Den}},
	  \code{\link{Dendro}},
	  \code{\link{ErrBar}},
	  \code{\link{Heat}},
	  \code{\link{Hist}},
	  \code{\link{ImpPlot}},
	  \code{\link{iprcomp}},
	  \code{\link{LinePlot}},
	  \code{\link{PCA_score}},
	  \code{\link{PVCA}},
	  \code{\link{statVisual}},
	  \code{\link{Volcano}},
	  \code{\link{XYscatter}}.
}
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


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")

