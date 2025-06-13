\name{cv_glmnet_plot}
\alias{cv_glmnet_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Cross-Validation Curve Produced by cv.glmnet
}
\description{
Plots the cross-validation curve, and upper and lower standard error curves, as a function of the values of the tuning parameter lambda.
}
\usage{
cv_glmnet_plot(x, 
	       y, 
	       family = "binomial", 
	       addThemeFlag = TRUE, 
	       ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   a matrix with rows are subjects and columns are numeric variables (predictors). No missing values are allowed.
}
  \item{y}{
   a vector of response. The number of elements of \code{y} is the same as the number of rows of \code{x}.
  }
  \item{family}{
   character. Indicating response type. see the description in \code{\link{glmnet}}.   
  }
    \item{addThemeFlag}{
	  logical. Indicates if light blue background and white grid should be added to the figure.
}
  \item{...}{
  other input parameters for \code{\link{glmnet}} function.
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
library(tibble)
library(glmnet)

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


statVisual(type = "cv_glmnet_plot",
           x = as.matrix(pDat[, c(3:8)]), 
           y = pDat$grp, 
           family = "binomial")

cv_glmnet_plot(x = as.matrix(pDat[, c(3:8)]), 
               y = pDat$grp, 
               family = "binomial")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
