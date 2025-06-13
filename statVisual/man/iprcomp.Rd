\name{iprcomp}
\alias{iprcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Improved Function for Obtaining Principal Components
}
\description{
Calculate principal components when data contains missing values.
}
\usage{
iprcomp(dat, center = TRUE, scale. = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
	  n by p matrix. rows are subjects and columns are variables
}
  \item{center}{
	  logical. Indicates if each row of \code{dat} needs to be mean-centered
}
  \item{scale.}{
	  logical. Indicates if each row of \code{dat} needs to be scaled to have variance one
}
}
\details{
We first set missing values as median of the corresponding variable, then call the function \code{prcomp}.
This is a very simple solution. The user can use their own imputation methods before calling \code{prcomp}.
}
\value{
A list of 3 elements
\item{sdev }{square root of the eigen values}
\item{rotation }{a matrix with columns are eigen vectors, i.e., projection direction}
\item{x }{a matrix with columns are principal components}
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
# generate simulated data
set.seed(1234567)
dat.x = matrix(rnorm(500), nrow = 100, ncol = 5)
dat.y = matrix(rnorm(500, mean = 2), nrow = 100, ncol = 5)
dat = rbind(dat.x, dat.y)
grp = c(rep(0, 100), rep(1, 100))
print(dim(dat))

res = iprcomp(dat, center = TRUE, scale.  =  FALSE)

# for each row, set one artificial missing value
dat.na=dat
nr=nrow(dat.na)
nc=ncol(dat.na)
for(i in 1:nr)
{
  posi=sample(x=1:nc, size=1)
  dat.na[i,posi]=NA
}

res.na = iprcomp(dat.na, center = TRUE, scale.  =  FALSE)

##
# pca plot
##
par(mfrow = c(3,1))
# original data without missing values
plot(x = res$x[,1], y = res$x[,2], xlab = "PC1", ylab  =  "PC2")
# perturbed data with one NA per probe 
# the pattern of original data is captured
plot(x = res.na$x[,1], y = res.na$x[,2], xlab = "PC1", ylab  =  "PC2", main = "with missing values")
par(mfrow = c(1,1))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
