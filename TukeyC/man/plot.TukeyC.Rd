\name{plot.TukeyC}
\alias{plot.TukeyC}

\title{
  Plot TukeyC and TukeyC.nest Objects
}

\description{
  S3 method to plot \code{TukeyC} and \code{TukeyC.nest} objects.
}

\usage{
\method{plot}{TukeyC}(x,
     result         = TRUE,
     replicates     = TRUE,
     pch            = 19,
     col            = NULL,
     xlab           = NULL,
     ylab           = NULL,
     xlim           = NULL,
     ylim           = NULL,
     axisx          = TRUE,
     axisy          = TRUE,
     id.lab         = NULL,
     id.las         = 1,
     yl             = TRUE,
     yl.lty         = 3,
     yl.col         = 'gray',
     dispersion     = c('none','mm','sd','ci','cip'),
     d.lty          = 1,
     d.col          = 'black',
     title          = '', \dots)
}

\arguments{
  \item{x}{A \code{TukeyC} object.}
  \item{result}{The result of the test (letters) should be visible.}
  \item{replicates}{The number of replicates should be visible.}
  \item{pch}{A vector of plotting symbols or characters.}
  \item{col}{A vector of colors for the means representation.}
  \item{xlab}{A label for the \samp{x} axis.}
  \item{ylab}{A label for the \samp{y} axis.}
  \item{xlim}{The \samp{x} limits of the plot.}
  \item{ylim}{The \samp{y} limits of the plot.}
  \item{axisx}{Axis x? If \samp{TRUE} you must accept the default, otherwise, you must customize.}
  \item{axisy}{Axis y? If \samp{TRUE} you must accept the default, otherwise, you must customize.}
  \item{id.lab}{Factor level names at \samp{x} axis.}
  \item{id.las}{Factor level names written either horizontally or vertically.}
  \item{yl}{Horizontal (reference) line connecting the circle to the \samp{y} axis.}
  \item{yl.lty}{Line type of \samp{yl}.}
  \item{yl.col}{Line color of \samp{yl}.}
  \item{dispersion}{Vertical line through the circle (mean value) linking the minimum to the maximum of the factor level values corresponding to that mean value. Other options are: sd (standard deviation), ci (confidence interval), cip (pooled confidence interval) and none.}
  \item{d.lty}{Line type of dispersion.}
  \item{d.col}{A vector of colors for the line type of dispersion.}
  \item{title}{A title for the plot.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{plot.TukeyC} function is a S3 method to plot \samp{Tukey} and
  \code{TukeyC.nest} objetcs. It generates a serie of points (the means) and a 
  vertical line showing the dispersion of the values corresponding to
  each group mean. The ci options is calculed utilizing each treatment variance as estimating of population variance. The cip options is calculed utilizing the means square error (MSE) as estimating of population variance.
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\references{
  Murrell, P. (2005) \emph{R Graphics}. Chapman and Hall/CRC Press.
}

\seealso{
  \code{\link{plot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='TukeyC')
##

library(TukeyC)
data(CRD2)

## From: formula
tk1 <- with(CRD2,
            TukeyC(y ~ x,
                   data=dfm,
                   which='x'))

old.par <- par(mar=c(6, 3, 6, 2))
plot(tk1,
     id.las=2)

plot(tk1,
     yl=FALSE,
     disp='sd',
     id.las=2)

## From: aov
av <- with(CRD2,
           aov(y ~ x,
               data=dfm))
summary(av)

tk2 <- TukeyC(x=av,
              which='x')
plot(tk2,
     disp='sd',
     yl=FALSE,
     id.las=2)

# From: lm
av_lm <- with(CRD2,
           lm(y ~ x,
              data=dfm))

tk3 <- TukeyC(x=av_lm,
              which='x')

par(mfrow=c(2, 1))
plot(tk3,
     disp='ci',
     id.las=2,
     yl=FALSE)

plot(tk3,
     disp='cip',
     id.las=2,
     yl=FALSE)

par(mfrow=c(1, 1))
par(old.par)
}

\keyword{package}

