\name{boxplot.TukeyC}
\alias{boxplot.TukeyC}

\title{
  Boxplot TukeyC Objects
}

\description{
  S3 method to plot \code{TukeyC} objects.
}

\usage{
\method{boxplot}{TukeyC}(x,
          mean.type   = c('line', 'point', 'none'),
          xlab        = NULL,
          mean.col    = 'gray',
          mean.pch    = 1,
          mean.lwd    = 1,
          mean.lty    = 1,
          args.legend = NULL, \dots)
}

\arguments{
  \item{x}{A \code{TukeyC} object.}
  \item{mean.type}{The type of mean must be ploted. O default is "line".}
  \item{xlab}{A label for the \samp{x} axis.} 
  \item{mean.col}{A vector of colors for the means representation.}
  \item{mean.pch}{A vector of plotting symbols or characters. Only if type are "point".}
  \item{mean.lwd}{Line width of mean.}
  \item{mean.lty}{Line type of mean. Only if type are "line".}
  \item{args.legend}{List of additional arguments to be passed to \code{legend}; The default is \code{NULL}.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{boxplot.TukeyC} function is a S3 method to plot \samp{TukeyC} objetcs. The difference to generic function is the Tukey inference under frame and the plot of means within box.
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
  \code{\link{boxplot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='TukeyC')
##

library(TukeyC)
data(CRD1)

## From: formula
# Simple!
tk1 <- TukeyC(y ~ x,
              data=CRD1$dfm,
              which='x')
boxplot(tk1)

# A little more elaborate!
boxplot(tk1,
        mean.lwd=1.3,
        mean.col='red')

# A little more! 
boxplot(tk1,
        mean.lwd=1.3,
        mean.lty=2,
        mean.col='red',
        args.legend=list(x='bottomleft'))

# With point type!
boxplot(tk1,
        mean.type='point')

boxplot(tk1,
        mean.type='point',
        mean.pch=19,
        cex=1.5,
        mean.col='red')

# With other point
boxplot(tk1,
        mean.type='point',
        mean.pch='+',
        cex=2,
        mean.col='blue',
        args.legend=list(x='bottomleft'))
}

\keyword{package}

