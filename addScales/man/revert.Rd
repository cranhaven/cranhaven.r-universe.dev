\name{revert}
\alias{revert.scaledTrellis}
\alias{revert}

\title{
Revert A Scaled Trellis Plot To Its Previous Unscaled Form
}
\description{
S3 generic and scaledTrellis method to remove all scaling information from a \code{scaledTrellis} object, returning the prior unscaled trellis object.
}
\usage{
revert(obj,\dots)
\method{revert}{scaledTrellis}(obj, \dots)
}
\arguments{
  \item{obj}{
An object inheriting from class \code{scaledTrellis}.
}
  \item{\dots}{
Currently ignored   }
}
\details{
Returns the last version of the \code{trellis} object with all \code{addScales} scales and legends removed. Note that this is \emph{not} the original \code{trellis} object if that was subsequently modified by \code{update} calls. See the examples.
}

\value{
A \code{trellis} object that can be printed/plotted as usual.
}

\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}

\seealso{
    \code{\link{update.scaledTrellis}}
}
\examples{
## Using simple artificial data
set.seed (2233)
x <- rep(1:10,4)
y <- rnorm(40, mean = rep(seq(10, 25, by = 5), each = 10),
           sd = rep(1:4, each = 10))
f <- rep(c("AA","BB","CC","DD"), each = 10)
##
## trellis plot the data with "free" y axis sxaling
orig <- xyplot(y ~ x|f, type = c("l","p"), col.line = "black",
       scales = list(alternating =1,
                     y = list(relation = "free")),
       as.table = TRUE,
       layout = c(2,2),
       main = "revert() Example"
)
## Plot it
orig

## Remove the y axis scales and add horizontal scalelines
orig <- update(orig, scales = list(alternating =1,
            y = list(relation = "free", draw = FALSE)))
upd1 <- addScales(orig)
## Plot it
upd1
class(upd1)

## revert
upd2 <- revert(upd1)
## Plot it
upd2
class(upd2)

## clean up
rm(x, y, f, orig, upd1, upd2)
}

\keyword{aplot}
\keyword{iplot}
\keyword{hplot}
\keyword{methods}
