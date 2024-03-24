\name{print.twostageTE}
\alias{print.twostageTE}
\alias{print}
\title{
print for twostageTE
}
\description{
print method for twostageTE
}
\usage{
\method{print}{twostageTE}(x, ...)
}
\arguments{
  \item{x}{
twostageTE object
}
  \item{\dots}{
ignored
}
}
\value{
prints basic information about the object (point estimate and confidence intervals)
}
\author{
Shawn Mankad
}

\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
oneStage_IR=stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)
print(oneStage_IR)

## The function is currently defined as
function (x, ...) 
{
    if (!inherits(x, "twostageTE")) {
        stop("Error:  Object is not of class twostageTE")
    }
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        cat("Call:\n")
        dput(cl)
    }
    cat(sprintf("\n\%.1f\%\% Confidence Interval", x$level * 100))
    if (is.na(x$L2)) {
        cat(sprintf("\nn   Lower   d0_hat   Upper\n\%d   \%.3f   \%.3f   \%.3f\n", 
            length(x$Y1), x$L1, x$estimate, x$U1))
    }
    else {
        cat(sprintf("\nn1   n2   Lower   d0_hat   Upper\n\%d   \%d   \%.3f   \%.3f   \%.3f\n", 
            length(x$Y1), length(x$Y2), x$L2, x$estimate, x$U2))
    }
    invisible(x)
  }
}


