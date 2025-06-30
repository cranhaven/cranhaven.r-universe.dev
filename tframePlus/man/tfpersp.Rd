\name{tfpersp}
\alias{tfpersp}

\title{Plot Series With persp}
\description{Plot series with persp}

\usage{
    tfpersp (x, tf=tfspan(x), start=tfstart(tf), end=tfend(tf),
       theta = -30, phi = 15, scale = FALSE, 
       xlab = "Time", ylab = "", zlab = "", 
       aspect= c(0.5, 0.5), #y/time, z/time,
       ticktype="detailed",ltheta = -120, lphi = 15,
       ...)
}

\arguments{
    \item{x}{multivariate time series (a tframed object).}
    \item{tf}{a tframe to set the start and end of the plot.}
    \item{start}{start period for the plot.}
    \item{end}{end period for the plot.}
    \item{theta}{argument passed to \code{persp}}
    \item{phi}{argument passed to \code{persp}}
    \item{scale}{argument passed to \code{persp}}
    \item{xlab }{argument passed to \code{persp}}
    \item{ylab}{argument passed to \code{persp}}
    \item{zlab}{argument passed to \code{persp}}
    \item{aspect}{argument passed to \code{persp}}
    \item{ticktype}{argument passed to \code{persp}}
    \item{ltheta}{argument passed to \code{persp}}
    \item{lphi}{argument passed to \code{persp}}
    \item{...}{additional arguments passed to \code{persp}}
}
\value{Depends}
\details{
  This function uses passes a multivariate time series to persp.
}

\seealso{
    \code{\link[tframe]{tfwindow}}
    \code{\link[graphics]{persp}}
}
\keyword{utilities}
\keyword{ts}
