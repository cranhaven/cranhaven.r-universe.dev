\name{kilns}
\alias{kilns}
\alias{kilnAfull}
\alias{kilnAsubset}
\alias{kilnAoneOut}
\alias{kilnB}
\docType{data}
\title{
  The \code{kilns} data sets.
}
\description{
  Data on the rotation times of kilns.
}
\usage{
    kilnAfull
    kilnAsubset
    kilnAoneOut
    kilnB
}
\format{
  Each data set is a data frame with columns consisting of observations
  on variables \code{x} and \code{y}:
  \describe{
    \item{\code{x}}{a numeric vector of \emph{reciprocals} of percentages}
    \item{\code{y}}{a numeric vector of times of a single revolution of
                    a kiln, in seconds}
  }
  \itemize{
      \item The data set \code{kilnAfull} has 3793 observations.
      \item The data set \code{kilnAoneOut} has 3792 observations.
      \item The data set \code{kilnAsubset} has 92 observations.
      \item The data set \code{kilnB} has 3740 observations.
  }
}
\details{
  These data consist of observation relating to the rotation times
  of two kilns, \dQuote{A} and \dQuote{B}.  They are daily averages
  observed over 11 years, or 4017 days, from 1 January 2005 to 31
  December 2015.  The \code{kilnAsubset} data consist of a small
  subset of the \code{kilnAfull} data.  The \code{kilnAoneOut}
  data set is the same as the \code{kilnAfull} data set but with
  one row/observation, number 1171 (which appears to be an outlier
  in some sense), removed.

  The reason that \code{kilnAfull} and \code{kilnB} do not
  contain 4017 observation is that there were a number of missing
  values in both \code{x} and \code{y}.  Rows in which either or
  both \code{x} and \code{y} were missing (there were 224 such)
  were deleted.  Likewise 277 rows were deleted in the process of
  forming \code{kilnB}.

  Plots of the percentages versus times displayed patterns of
  points with curved structure.  Transforming the percentages to
  their reciprocals changed these patterns to ones that are very
  close to being straight lines.

  The kiln \dQuote{A} data clearly involve three components.
  The kiln \dQuote{B} data involve only two components (likewise
  clearly discernible).
}
\source{
The data were kindly provided by Petr Pikal (Prerov, Czech Republic).
}
\examples{
    fit1 <- mixreg(y~x,data=kilnAfull,ncomp=3,seed=173)
    plot(fit1) # Components 1 and 2 seem to have got swapped and 
               # the component 1 (???) line is a bit skew-wiff.
# There's a point that looks to be a bit of an outlier.
# It has been identified to be point 1171.
    with(kilnAfull,text(x[1171],y[1171],labels="1171",
                        adj=-0.3,col="red"))
# Removing this point gives kilnAoneOut.
    fit2 <- mixreg(y~x,data=kilnAoneOut,ncomp=3,seed=173)
    plot(fit2) # Still no good; same as fit1, although the "outlier" is gone.
\dontrun{
    vfit <- visualFit(y~x,data=kilnAoneOut,ncomp=3)
    fit3 <- mixreg(y~x,data=kilnAoneOut,ncomp=3,thetaStart=vfit$theta)
    plot(fit3) # Much better.
    chk <- mixreg(y~x,data=kilnAfull,ncomp=3,thetaStart=vfit$theta)
    plot(chk) # No good; same as fit1 and fit2 but without the swapping
              # of components 1 and 2.  It was the outlier that caused the
              # problem, not the random starting values.
    }
   thStrt <- list(
                   list(beta=c(26.07,48808),sigsq=1.1573,lambda=0.33333333),
                   list(beta=c(23.48,32387),sigsq=1.8730,lambda=0.33333333),
                   list(beta=c(-0.0597,20760),sigsq=0.2478,lambda=0.33333333)
                 )
# Roughly vfit$theta.
    fit3.a <- mixreg(y~x,data=kilnAoneOut,ncomp=3,thetaStart=thStrt)
    plot(fit3.a) # Sames as fit3.
    chk.a <- mixreg(y~x,data=kilnAfull,ncomp=3,thetaStart=thStrt)
    plot(chk.a) # Same as chk.
}
\keyword{datasets}
