\name{dpwdg}
\alias{dpwdg}
\docType{data}
\title{Output list object of class ICEwedge for the High Uncertainty example, data(dulxparx)}
\description{
 dpwdg is the output list object of class ICEwdg resulting from the following time consuming
 computation: dpwdg <- ICEwedge(dpunc)
}
\usage{data(dpwdg)}
\format{
  Output list object of class ICEwedge.
  \describe{
    \item{ICEinp}{Name of the ICEuncrt object input to ICEwedge().}
    \item{lambda}{Positive value of lfact * ICEu\$lambda}
    \item{lfact}{Positive Multiplier for the ICEu\$lambda value input to ICEwedge().}  
    \item{ceunit}{Saved value of ceunit ("cost" or "effe") input to ICEuncrt.}
    \item{conf}{Statistical Confidence Level within [0.50, 0.99] input to ICEwedge.}  
    \item{R}{Saved integer value for number of bootstrap replications input to ICEuncrt.}
    \item{axys}{R x 4 data.frame with ICE Angle in column 1, bootstrap resampled values of
     (DeltaEffe, DeltaCost) in columns 2 and 3, and the binary flag with 0 => outcome outSide the 
     Confidence Wedge and 1 => outcome inSide the Confidence Wedge in column 4.}  
    \item{t1}{Observed value of (DeltaEffe, DeltaCost) when each patient is sampled exactly once.}
    \item{ia1}{The ICE Angle corresponding to the Objerved ICE Ratio.}
    \item{center}{The largest value of j such that axys[j, 1] < ia1 <= axys[j+1, 1].}
    \item{jlo}{Number of the ICE Angle Order Statistic defining the Clockwise or lower ICE Ray
     boundary of the Confidence Wedge.}
    \item{kup}{Number of the ICE Angle Order Statistic defining the Counter-Clockwise or upper
     ICE Ray boundary of the Confidence Wedge.}
    \item{subangle}{Subtended Polar ICE Angle between Order Statistics jlo and kup.}
    \item{xmax}{Alias plots of ICEwedge have horizontal range [-xmax, +xmax].}
    \item{ymax}{Alias plots of ICEwedge have vertical range [-ymax, +ymax].}
    \item{ab}{ICE angle computation perspective of alibi or alias.}
  }
}
\examples{
    # Intermediate ICEinfer Output List for the dulxparx dataset...
    data(dpwdg)
    plot(dpwdg)
}
\keyword{datasets}