\name{stRoc}
\alias{stRoc}
\title{Smooth Time-dependent ROC curve estimations}
\description{
Provides smooth estimations of \bold{Cumulative/Dynamic (C/D)} and \bold{Incident/Dynamic (I/D)} ROC curves in presence of rigth censorship and the corresponding Areas Under the Curves (AUCs), at a single point of time or a vector of points.
\itemize{
\item {The function computes two different procedures to obtain smooth estimations of the \bold{C/D ROC curve}. Both are based on the kernel density estimation of the joint distribution function of the marker and time-to-event variables, provided by \code{\link{funcen}} function. The first method, to which we will refere as \bold{smooth method}, is carried out according to the methodology proposed in \url{https://doi.org/10.1177/0962280217740786}. The second one uses this estimation of the joint density function of the variables marker and time-to-event for computing the weights or probabilities allocated to censored observations (undefined individuals) in \url{https://doi.org/10.1080/00949655.2016.1175442} and \url{ https://doi.org/10.1177/0962280216680239}. It will be referred as \bold{p-kernel} method.}
\item {In case of the \bold{I/D ROC curve}, a smooth approximation procedure (\bold{smooth method}) is computed based as well on the kernel density estimation of the joint distribution function of the marker and time-to-event variables proposed in \url{https://doi.org/10.1177/0962280217740786} }}}
\usage{
stRoc(data, t, H, bw, adj, tcr, meth, ...)
}

\arguments{
 \item{data}{matrix of data values with three columns: time-to-event, censoring status (0=censored/1=uncensored) and marker.}
 \item{t}{point of time or vector of points where the time-dependent ROC curve is estimated.}
 \item{H}{2x2 bandwidth matrix.}
 \item{bw}{procedure for computing the bandwidth matrix. Most of the methods included at the \code{kde} function can be used: Hpi, Hpi.diag, Hlscv, Hlscv.diag, Hbcv, Hbcv.diag, Hscv, Hscv.diag, Hucv and Hucv.diag. Other considered methods are naive.pdf (diag(N^{-1/5}, N^{-1/5})^2) and naive.cdf (diag(N^{-1/3}, N^{-1/3})^2), where N is the sample size.}
 \item{adj}{adjusment parameter for calculating the bandwidth matrix. Default value 1.}

 \item{tcr}{type of time-dependent ROC curve estimation that will be estimated:
            \itemize{
                \item \dQuote{C} for \bold{C}umulative/Dynamic,
                \item \dQuote{I} for \bold{I}ncident/Dynamic,
                \item \dQuote{B} for \bold{B}oth time-dependent ROC curve estimations.}
                }

\item{meth}{method for computing the estimation of the C/D ROC curve.The suitable values are:
\itemize{
                \item \dQuote{1} for the smooth method,
                \item \dQuote{2} for the p-kernel method.}
As default value the smooth method is taken.}
\item{...}{\code{\link{kde}} function arguments can be used for estimating the bivariate kernel density function.}
}

\details{
Function \code{\link{funcen}} is called from each execution of function \code{stRoc}, in order to compute the kernel
density estimation of the joint distribution of the (Marker, Time-to-event) variable, therefore, the input
parameters in \code{\link{funcen}} are input parameters as well in \code{stRoc} and the same considerations apply.

The matrix of bandwidths can be defined by using H=matrix() or automatically selected by the method indicated in \code{bw}.

Given the matrix of bandwidths, H, the argument \code{adj} modifies it and the final matrix is adj^2 H.

If \code{H} is missing, the naive.pdf method is used.

If \code{tcr} is missing the C/D ROC curve estimation will be computed with the method indicated in \code{meth}.

If no value has been placed in \code{meth} the smooth method will be used. The I/D ROC curve estimation will be always computed with the smooth method.

}
\value{
An object of class \code{sROCt} is returned. It is a list with the following values:
 \item{th}{considered thresholds for the marker.}
 \item{FP}{false-positive rate calculated at each point in \code{th}.}
 \item{TP}{true-positive rate estimated at each point in \code{th}.}
 \item{p}{points where the time-dependent ROC curve is evaluated.}
 \item{R}{time-dependent ROC curve values computed at \code{p}.}
 \item{t}{time/s at which each time-dependent ROC curve estimation is computed. Each point ot time will appear as many times as the length of the vector of points \code{p}.}
 \item{auc}{area under the corresponding time-dependent ROC curve estimation. As in the previous case, each value appears as many times as the length of the vector of points \code{p}.}
 \item{tcr}{type of time-dependent ROC curve estimation computed,
   \itemize{
       \item \dQuote{C} - \bold{C}umulative/Dynamic.
       \item \dQuote{I} - \bold{I}ncident/Dynamic.
       }
For each computed time-dependent ROC curve estimation this value is repeated as many times as the length of \code{p}.
 }
 \item{Pi}{probabilities calculated for the individuals in the sample if the p-kernel method has been used for the estimation of the C/D ROC curve. This element is a matrix with the following columns:
 \itemize{
       \item {\code{time}} - {single point of time at which the estimation each the C/D ROC curve has been computed.}
       \item {\code{obvt}} - {observed times for the individuals in the sample.}
       \item {\code{p}} - {estimations of the probabilities computed and allocated to each subject.}
       }
       }
}

\examples{
library(smoothROCtime)
require(KMsurv)

data(kidtran)

# Preparing data: a logarithmic transformation of the time-to-event variable is made
  DT <- cbind(log(kidtran$time),kidtran$delta,kidtran$age)

# Point of Time
  t5 <- log(5*365.25) # five years in logarithm scale

# Cumulative/dynamic ROC curve estimation
  rcd <- stRoc(data=DT, t=t5, bw="Hpi", tcr="C", meth=2)

# Plot graphic
  plot(rcd$p, rcd$ROC, type="l", lwd=5, main="C/D ROC",xlab="FPR",ylab="TPR")
  lines(c(0,1),c(0,1),lty=2,col="gray")

}

\references{
 {P. Martinez-Camblor and J. C. Pardo-Fernandez. Smooth time-dependent receiver operating characteristic
 curve estimators. Statistical Methods in Medical Research, 27(3):651-674, 2018.\url{https://doi.org/10.1177/0962280217740786}.
 }

 {P. Martinez-Camblor, G. F-Bay?n, and S. P?rez-Fern?ndez. Cumulative/dynamic ROC curve estimation.
 JOURNAL of Statistical Computation and Simulation, 86(17):3582-3594, 2016. \url{
 https://doi.org/10.1080/00949655.2016.1175442}.
 }

 {L. Li, T. Green, and B. Hu. A simple method to estimate the time-dependent receiver operating
 characteristic curve and the area under the curve with right censored data. Statistical Methods in
 Medical Research, 27(8), 2016. \url{https://doi.org/10.1177/0962280216680239}.
 }

 {T. Duong. Bandwidth matrices for multivariate kernel density estimation. Ph.D. Thesis, University of
 Western, Australia, 2004. \url{http://www.mvstat.net/tduong}.}
}
