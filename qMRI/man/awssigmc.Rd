\name{awssigmc}
\alias{awslsigmc}
\alias{awssigmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate noise variance for multicoil MR systems
}
\description{
    The distribution of image intensity values \eqn{S_i} divided by the noise standard deviation in \eqn{K}-space \eqn{\sigma} 
    in dMRI experiments is assumed
    to follow a non-central chi-distribution with \eqn{2L} degrees of freedom and noncentrality parameter \eqn{\eta}, where \eqn{L} refers to the number of receiver 
    coils in the system and \eqn{\sigma \eta} is the signal of interest. This is an idealization in the sense that 
    each coil is assumed to have the same contribution at each location. For realistic modeling \eqn{L} should 
    be a locally smooth function in voxel space that reflects the varying local influence of the receiver coils in the
    the reconstruction algorithm used. 
  
    The functions assume \eqn{L} to be known and estimate either a local 
    (function \code{awslsigmc}) or global ( function \code{awssigmc}) 
    \eqn{\sigma} employing an assumption of local homogeneity for
     the noncentrality parameter \eqn{\eta}.
    
    Function \code{afsigmc} implements estimates from Aja-Fernandez (2009). 
    Function \code{aflsigmc} implements the estimate from Aja-Fernandez (2013). 
}
\usage{
awssigmc(y, steps, mask = NULL, ncoils = 1, vext = c(1, 1), lambda = 20, 
         h0 = 2, verbose = FALSE, sequence = FALSE, hadj = 1, q = 0.25, 
         qni = .8, method=c("VAR","MAD"))
awslsigmc(y, steps, mask = NULL, ncoils = 1, vext = c(1, 1), lambda = 5, minni = 2, 
         hsig = 5, sigma = NULL, family = c("NCchi"), verbose = FALSE, 
         trace=FALSE, u=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
    3D array, usually obtained from an object of class \code{dwi} as
    \code{obj@si[,,,i]} for some \code{i}, i.e. one 3D image from an dMRI experiment.
    Alternatively a vector of length \code{sum(mask)} may be suppied together with a brain mask in \code{mask}.
}
  \item{steps}{
   number of steps in adapive weights smoothing, used to reveal the unerlying
   mean structure.
}
  \item{mask}{
   restrict computations to voxel in mask, if \code{is.null(mask)} all voxel are used.
   In function \code{afsigmc} mask should refer to background for \code{method \%in\% c("modem1chi","bkm2chi","bkm1chi")} and to voxel within the head for 
   \code{method=="modevn"}. 
}
  \item{ncoils}{
   number of coils, or equivalently number of effective degrees of freedom of non-central chi distribution
   divided by 2.
}
  \item{vext}{
   voxel extentions
}
  \item{lambda}{
   scale parameter in adaptive weights smoothing
}
  \item{h0}{
   initial bandwidth
}
  \item{verbose}{
   if \code{verbose==TRUE} density plots 
   and quantiles of local estimates of \code{sigma} are provided.
}
  \item{trace}{
   if \code{trace==TRUE} intermediate results for each step are
   returned in component tergs for all voxel in mask.
}
  \item{sequence}{
   if \code{sequence=TRUE} a vector of estimates for the noise
   standard deviation \code{sigma} for the individual steps is returned
   instead of the final value only.  
}
  \item{hadj}{
   adjustment factor for bandwidth (chosen by \code{bw.nrd}) in mode estimation 
}
  \item{q}{
   quantile to be used for interquantile-differences. 
}
  \item{qni}{
   quantile of distribution of actual sum of weights \eqn{N_i=\sum_j w_{ij}} in adaptive smoothing. Only voxel i with \eqn{N_i > q_{qni}(N_.)} are used for variance estimation. Should be larger than 0.5.  
}
  \item{method}{ in case of function \code{awssigmc} the
   method for variance estimation, either "VAR" (variance) or "MAD" (mean absolute deviation). In function \code{afsigmc} see last column in Table 2 in Aja-Fernandez (2009). 
}
  
  \item{minni}{Minimum sum of weights for updating values of \code{sigma}.}
  \item{hsig}{Bandwidth of the median filter.} 
  \item{sigma}{Initial estimate for \code{sigma}}
  \item{family}{One of \code{"Gauss"} or \code{"NCchi"} (default) defining the 
      probability distribution to use.} 
  \item{u}{if \code{verbose==TRUE} an array of noncentrality paramters for
      comparisons. Internal use for tests only}
}
\value{
   a list with components
\item{sigma}{either a scalar or a vector of estimated noise standard deviations.
}
\item{theta}{the estimated mean structure}
}
\references{K. Tabelow, H.U. Voss, J. Polzehl, Local estimation of the noise level in MRI using structural adaptation, Medical Image Analysis, 20 (2015), pp. 76--86.
}
\author{
  J\"org Polzehl \email{polzehl@wias-berlin.de}
}
\keyword{ smooth }
