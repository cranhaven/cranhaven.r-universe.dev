\name{estimateESTATICS}
\alias{estimateESTATICS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Estimate parameters in the ESTATICS model.
}
\description{
Evaluation of the
ESTATICS model (Weisskopf (2013) using nonlinear least squares regression
and a quasi-likelihood approach assuming a noncentral chi- or a Rician distribuion
for the data. The latter  should be preferred in case of low SNR (high resolution)
data to avoid biased parameter estimates. Quasi-likelihood estimation requires
a specification of the scale parameter sigma of the data distribution.
}
\usage{
estimateESTATICS(mpmdata, TEScale = 100, dataScale = 1000, method = c("NLR", "QL"),
                 sigma = NULL, L = 1, maxR2star = 50,
                 varest = c("RSS", "data"), verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mpmdata}{
   Object of class MPMData as created by \code{\link{readMPMData}}.
}
  \item{TEScale}{
   scale factor for TE (used for improved numerical stability)
}
  \item{dataScale}{
   scale factor for image intensities (used for improved numerical stability)
}
  \item{method}{
  either "NLR" or "QL". Specifies non-linear regression or quasi-likelihood.
}
  \item{sigma}{
  scale parameter sigma of signal distribution (either a scalar or a 3D array).
  (only needed in case of \code{method="QL"}.)
}
  \item{L}{
  effective number of receiver coils (2*L is degrees of freedom of the signal distribution).
  L=1 for Rician distribution. (only needed in case of \code{method="QL"}.)
}
  \item{maxR2star}{
  maximum value allowed for the R2star parameter in the ESTATICS model.
}
\item{varest}{
  For parameter covariance estimation use either residual sum of squares (RSS)
  or estimate variances for T1, MT (is available) and PD from higest intensity
  images using function \code{awsLocalSigma}from package \pkg{aws}.
}
  \item{verbose}{
   logical: Monitor process.
}
}
\value{
list with components
                 \item{modelCoeff }{Estimated parameter maps}
                 \item{invCov }{ map of inverse covariance matrices}
                 \item{rsigma}{map of residual standard deviations}
                 \item{isConv }{ convergence indicator map}
                 \item{isThresh}{ logical map indicating where \code{R2star==maxR2star}. }
                 \item{sdim }{ image dimension}
                 \item{nFiles }{ number of images}
                 \item{t1Files }{ vector of T1 filenames}
                 \item{pdFiles }{ vector of PD filenames}
                 \item{mtFiles }{ vector of MT filenames}
                 \item{model }{ model used (depends on specification of MT files)}
                 \item{maskFile }{ filename of brain mask}
                 \item{mask }{ brain mask}
                 \item{sigma }{ sigma}
                 \item{L }{ L}
                 \item{TR }{ TR values}
                 \item{TE }{ TE values}
                 \item{FA }{ Flip angles (FA)}
                 \item{TEScale }{ TEScale}
                 \item{dataScale }{dataScale}
and class-attribute 'ESTATICSModel'
}
\references{
Weiskopf, N.; Suckling, J.; Williams, G.; Correia, M. M.; Inkster, B.; Tait, R.;
Ooi, C.; Bullmore, E. T. & Lutti, A. Quantitative multi-parameter mapping of R1,
PD(*), MT, and R2(*) at 3T: a multi-center validation. Front Neurosci,
Wellcome Trust Centre for Neuroimaging, UCL Institute of Neurology,
University College London, UK., 2013, 7, 95

J. Polzehl, K. Tabelow (2019). Magnetic Resonance Brain Imaging:
Modeling and Data Analysis Using R. Springer, Use R! series. Doi:10.1007/978-3-030-29184-6.

}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}
}

\seealso{
\code{\link{readMPMData}}, \code{\link{calculateQI}},
\code{\link{smoothESTATICS}}, \code{\link{writeESTATICS}},
\code{\link[aws]{awsLocalSigma}}
}
\examples{
\donttest{
dataDir <- system.file("extdata",package="qMRI")
#
#  set file names for T1w, MTw and PDw images
#
t1Names <- paste0("t1w_",1:8,".nii.gz")
mtNames <- paste0("mtw_",1:6,".nii.gz")
pdNames <- paste0("pdw_",1:8,".nii.gz")
t1Files <- file.path(dataDir, t1Names)
mtFiles <- file.path(dataDir, mtNames)
pdFiles <- file.path(dataDir, pdNames)
#
#  file names of mask and B1 field map
#
B1File <- file.path(dataDir, "B1map.nii.gz")
maskFile <- file.path(dataDir, "mask0.nii.gz")
#
#  Acquisition parameters (TE, TR, Flip Angle) for T1w, MTw and PDw images
#
TE <- c(2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8,
        2.3, 4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4)
TR <- rep(25, 22)
FA <- c(rep(21, 8), rep(6, 6), rep(6, 8))
#
#   read MPM example data
#
library(qMRI)
mpm <- readMPMData(t1Files, pdFiles, mtFiles,
                   maskFile, TR = TR, TE = TE,
                   FA = FA, verbose = FALSE)
#
#  Estimate Parameters in the ESTATICS model
#
modelMPM <- estimateESTATICS(mpm, method = "NLR")
# Alternatively using Quasi-Likelihood
sigma <- 50
modelMPMQL <- estimateESTATICS(mpm, method = "QL",
                  sigma = array(sigma,mpm$sdim), L = 1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }% use one of  RShowDoc("KEYWORDS")
\keyword{ regression }% __ONLY ONE__ keyword per line
