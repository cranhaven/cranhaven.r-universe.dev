\name{smoothESTATICS}
\alias{smoothESTATICS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adaptive smoothing of ESTATICS parameters and MPM data
}
\description{
Performs adaptive smoothing of parameter maps in the ESTATICS model and if
\code{mpmData} is specified these data. Implements both vectorized variants of the
Adaptive Weights Smoothing (AWS, Polzehl and Spokoiny (2006))
and patchwise AWS (PAWS, Polzehl et al (2018)) algorithms with weighting schemes determined by
the estimated parameter maps and their covariances.
}
\usage{
smoothESTATICS(mpmESTATICSModel, mpmData = NULL, kstar = 16, alpha = 0.025,
               patchsize = 0, mscbw =5, wghts = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mpmESTATICSModel}{
Object of class 'ESTATICSModel' as returned from function \code{\link{estimateESTATICS}}.
}
  \item{mpmData}{
(optional) Object of class MPMData as created by \code{\link{readMPMData}} from which
the parameter maps were obtained.
}
  \item{kstar}{
Maximum number of steps.
}
  \item{alpha}{
specifies the scale parameter for the adaptation criterion. smaller values are more restrictive.
}
  \item{patchsize}{
Patchsize in PAWS, 0 corresponds to AWS, alternative values are 1 and 2.
}
  \item{mscbw}{
   bandwidth for 3D median smoother used to stabilize the covariance estimates.
}
  \item{wghts}{
 (optional) voxel size if measurments are not isotropic.
}
  \item{verbose}{
  logical - provide information on progress
}
}
\value{
list with components
                 \item{modelCoeff }{Estimated parameter maps}
                 \item{invCov }{ map of inverse covariance matrices}
                 \item{isConv }{ convergence indicator map}
                 \item{bi}{ Sum of weights map from AWS/PAWS}
                 \item{smoothPar}{ smooting parameters used in AWS/PAWS}
                 \item{smoothedData}{ smoothed mpmData}
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
and class-attribute 'sESTATICSModel'
}
\references{
J. Polzehl, V. Spokoiny,  Propagation-separation approach for local likelihood estimation,  Probab. Theory Related Fields  135 (3),  (2006)  , pp. 335--362.

J. Polzehl, K. Papafitsorus, K. Tabelow (2018). Patch-wise adaptive weights smoothing. WIAS-Preprint 2520.

J. Polzehl, K. Tabelow (2019). Magnetic Resonance Brain Imaging:
Modeling and Data Analysis Using R. Springer, Use R! series. Doi:10.1007/978-3-030-29184-6.
}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{readMPMData}}, \code{\link{estimateESTATICS}}
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
maskFile <- file.path(dataDir, "mask.nii.gz")
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
#
#  smooth maps of ESTATICS Parameters
#
setCores(2)
modelMPMsp1 <- smoothESTATICS(modelMPM,
                              kstar = 16,
                              alpha = 0.004,
                              patchsize=1,
                              verbose = TRUE)
#
#  resulting ESTATICS parameter maps for central coronal slice
#
if(require(adimpro)){
rimage.options(zquantiles=c(.01,.99), ylab="z")
oldpar <- par(mfrow=c(2,4),mar=c(3,3,3,1),mgp=c(2,1,0))
pnames <- c("T1","MT","PD","R2star")
modelCoeff <- extract(modelMPM,"modelCoeff")
for(i in 1:4){
   rimage(modelCoeff[i,,11,])
   title(pnames[i])
   }
modelCoeff <- extract(modelMPMsp1,"modelCoeff")
for(i in 1:4){
   rimage(modelCoeff[i,,11,])
   title(paste("smoothed",pnames[i]))
   }
}
par(oldpar)
}
}
\keyword{ model }% use one of  RShowDoc("KEYWORDS")
\keyword{ smooth }% __ONLY ONE__ keyword per line
