\name{readMPMData}
\alias{readMPMData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read experimental Multi-Parameter Mapping (MPM) data.
}
\description{
The function reads data generated in Multimodal Parameter Mapping (MPM) experiments.
}
\usage{
readMPMData(t1Files = NULL, pdFiles = NULL, mtFiles = NULL, maskFile = NULL,
            TR = NULL, TE = NULL, FA = NULL, wghts = NULL, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t1Files}{
    Vector of filenames corresponding to T1 weighted images (in Nifti-Format) with varying TE
}
  \item{pdFiles}{
    Vector of filenames corresponding to PD weighted images (in Nifti-Format) with varying TE
}
  \item{mtFiles}{
    optional Vector of filenames corresponding to MT weighted images
    (in Nifti-Format) with varying TE
}
  \item{maskFile}{
   optional filename for mask (in Nifti-Format)
}
  \item{TR}{
   optional numeric TR vector, if omitted information is extracted from .nii files if possible
}
  \item{TE}{
   optional numeric TE vector, if omitted information is extracted from .nii files if possible
}
  \item{FA}{
   optional numeric FA (flip-angle) vector, if omitted information is extracted from .nii files if possible
}
  \item{wghts}{
   optional weights for MPM data volumes. Only needed is volumes have different data variance, e.g.,
    in case of averages of multiple acquisitions.
  }
  \item{verbose}{
  logical - provide information on progress
}
}
\value{
List with components
     \item{ddata}{mpm data}
     \item{sdim }{ dimension of image cube }
     \item{nFiles }{ number of images / image files}
     \item{ t1Files }{character - filenames of t1Files}
     \item{ pdFiles }{character - filenames of  pdFiles}
     \item{mtFiles }{character - filenames of  mtFiles}
     \item{ model }{ Number of the ESTATICS model that can be used }
     \item{maskFile }{character - filenames of   maskFile}
     \item{mask }{ mask}
     \item{TR }{ vector of TR values}
     \item{TE }{ vector of TE values}
     \item{FA }{ vector of FA values}
and class-attribute 'mpmData'
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{estimateESTATICS}}, \code{\link{calculateQI}},
\code{\link{smoothESTATICS}}, \code{\link{writeESTATICS}},
\code{\link[aws]{awsLocalSigma}}
}
\examples{
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
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
