\name{writeESTATICS}
\alias{writeESTATICS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write maps of ESTATICS parameters in standardized form as NIfTI files.
}
\description{
   \code{R2}, \code{ST1}, \code{SPD} and, if available, \code{SMT}-maps are written
   as compressed NIfTI files into directory the speecified directory.
   If \code{class(mpmESTATICSModel) == "sESTATICSModel"} and an smoothed data are stored in
   \code{mpmESTATICSModel$smoothedData} the smoothed data are stored as ompressed NIfTI files
   in \code{dir} with filenames assembled using \code{prefix} and the names of the data source
   files.
}
\usage{
writeESTATICS(mpmESTATICSModel, dir = NULL, prefix = "estatics", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mpmESTATICSModel}{
Object of class 'ESTATICSModel' or 'sESTATICSModel' as returned from function
\code{\link{estimateESTATICS}} or \code{\link{smoothESTATICS}}.}
  \item{dir}{
  Directory name (or path) for output.
}
  \item{prefix}{
  Prefix for file names
}
  \item{verbose}{
  logical - provide information on progress
}
}
\value{
The function returns \code{NULL}
}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}
}

\seealso{
\code{\link{readMPMData}}, \code{\link{estimateESTATICS}},
\code{\link{smoothESTATICS}}
}
\examples{
dataDir <- system.file("extdata",package="qMRI")
outDir <- tempdir()
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
#
#  resulting ESTATICS parameter maps for central coronal slice
#
if(require(adimpro)){
rimage.options(zquantiles=c(.01,.99), ylab="z")
oldpar <- par(mfrow=c(2,2),mar=c(3,3,3,1),mgp=c(2,1,0))
pnames <- c("T1","MT","PD","R2star")
modelCoeff <- extract(modelMPM,"modelCoeff")
for(i in 1:4){
   rimage(modelCoeff[i,,11,])
   title(pnames[i])
   }
}
#
#  write ESTATICS parameter maps
#
writeESTATICS(modelMPM, dir=outDir, prefix="estatics")
par(oldpar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
