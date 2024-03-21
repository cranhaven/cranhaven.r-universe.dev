\name{extract-methods}
\docType{methods}
\alias{extract.ANY-method}
\alias{extract.MPMData}
\alias{extract.ESTATICSModel}
\alias{extract.sESTATICSModel}
\alias{extract.qMaps}
\alias{extract.IRdata}
\alias{extract.IRfluid}
\alias{extract.IRmixed}
\alias{[.ANY-method}
\alias{[.MPMData}
\alias{[.ESTATICSModel}
\alias{[.sESTATICSModel}
\alias{[.qMaps}
\alias{[.IRdata}
\alias{[.IRfluid}
\alias{[.IRmixed}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Methods to extract information from objects of class \code{"MPMData"},
\code{"ESTATICSModel"}, \code{"sESTATICSModel"}, \code{"qMaps"},
\code{"IRdata"}, \code{"IRfluid"} and \code{"IRmixed"}.
}
\description{
The extract-methods extract and/or compute specified statistics from object of class
\code{"MPMData"}, \code{"ESTATICSModel"}, \code{"sESTATICSModel"}, \code{"qMaps"},
\code{"IRdata"}, \code{"IRfluid"} and \code{"IRmixed"}.
The [-methods can be used to reduce objects of class
\code{"MPMData"}, \code{"ESTATICSModel"}, \code{"sESTATICSModel"}, \code{"qMaps"},
\code{"IRdata"}, \code{"IRfluid"} and \code{"IRmixed"} such that they contain a subcube of data and results.

}
\usage{
\S3method{extract}{MPMData}(x, what, ...)
\S3method{extract}{ESTATICSModel}(x, what, ...)
\S3method{extract}{sESTATICSModel}(x, what, ...)
\S3method{extract}{qMaps}(x, what, ...)
\S3method{extract}{IRdata}(x, what, ...)
\S3method{extract}{IRfluid}(x, what, ...)
\S3method{extract}{IRmixed}(x, what, ...)
\S3method{[}{MPMData}(x, i, j, k, ...)
\S3method{[}{ESTATICSModel}(x, i, j, k, ...)
\S3method{[}{sESTATICSModel}(x, i, j, k, ...)
\S3method{[}{qMaps}(x, i, j, k, ...)
\S3method{[}{IRdata}(x, i, j, k, tind, ...)
\S3method{[}{IRfluid}(x, i, j, k, ...)
\S3method{[}{IRmixed}(x, i, j, k, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  object of class \code{"MPMData"}, \code{"ESTATICSModel"}, \code{"sESTATICSModel"}
  or \code{"qMaps"}.
}
  \item{what}{
   Character vector of of names of statistics to extract. See Methods Section for details.
}
  \item{i}{
   index vector for first spatial dimension
}
  \item{j}{
   index vector for second spatial dimension
}
  \item{k}{
   index vector for third spatial dimension
}
  \item{tind}{
   index vector for inversion times
}
  \item{\dots}{
  additional parameters, currently unused.
}
}
\section{Methods}{
\describe{
  %\itemize{
  \item{class(x) = "ANY"}{ Returns a warning for \code{extract}}

  \item{class(x) = "MPMData"}{
  Depending the occurence of names in \code{what} a list with the specified components
  is returned
  \itemize{
  \item{"ddata"}{mpm data}
  \item{"sdim" }{ dimension of image cube }
  \item{"nFiles" }{ number of images / image files}
  \item{"t1Files" }{character - filenames of t1Files}
  \item{"pdFiles" }{character - filenames of  pdFiles}
  \item{"mtFiles" }{character - filenames of  mtFiles}
  \item{"model" }{ Number of the ESTATICS model that can be used }
  \item{"maskFile" }{character - filenames of   maskFile}
  \item{"mask" }{ mask}
  \item{"TR" }{ vector of TR values}
  \item{"TE" }{ vector of TE values}
  \item{"FA" }{ vector of FA values}
  }
  }

  \item{class(x) = "ESTATICSModel"}{
  Depending the occurence of names in \code{what} a list with the specified components
  is returned
  \itemize{
  \item{"modelCoeff"}{Estimated parameter maps}
  \item{"invCov" }{ map of inverse covariance matrices}
  \item{"rsigma"}{map of residual standard deviations}
  \item{"isConv" }{ convergence indicator map}
  \item{"isThresh"}{ logical map indicating where \code{R2star==maxR2star}. }
  \item{"sdim" }{ image dimension}
  \item{"nFiles" }{ number of images}
  \item{"t1Files" }{ vector of T1 filenames}
  \item{"pdFiles" }{ vector of PD filenames}
  \item{"mtFiles" }{ vector of MT filenames}
  \item{"model" }{ model used (depends on specification of MT files)}
  \item{"maskFile" }{ filename of brain mask}
  \item{"mask" }{ brain mask}
  \item{"sigma" }{ sigma}
  \item{"L" }{ L}
  \item{"TR" }{ TR values}
  \item{"TE" }{ TE values}
  \item{"FA" }{ Flip angles (FA)}
  \item{"TEScale" }{ TEScale}
  \item{"dataScale" }{dataScale}
  }
 }

  \item{class(x) = "sESTATICSModel"}{
  Depending the occurence of names in \code{what} a list with the specified components
  is returned
  \itemize{
  \item{"modelCoeff"}{Estimated parameter maps}
  \item{"invCov" }{ map of inverse covariance matrices}
  \item{"rsigma"}{ map of residual standard deviations}
  \item{"isConv" }{ convergence indicator map}
  \item{"bi"}{ Sum of weights map from AWS/PAWS}
  \item{"smoothPar"}{ smooting parameters used in AWS/PAWS}
  \item{"smoothedData"}{ smoothed mpmData}
  \item{"isThresh"}{ logical map indicating where \code{R2star==maxR2star}. }
  \item{"sdim" }{ image dimension}
  \item{"nFiles" }{ number of images}
  \item{"t1Files" }{ vector of T1 filenames}
  \item{"pdFiles" }{ vector of PD filenames}
  \item{"mtFiles" }{ vector of MT filenames}
  \item{"model" }{ model used (depends on specification of MT files)}
  \item{"maskFile" }{ filename of brain mask}
  \item{"mask" }{ brain mask}
  \item{"sigma" }{ sigma}
  \item{"L" }{ L}
  \item{"TR" }{ TR values}
  \item{"TE" }{ TE values}
  \item{"FA" }{ Flip angles (FA)}
  \item{"TEScale" }{ TEScale}
  \item{"dataScale" }{dataScale}
  }
}

  \item{class(x) = "qMaps"}{
  Depending the occurence of names in \code{what} a list with the specified components
  is returned
  \itemize{
  \item{b1Map }{ b1Map }
  \item{R1 }{ Estimated map of R1}
  \item{R2star }{ Estimated map of R2star}
  \item{PD }{Estimated map of  PD}
  \item{MT }{Estimated map of  delta (if MT-series was used)}
  \item{model }{Type of ESTATICS model used}
  \item{t1Files }{filenames T1}
  \item{mtFiles }{filenames MT}
  \item{pdFiles }{filenames PD}
  \item{mask }{brainmask}
}
  }
 }
  }
\value{
  A list with components carrying the names of the options specified in
  argument \code{what}.
}
\author{
  Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
  J\"org Polzehl \email{polzehl@wias-berlin.de}
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
#  display some data
#
data <- extract(mpm,"ddata")
if(require(adimpro)){
rimage.options(ylab = "z")
oldpar <- par(mfrow=c(1,3),mar=c(3,3,3,1),mgp=c(2,1,0))
rimage(data[1,,11,], main="first T1w image")
rimage(data[9,,11,], main="first MTw image")
rimage(data[15,,11,], main="first PDw image")
}
#
#  Estimate Parameters in the ESTATICS model
#
modelMPM <- estimateESTATICS(mpm, method = "NLR")
#
#  Parameter maps and residual standard deviation
#
z <- extract(modelMPM,c("rsigma","modelCoeff"))
if(require(adimpro)){
rimage.options(ylab = "z")
par(mfrow=c(1,5),mar=c(3,3,3,1),mgp=c(2,1,0))
rimage(z$modelCoeff[1,,11,], main="S_T1")
rimage(z$modelCoeff[2,,11,], main="S_MT")
rimage(z$modelCoeff[3,,11,], main="S_PD")
rimage(z$modelCoeff[4,,11,], main="R2star")
rimage(z$rsigma[,11,], main="Residual sd")
}
#
#  Compute quantitative maps (R1, R2star, PD, MT)
#
qMRIMaps <- calculateQI(modelMPM,
                        b1File = B1File,
                        TR2 = 3.4)
#
#  resulting quantitative maps for central coronal slice
#
if(require(adimpro)){
rimage.options(zquantiles=c(.01,.99), ylab="z")
par(mfrow=c(2,4),mar=c(3,3,3,1),mgp=c(2,1,0))
nmaps <- c("R1","R2star","PD","MT")
qmap <- extract(qMRIMaps,nmaps)
for (i in 1:4) rimage(qmap[[i]][,11,],main=nmaps[i])
}
par(oldpar)
}
\keyword{ methods }% use one of  RShowDoc("KEYWORDS")
\keyword{ manip }% __ONLY ONE__ keyword per line
