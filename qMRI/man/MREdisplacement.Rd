\name{MREdisplacement}
\alias{MREdisplacement}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the motion induced signal phase for IR-MRE in biphasic material 
}
\description{
The function takes magnitude images and phase images (as NIfTI files) recordet with inversion \code{IT1=Inf} 
and a second inversion time \code{IT2} that nulls the fluid signal. Tissue parameters (Relaxation rates)
are extracted from an object of class \code{"IRmixed"} calculated from data of a related IRMRI experiment. }
\usage{
MREdisplacement(MagnFiles1, PhaseFiles1, MagnFiles2, PhaseFiles2, TI2 = 2400,
                IRmixobj, method = c("full", "approx"),rescale=FALSE,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MagnFiles1}{
  Filenames of magnitude images recorded with inversion time \code{IT=Inf} .
}
  \item{PhaseFiles1}{
  Filenames of phase images recorded with inversion time \code{IT=Inf} .
%%     ~~Describe \code{PhaseFiles1} here~~
}
  \item{MagnFiles2}{
    Filenames of magnitude images recorded with inversion time \code{IT=IT2}.
}
  \item{PhaseFiles2}{
Filenames of phase images recorded with inversion time \code{IT=IT2} .
}
  \item{TI2}{
Inversion time used for \code{MagnFiles2} and \code{PhaseFiles2}. \code{IT2} should be selected to
extinguish the signal intendity for fluid.
}
  \item{IRmixobj}{
Object of class \code{"IRmixed"} obtained from a related IRMRI experiment.
}
  \item{method}{
Either \code{"full"} or \code{"approx"}
}
  \item{rescale}{
Logical, do we need to rescale phase images ? 
}
  \item{verbose}{
 Report scale range of phase images
}
}
\details{
The first 4 arguments need to be vectors of filenames of identical length with files containing compatible 3D NIfTI images. Object \code{IRmixobj} needs to contain a components \code{segm} and \code{Rx} of compatible dimension that need to be registered to the MRE images.
}
\value{
A list of class \code{"IRMREbiphasic"} with components
\item{phisolid}{displacement solid}
\item{phifluid}{displacement fluid}
}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{estimateIRfluid}}, \code{\link{estimateIRsolid}}, \code{\link{estimateIRsolidfixed}},\code{\link{smoothIRSolid}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (MagnFiles1, PhaseFiles1, MagnFiles2, PhaseFiles2, TI2 = 2400, 
    IRmixobj, method = c("full", "approx"), verbose = FALSE) 
{
    segm <- IRmixobj
    sdim <- dim(segm)
    nfiles <- length(MagnFiles1)
    if (length(PhaseFiles1) != nfiles || length(MagnFiles2) != 
        nfiles || length(PhaseFiles2) != nfiles) {
        stop("Incompatible lengths of filelists")
    }
    imgdim1 <- readNIfTI(MagnFiles1, read_data = FALSE)@dim_[2:4]
    imgdim2 <- readNIfTI(MagnFiles2, read_data = FALSE)@dim_[2:4]
    imgdim3 <- readNIfTI(PhaseFiles1, read_data = FALSE)@dim_[2:4]
    imgdim4 <- readNIfTI(PhaseFiles2, read_data = FALSE)@dim_[2:4]
    if (any(imgdim1 != sdim) || any(imgdim2 != sdim) || any(imgdim3 != 
        sdim) || any(imgdim4 != sdim)) {
        stop("Incompatible image dimensions")
    }
    Mimg1 <- Mimg2 <- phiimg1 <- phiimg2 <- array(0, c(sdim, 
        nfiles))
    for (i in 1:nfiles) {
        Mimg1[, , , i] <- readNIfTI(MagnFiles1, reorient = FALSE)@.Data
        phiimg1[, , , i] <- readNIfTI(PhaseFiles1, reorient = FALSE, 
            rescale = FALSE)@.Data
        Mimg2[, , , i] <- readNIfTI(MagnFiles2, reorient = FALSE)@.Data
        phiimg2[, , , i] <- readNIfTI(PhaseFiles2, reorient = FALSE, 
            rescale = FALSE)@.Data
        rngimg <- range(phiimg1, phiimg2)
        cat("range of phase images", rngimg, "\n")
        phiimg1 <- phiimg1/max(abs(rngimg)) * pi
        phiimg2 <- phiimg2/max(abs(rngimg)) * pi
    }
    Rf <- IRmixobj$Rf
    R1x <- IRmixobj$Rx
    masksolid <- segm > 1
    CCs <- array(1 - 2 * exp(-TI2 * R1x), dim(Mimg1))
    CCf <- array(1 - 2 * exp(-TI2 * Rf), dim(Mimg1))
    masks <- array(masksolid[, , 2:6], dim(Mimg1))
    if ("full" \%in\% method) {
        ss <- CCf * Mimg1 * sin(phiimg1) - Mimg2 * sin(phiimg2)
        cs <- CCf * Mimg1 * cos(phiimg1) - Mimg2 * cos(phiimg2)
    }
    else {
        ss <- -Mimg2 * sin(phiimg2)
        cs <- -Mimg2 * cos(phiimg2)
    }
    sf <- -CCs * Mimg1 * sin(phiimg1) + Mimg2 * sin(phiimg2)
    cf <- -CCs * Mimg1 * cos(phiimg1) + Mimg2 * cos(phiimg2)
    phis <- atan2(cs, ss)
    phif <- atan2(cf, sf)
    phis[!masks] <- 0
    phif[!masks] <- 0
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ IR-MRE }
% \concept{ ~cpt2 }
% Use only one concept per line.
