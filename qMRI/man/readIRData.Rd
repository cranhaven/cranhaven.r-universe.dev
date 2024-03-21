\name{readIRData}
\alias{readIRData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Prepare IRMRI dataset
}
\description{
The function reads IRMRI images given as NIfTI files in t1Files, inversion times and segmentation image(s) aund prepares an object class \code{"IRdata"}
}
\usage{
readIRData(t1Files, InvTimes, segmFile, sigma = NULL, L = 1,
           segmCodes = c("GM", "WM", "CSF"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t1Files}{
   Names of NIfTI files containing the recorded images.
}
  \item{InvTimes}{
   Corresponding inversion times
}
  \item{segmFile}{
   Either a NIfTI file containing a segmentation into GM, WM and CSF or
   three files containing probability maps for GM, WM and CSF
}
  \item{sigma}{
   Noise standard deviation
}
  \item{L}{
   Effective number of coils, L=1 assumes a Rician signal distribution
}
  \item{segmCodes}{
   sequence of tissue code in segmFile
}
}
\value{
A list of class \code{"IRdata"} with components
\item{IRdata }{4D array containing the IRMRI data, first dimension refers to inversion times}
\item{InvTimes }{vector of inversion times}
\item{segm}{segmentation codes, 1 for CSF, 2 for GM, 3 for WM, 0 for out of brain}
\item{sigma}{noise standard deviation, if not specified estimated fron CSF areas in image with largest inversion time}
\item{L}{effective number of coils}
}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{estimateIRfluid}}, \code{\link{estimateIRsolid}}, \code{\link{estimateIR}},\code{\link{smoothIRSolid}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (t1Files, InvTimes, segmFile, sigma = NULL, L = 1, segmCodes = c("GM", 
    "WM", "CSF")) 
{
    if (is.null(t1Files)) 
        stop("vector of T1 files required")
    nFiles <- length(t1Files)
    if (length(InvTimes) != nFiles) 
        stop("readIRData: t1Files and InvTimes have different lengths")
    sdim <- dim(readNIfTI(t1Files[1], read_data = FALSE))
    s1 <- (1:3)[segmCodes == "CSF"]
    s2 <- (1:3)[segmCodes == "GM"]
    s3 <- (1:3)[segmCodes == "WM"]
    segm <- c1 <- readNIfTI(segmFile[1])@.Data
    if (length(segmFile) == 1) {
        segm[c1 == s1] <- 1
        segm[c1 == s2] <- 2
        segm[c1 == s3] <- 3
    }
    else if (length(segmFile) == 3) {
        c2 <- readNIfTI(segmFile[2], reorient = FALSE)
        c3 <- readNIfTI(segmFile[3], reorient = FALSE)
        segm[c1 >= pmax(1/3, c1, c2, c3)] <- s2
        segm[c2 >= pmax(1/3, c1, c2, c3)] <- s3
        segm[c3 >= pmax(1/3, c1, c2, c3)] <- s1
    }
    if (any(dim(segm) != sdim)) 
        stop("readIRData: dimensions of t1Files and segmFiles are incompatible")
    IRdata <- array(0, c(nfiles, sdim))
    for (i in 1:nfiles) IRdata[i, , , ] <- readNIfTI(t1Files[i], 
        reorient = FALSE)
    InvTimes[is.inf(InvTimes)] <- 10 * max(InvTimes[!is.inf(InvTimes)])
    if (is.null(sigma)) {
        ind <- (InvTimes == max(InvTimes))[1]
        ddata <- IRdata[ind, , , ]
        shat <- awsLocalSigma(ddata, steps = 16, mask = (segm == 
            1), ncoils = L, hsig = 2.5, lambda = 6, family = "Gauss")$sigma
        dim(shat) <- sdim
        shat <- shat[segm == 1]
        sigma <- median(shat)
    }
    data <- list(IRimg = IRdata, InvTimes = InvTimes, segm, sigma = sigma, 
        L = 1)
    class(data) <- "IRdata"
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ utililities }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
 \concept{ IRMRI }
% \concept{ ~cpt2 }
% Use only one concept per line.
