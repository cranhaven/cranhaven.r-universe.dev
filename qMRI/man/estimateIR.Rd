\name{estimateIR}
\alias{estimateIR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate IRMRI parameters
}
\description{
Parameter estimation ( intensity, relaxation rate, proportion of fluid) in Inversion Recovery MRI data.
}
\usage{
estimateIR(IRdataobj, TEScale = 100, dataScale = 1000, method = c("NLR", "QL"),
           varest = c("RSS","data"), fixed = TRUE, smoothMethod=c("PAWS","Depth"),
           kstar = 24, alpha = .025, bysegment = TRUE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{IRdataobj}{
4D array of IRMRI signals. First dimension corresponds to Inversion times (InvTime).
}
  \item{TEScale}{
 Internal scale factor for Echo Times. This influences parameter scales in numerical calculations.

}
  \item{dataScale}{
 Internal scale factor for MR signals. This influences parameter scales in numerical calculations.
}
  \item{method}{
  Either \code{"NLS"} for nonlinear least squares (ignores Rician bias) or \code{"QL"} for Quasi-Likelihood. The second option is more accurate but requires additional information and is computationally more expensive.
}
  \item{varest}{
  Method to, in case of \code{method="QR"}, estimate \code{sigma}if not provided. Either from residual sums of squares (\code{"RSS"}) or MR signals (\code{"data"}) using function \code{varest} from package {\code{aws}}. Only to be used in case that no image registration was needed as preprocessing.
}
\item{fixed}{
 Should adaptive smoothing performed for Sx and Rx maps and fx maps reestimated afterwards ?
}
  \item{smoothMethod}{
 Either "PAWS" or "Depth". the second option is not yet implemented.
}

  \item{kstar}{
  number of steps used in PAWS
}
  \item{alpha}{
   significance level for decisions in aws algorithm (suggestion: between 1e-5 and 0.025)
}
   \item{bysegment}{
   \code{TRUE}: restrict smoothing to segments from segmentation, \code{FALSE}: restrict smoothing to solid mask.
}
  \item{verbose}{
Logical. Provide some runtime diagnostics.
}
}
\details{
This function implements the complete pipeline of IRMRI anlysis.
}
\value{
List of class \code{"IRmixed"} with components
\item{IRdata }{4D array containing the IRMRI data, first dimension refers to inversion times}
\item{InvTimes }{vector of inversion times}
\item{segm}{segmentation codes, 1 for CSF, 2 for GM, 3 for WM, 0 for out of brain}
\item{sigma}{noise standard deviation, if not specified estimated fron CSF areas in image with largest inversion time}
\item{L}{effective number of coils}
\item{fx}{Array of fluid proportions}
\item{Sx}{Array of maximal signals}
\item{Rx}{Array of relaxation rates}
\item{Sf}{Global estimate of maximal fluid signal}
\item{Rf}{Global estimate of fluid relaxation rate}
\item{ICovx}{Covariance matrix of estimates \code{fx}, \code{Sx} and \code{Rx}.}
\item{sigma}{Array of provided or estimated noise standard deviations }
\item{Convx}{Array of convergence indicators}
\item{rsdx}{Residual standard deviations}
The arrays contain entries for all voxel with \code{segments\%in\%1:3}.
}
\author{
Karsten Tabelow \email{tabelow@wias-berlin.de}\cr
J\"org Polzehl \email{polzehl@wias-berlin.de}

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
function (IRdata, InvTimes, segments, fixed = TRUE, smoothMethod = c("Depth", 
    "PAWS"), bw = 5, TEScale = 100, dataScale = 1000, method = c("NLR", 
    "QL"), sigma = NULL, L = 1, maxR2star = 50, varest = c("RSS", 
    "data"), verbose = TRUE) 
{
    ergsFluid <- estimateIRfluid(IRdata, InvTimes, segments)
    Sfluid <- median(ergsFluid$Sfluid)
    Rfluid <- median(ergsFluid$Rfluid)
    ergsBrain <- erstimateIRsolid(IRdata, InvTimes, segments, 
        Sfluid, Rfluid)
    if (fixed) {
        if (smmothMethod == "Depth") stop("not yet implemented")
#            ergsSmooth <- SdepthSmooth(ergsBrain, segments)
        if (smmothMethod == "PAWS") 
            ergsSmooth <- vpawscov2(ergsBrain, segments)
    }
    ergsSmooth
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{ models }
 \keyword{ regression }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
 \concept{ IRMRI }
% Use only one concept per line.
