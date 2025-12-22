\name{GetCandReg}
\alias{GetCandReg}
\alias{GetCisCandReg}
\alias{GetCoMappingTraits}
\title{
Get genetic information on candidate regulators and co-mapping traits.
}
\description{
Get chromosome (phys.chr) and physical position in cM (phys.pos), along
with the LOD score (peak.lod) at the peak position (peak.pos), and the
chromosome where the peak is located (peak.chr). Some candidates may map to the
same chromosome where they are physically located.
}
\usage{
GetCandReg(highobj, annot, traits)
GetCisCandReg(highobj, cand.reg, lod.thr = NULL)
GetCoMappingTraits(highobj, cand.reg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{highobj}{
data frame from \code{\link{highlod}}, which is sparse summary of
high LODs in large \code{\link[qtl]{scanone}} object
}
  \item{annot}{
data frame with annotation information; must have first column as unique
identifier, third column as chromosome, and fifth column as position in
cM; typically column 2 has gene name, and column 4 has position in Mb
}
  \item{traits}{
names of traits to examine as candidate regulators; names must
correspond to phenotypes in \code{cross} object
}
  \item{cand.reg}{
data frame with candidate regulator; see value section below
}
\item{lod.thr}{LOD threshold; restrict to intervals above this value if
  not \code{NULL}}
}
\details{
Traits that map to positions close to their physical locations are said
to map in cis (local linkages). 
Traits that map to positions away from their physical locations are said to map in
trans (distal linkages). There is no unambiguous way to determine how close a trait needs to
map to its physical location in order to be classified as cis. Our choice is to classify a trait as
cis if the 1.5-LOD support interval (Manichaikul et al. 2006) around the LOD peak contains
the trait's physical location, and if the LOD score at its physical location is higher the the LOD
threshold. The function \code{GetCisCandReg} determines which of the candidate regulators map in
cis. The function \code{GetCoMappingTraits} returns a list with the putative
targets of each trait. A trait is included in the putative target list of
a trait when its LOD peak is greater than \code{lod.thr} and the
\code{drop} LOD support interval around the peak contains the location
of the trait's QTL.
The function \code{JoinTestOutputs} currently relies on external files
that contain results of \code{\link{FitAllTests}}. It needs to be
rewritten to save space.
}
\value{
  \code{GetCoMappingTraits} returns a list with each element being the
  names of co-mapping traits for a particular name in \code{traits}.
  \code{GetCandReg} returns a data frame while \code{GetCisCandReg}
  returns a list with a similar candidate regulator data frame as the
  element \code{cis.reg}, and the index of trait names as the element
  \code{cis.index}. The elements of the candidate regulator data frame
  are as follows (\code{peak.pos.lower} and \code{peak.pos.upper} only
  for \code{GetCisCandReg}):
  \item{gene}{name of trait, which might be a gene name}
  \item{phys.chr}{chromosome on which gene physically resides}
  \item{phys.pos}{physical position (in cM)}
  \item{peak.chr}{chromosome where peak LOD is located}
  \item{peak.pos}{position of peak (in cM)}
  \item{peak.lod}{LOD value at peak}
  \item{peak.pos.lower,peak.pos.upper}{lower and upper bounds
    of the 1.5-LOD support interval around \code{peak.pos}}
}
\references{
Manichaikul et al. (2006) Genetics
}
\author{
Elias Chaibub Neto
}
\seealso{
  \code{\link{highlod}},
  \code{\link{FitAllTests}},
  \code{\link[qtl]{scanone}}
}
\examples{
## data(CMSTCross) is loaded lazily.
CMSTscan <- scanone(CMSTCross, pheno.col = 1:3, method = "hk")
CMSThigh <- highlod(CMSTscan)
traits <- names(CMSTCross$pheno)
annot <- data.frame(name = traits, traits = traits, chr = rep(1, 3),
 Mb.pos = c(55,10,100))
annot$cM.pos <- annot$Mb.pos
cand.reg <- GetCandReg(CMSThigh, annot, traits)
cis.cand.reg <- GetCisCandReg(CMSThigh, cand.reg)
comap.targets <- GetCoMappingTraits(CMSThigh, cand.reg)
}
\keyword{utilities}
