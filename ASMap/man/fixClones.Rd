\name{fixClones}
\alias{fixClones}
\title{
Consensus genotypes for clonal genotype groups
}
\description{
Consensus genotypes for clonal genotype groups of an R/qtl object.
}
\usage{
fixClones(object, gc, id = "Genotype", consensus = TRUE)
}
\arguments{
  \item{object}{
An \pkg{qtl} \code{object} object with any class structure.
}
\item{gc}{
A data frame of genotype clone infomation usually from a call to
\code{genClones} (see Details).
}
\item{id}{
Character string defining the column of \code{object$pheno} containing
the genotype names.
}
\item{consensus}{
A logical value. If \code{TRUE} then consensus genotypes will be
calculated for each clonal group by intelligently collapsing alleles for each marker
(see Details). If \code{FALSE} then for each clonal group the genotype with the least missing
alleles across the genome will be retained and the remaining genotypes
from each group will be removed.
}
}
\details{
This function provides a very efficient way of dealing with genotype
clones in a genetic marker set. This function can be used at any stage
of the map construction process as it retains linkage group and marker
position information.

The \code{gc} argument needs to be a data frame of clone
information and is easily obtained from a call to \code{genClones}. If
this function is not used then the data frame must contain at least three
columns with the first two columns named \code{"G1"} and \code{"G2"}
containing the pairs of genotypes that are clones and a \code{"group"}
column that indicates the clonal group the pairs of genotypes belongs to.

If \code{consensus = TRUE} then the function will intelligently collapse
the alleles for each marker to form a consensus genotype. Specifically, the allele
value will remain unchanged when there are observed allele values
across all genotypes in the clone group. For cases where there are
missing alleles for some but not all of the
genotypes, the consensus genotype will be given the common allele value
from the genotypes that contained observed allele values. If there is
more than one unique allele value across the genotypes for any marker
then it is set to missing.
}
\value{
The cross object is returned with identical class structure as the
imputted cross object.
}
\references{
Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
Linkage Map Construction and Diagnosis. Journal of Statistical Software,
\bold{79}(6), 1--29.
}
\author{
Julian Taylor
}
\seealso{
\code{\link[qtl]{comparegeno}} and \code{\link{genClones}}
}
\examples{

data(mapDH, package = "ASMap")

gc <- genClones(mapDH)
mapDHf <- fixClones(mapDH, gc$cgd, consensus = TRUE)

}
\keyword{misc}
