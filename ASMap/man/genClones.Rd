\name{genClones}
\alias{genClones}
\title{
Find and report genotype clones
}
\description{
Find and report genotype clones for \pkg{qtl} objects.
}
\usage{
genClones(object, chr, tol = 0.9, id = "Genotype")
}
\arguments{
  \item{object}{
An \pkg{qtl} \code{object} object with any class structure.
}
\item{chr}{
A character string of linkage group names.
}
\item{tol}{
Pairs of genotypes with a proporion of matching alleles above this
tolerance will be returned.
}
\item{id}{
Character string defining the column of \code{object$pheno} containing
the genotype names.
}
}
\details{
This function extends the functionality of \code{comparegeno} in the
\pkg{qtl} package by providing breakdown statistics for the pairs of
genotypes that have a proportion of matching alleles above \code{tol}.
}
\value{
A list is returned with the matrix from \code{comparegeno} as an element
\code{cgm} and the breakdown statistics for returned genotype pairs in
\code{cgd}. Specifically, the statistics contain a \code{"group"} column
which determines the clonal group the pair of genotypes belongs to.
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
\code{\link[qtl]{comparegeno}} and \code{\link{fixClones}}
}
\examples{

data(mapDH, package = "ASMap")

gc <- genClones(mapDH)

}
\keyword{misc}
