\name{breakCross}
\alias{breakCross}
\title{
Break linkage groups of an \pkg{qtl} cross object
}
\description{
Breaks linkage groups of an \pkg{qtl} cross object from a user specified
list.
}
\usage{
breakCross(cross, split = NULL, suffix = "numeric", sep = ".")
}
\arguments{
  \item{cross}{
An \pkg{qtl} \code{cross} object with any class structure.
}
\item{split}{
A list named by the linkage groups required for splitting and containing
marker names immediately preceding where the splits are to be made (see
Details).
}
\item{suffix}{
This can be a vector of character strings containing \code{"numeric"} or
\code{"alpha"} specifying whether integers or letters are to be appended to the old linkage group
names to form new names. This argument may also be list with elements named by the
linkage groups that are in \code{split} and containing the new names for
the split linkage groups (see Examples).
}
\item{sep}{
The character separator to be used to separate the linkage group name
and the suffix.
}
}
\details{
The splitting of any linkage group only needs to be defined by the
markers immediately preceding where the splits are to be made. Multiple
splits in the one linkage group are possible as well as splitting across
multiple linkage groups with one call.
}
\value{
The cross object is returned with identical class structure as the
inputted cross object. The \code{"geno"} element will contain
separate linkage groups for the user defined splits.
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
\code{\link{mergeCross}}
}
\examples{

data(mapDH, package = "ASMap")

mapDH1 <- breakCross(mapDH, split = list("4A" = "4A.m.8"))
pull.map(mapDH1)[["4A.1"]]
pull.map(mapDH1)[["4A.2"]]

## manually choose suffix

mapDH1 <- breakCross(mapDH, split = list("4A" = "4A.m.8"),
                     suffix = list("4A" = c("4AA","4AB")))

}
\keyword{misc}
