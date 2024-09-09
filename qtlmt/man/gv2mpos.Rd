\name{misFct}
\alias{gv2mpos}
\alias{xid1ch}
\alias{xeff}
\alias{qeff}
\title{
Miscellaneous functions
}
\description{
Functions that may be useful.
}
\usage{
# create 'mpos' and 'dists'
gv2mpos(gmap,v)

# extract 'xid' on chromosome 'k'
xid1ch(mpos,v,k)

# extract covariate effects of a mtcmim object 'object'
xeff(a,xid)

# extract QTL effect from a mtcmim object 'object'
qeff(object)
}

\arguments{
  \item{gmap}{
a genetic map, which is a data frame (chr=chromosome id, dist=genetic distance (cM) on the chromosome,...). The chromosome id should be an integer.
}
  \item{v}{
a list; v[[j]] indicates which x's in the model for y[,j].
}
  \item{mpos}{
a data frame (id=marker index, ch=chromosome id, m=marker index on the chromosome, dist=genetic position in cM on the chromosome). Chromosome id should be an integer.
}
  \item{k}{
which chromosome?
}
  \item{a}{
covariate effects of an \code{\link{mtcmim}} object.
}
  \item{xid}{
a list of length p, xid[[j]] specifies columns of x as covariates for y[,j] .
}
  \item{object}{
an \code{\link{mtcmim}} object.
}
}

\seealso{
\code{\link{mtcmim}} and \code{\link{sureEst}}.
}

