% $Author: sinnwell $
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.design.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% $Log: 

\name{haplo.design}
\alias{haplo.design}
\title{
  Build a design matrix for haplotypes
}
\description{
Build a design matrix for haplotypes estimated from a haplo.em object. 
}
\usage{
haplo.design(obj, haplo.effect="additive", hapcodes=NA, min.count=5, haplo.base=NA)
}
\arguments{
\item{obj}{
an object created from haplo.em
}
\item{haplo.effect }{
The "effect" pattern of haplotypes on the response. This
parameter determines the coding for scoring the haplotypes. 
Valid coding options for heterozygous and homozygous carriers
of a haplotype are "additive" (1, 2, respectively),
"dominant" (1,1, respectively), and "recessive" (0, 1,
respectively). 
}
\item{hapcodes}{
codes assigned in haplo.em, corresponding to the row numbers in the
haplotypes matrix item in \code{obj}
}
\item{min.count }{
The minimum number of estimated counts of the haplotype in the sample
in order for a haplotype to be included in the design matrix.  
}
\item{haplo.base }{
code for which haplotype will be the reference group, or to be
considered the baseline of a model.  The code is the row number of the
\code{haplotypes} matrix in \code{obj}. This haplotype is removed from
the design matrix.
}
}
\value{
Matrix of columns for haplotype effects. Column names are "hap.k"
where k is the row number of the unique haplotypes within the haplo.em
object's "haplotypes" item. 
}
\details{
First a matrix is made for the possible haplotypes for each
person, coded for the haplo.effect, weighted by the posterior
probability of those possible haplotypes per person, and then 
collapsed back to a single row per person.
}
\seealso{
\code{\link{haplo.em}}
}
\examples{

 ###------------------------------------------------
 ### See the user manual for more complete examples
 ###------------------------------------------------

 data(hla.demo)
 attach(hla.demo)

 geno <- hla.demo[,c(17,18,21:24)]
 label <-c("DQB","DRB","B")

 keep <- !apply(is.na(geno) | geno==0, 1, any)

 save.em.keep <- haplo.em(geno=geno[keep,], locus.label=label)

 save.df <- haplo.design(save.em.keep, min.count=10)
 dim(save.df)

 names(save.df)
 save.df[1:10,]


}
\keyword{models}
% docclass is function
% Converted by Sd2Rd version 37351.
