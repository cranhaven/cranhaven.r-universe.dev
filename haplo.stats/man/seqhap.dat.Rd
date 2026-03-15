%  $Author: sinnwell $ 
%  $Date: 2009/04/27 21:22:07 $ 
%  $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/seqhap.dat.Rd,v 1.8 2009/04/27 21:22:07 sinnwell Exp $
%  $Locker:  $ 
%  $Log: seqhap.dat.Rd,v $
%  Revision 1.8  2009/04/27 21:22:07  sinnwell
%  em to emph
%
%  Revision 1.7  2009/04/09 20:25:06  sinnwell
%  add log keyword for RCS
%

\name{seqhap.dat}
\alias{seqhap.dat}
\alias{seqhap.pos}
\title{
  Simulated data for seqhap examples
}
\description{
 Simulated data set for the demonstration of seqhap functionality.
 Contains one column for disease status and columns representing 10 SNP
 loci with a known association.  seqhap.pos contains a column for
 chromosome position, as required by seqhap.
}
\usage{
data(seqhap.dat)
data(seqhap.pos)
}
\format{
    A data frame with 1000 observations on the following 21 variables.
  \describe{
    \item{\code{disease}}{numeric, indicator of disease status 0=no, 1=yes}
    \item{\code{m1.1}}{first allele of genotype}
    \item{\code{m1.2}}{second allele of genotype}
    \item{\code{m2.1}}{first allele of genotype}
    \item{\code{m2.2}}{second allele of genotype}
    \item{\code{m3.1}}{first allele of genotype}
    \item{\code{m3.2}}{second allele of genotype}
    \item{\code{m4.1}}{first allele of genotype}
    \item{\code{m4.2}}{second allele of genotype}
    \item{\code{m5.1}}{first allele of genotype}
    \item{\code{m5.2}}{second allele of genotype}
    \item{\code{m6.1}}{first allele of genotype}
    \item{\code{m6.2}}{second allele of genotype}
    \item{\code{m7.1}}{first allele of genotype}
    \item{\code{m7.2}}{second allele of genotype}
    \item{\code{m8.1}}{first allele of genotype}
    \item{\code{m8.2}}{second allele of genotype}
    \item{\code{m9.1}}{first allele of genotype}
    \item{\code{m9.2}}{second allele of genotype}
    \item{\code{m10.1}}{first allele of genotype}
    \item{\code{m10.2}}{second allele of genotype}
  }
    
}
\references{
 Yu Z, Schaid DJ (2007) Sequantial haplotype scan methods for
association analysis.  \emph{Gen Epi}, in print.
}
\examples{
data(seqhap.dat)
}
\keyword{datasets}
