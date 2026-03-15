% $Author: sinnwell $
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.em.fitter.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $
% $Locker:  $ 
% $Log: haplo.em.fitter.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/08 22:28:36  sinnwell
% Initial revision
%
%Revision 1.2  2004/02/26 22:30:08  sinnwell
%remove example
%
%Revision 1.1  2004

\name{haplo.em.fitter}
\alias{haplo.em.fitter}
\title{
  Compute engine for haplotype EM algorithm 
}
\description{
For internal use within the haplo.stats library
}
\usage{
haplo.em.fitter(n.loci, n.subject, weight, geno.vec, n.alleles,
                max.haps, max.iter, loci.insert.order, min.posterior, 
                tol, insert.batch.size, random.start, iseed1, iseed2, 
                iseed3, verbose)
}
\arguments{
\item{n.loci }{
number of loci in genotype matrix
}
\item{n.subject }{
number of subjects in the sample
}
\item{weight}{
numeric weights
}
\item{geno.vec }{
vectorized genotype matrix
}
\item{n.alleles }{
numeric vector giving number of alleles at each marker
}
\item{max.haps }{
maximum unique haplotypes in the sample
}
\item{max.iter }{
maximum iterations to perform in the fitter
}
\item{loci.insert.order }{
order to insert loci for progressive insertion
}
\item{min.posterior }{
  after insertion and maximization, discard haplotype pairs per person
  that do not meet minimum posterior prob
}
\item{tol}{
  convergence tolerance for E-M steps
}
\item{insert.batch.size }{
  number of markers to insert per batch
}
\item{random.start}{
  logical; if TRUE, allow for random starting values of haplotype frequencies 
}
\item{iseed1}{
random seed for algorithm
}
\item{iseed2}{
random seed for algorithm
}
\item{iseed3}{
random seed for algorithm
}
\item{verbose}{
logical, print long, verbose output from E-M steps?
}
}
\details{
For internal use within the haplo.stats library
}
% docclass is function
% Converted by Sd2Rd version 37351.
