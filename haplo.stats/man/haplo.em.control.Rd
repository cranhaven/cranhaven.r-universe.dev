% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.em.control.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $
% $Locker:  $ 
% $Log: haplo.em.control.Rd,v $
% Revision 1.4  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.3  2008/04/15 20:13:52  sinnwell
%  fix a few words in min.post and max.haps.limit
%
% Revision 1.2  2008/02/05 16:31:58  sinnwell
% change default for min.posterior
%
% Revision 1.1  2008/01/08 22:28:27  sinnwell
% Initial revision
%
%Revision 1.8  2007/04/16 20:42:05  sinnwell
%remove max.haps.limit
%
%Revision 1.7  2005/03/31 16:39:15  sinnwell
%code-doc agree on max.iter
%
%Revision 1.6  2004/12/29 16:34:23  sinnwell
%explain tol and max.iter better
%
%Revision 1.5  2004/03/01 21:16:43  sinnwell
%comment out example
%
%Revision 1.4  2004/02/05 22:27:20  sinnwell
%fix usage again, remove '<-function'
%
%Revision 1.3  2004/02/05 17:28:20  schaid
%set defaults to new values
%Revision 1.2  2003/08/26 17:57:06  schaid
%edits by Dan Schaid .
%
%Revision 1.1  2003/08/22 20:23:05  sinnwell
%Initial revision

\name{haplo.em.control}
\alias{haplo.em.control}
\title{
  Create the Control Parameters for the EM Computation of Haplotype 
Probabilities, with Progressive Insertion of Loci
}
\description{
Create a list of parameters that control the EM algorithm for 
estimating haplotype frequencies, based on progressive insertion 
of loci. Non-default parameters for the EM algorithm can be
set as parameters passed to haplo.em.control.
}
\usage{
haplo.em.control(loci.insert.order=NULL, insert.batch.size = 6,
                             min.posterior = 1e-09, tol = 1e-05,
                             max.iter=5000, random.start=0, n.try = 10,
                             iseed=NULL, max.haps.limit=2e6, verbose=0)
}
\arguments{

\item{loci.insert.order }{
Numeric vector with specific order to insert the loci. If this value
is NULL, the insert order will be in sequential order (1, 2, ..., No. Loci).
}
\item{insert.batch.size }{
Number of loci to be inserted in a single batch.
}
\item{min.posterior }{
Minimum posterior probability of a haplotype pair, conditional on
observed marker genotypes. Posteriors below this minimum value will
have their pair of haplotypes "trimmed" off the list of possible
pairs.  If all markers in low LD, we recommend using the default.
If markers have at least moderate LD, can increase this value to use
less memory.
}
\item{tol}{
If the change in log-likelihood value between EM steps is less 
than the tolerance (tol), it has converged.
}
\item{max.iter }{
Maximum number of iterations allowed for the EM algorithm before it
stops and prints an error.  If the error is printed, double max.iter.
}
\item{random.start }{
If random.start = 0, then the inititial starting values of the
posteriors for the first  EM attempt will be based on assuming equal
posterior probabilities (conditional on genotypes). If random.start = 1,
then the initial starting values of the first EM attempt will be based on assuming a uniform distribution for
the initial posterior probabilities.
}
\item{n.try }{
Number of times to try to maximize the lnlike by the EM algorithm. The
first try uses, as initial starting values for the posteriors,
either equal values or uniform random variables, as determined by
random.start. All subsequent tries will use random uniform values as
initial starting values for the posterior probabilities.
}
\item{iseed}{
An integer or a saved copy of .Random.seed. This allows simulations to
be reproduced by using the same initial seed.
}
\item{max.haps.limit }{
Maximum number of haplotypes for the input genotypes.  It is used as
the amount of memory to allocate in C for the progressive-insertion
E-M steps.  Within haplo.em, the first step is to try to allocate the
sum of the result of geno.count.pairs(), if that exceeds
max.haps.limit, start by allocating max.haps.limit.  If that is
exceeded in the progressive-insertions steps, the C function doubles
the memory until it can no longer request more. 
}
\item{verbose}{
Logical, if TRUE, print procedural messages to the screen.  If FALSE, 
do not print any messages. 
}
}
\value{
A list of the parameters passed to the function.
}
\details{
The default is to use n.try = 10. If this takes too much time, it may
be worthwhile to decrease n.try. Other tips for computing haplotype
frequencies for a large number of loci, particularly if some have many
alleles, is to decrease the batch size (insert.batch.size), increase
the memory (max.haps.limit), and increase the probability of trimming
off rare haplotypes at each insertion step (min.posterior).
}
\seealso{
\code{\link{haplo.em}},
\code{\link{haplo.score}}
}
\examples{
# This is how it is used within haplo.score
#    > score.gauss <- haplo.score(resp, geno, trait.type="gaussian", 
#    >           em.control=haplo.em.control(insert.batch.size = 2, n.try=1))
}

