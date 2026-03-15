% $Author: sinnwell $ 
% $Date: 2011/02/21 19:08:18 $ 
% $Header: /projects/genetics/cvs/cvsroot/mgenet/man/geno1to2.Rd,v 1.1.1.1 2011/02/21 19:08:18 sinnwell Exp $ 
% $Locker:  $ 
% 
%$Log: geno1to2.Rd,v $
%Revision 1.1.1.1  2011/02/21 19:08:18  sinnwell
%"initial, general functions for R genetics at Mayo BSI
%
%Revision 1.4  2009/09/30 20:05:38  sinnwell
%rm keywords, or leave one with one from /doc/KEYWORDS
%
%Revision 1.3  2009/09/30 19:19:23  sinnwell
%remove empty sections
%
%Revision 1.2  2008/01/08 20:25:48  sinnwell
%change comments
%
%Revision 1.1  2008/01/08 19:54:32  sinnwell
%Initial revision
%
%Revision 1.5  2007/04/24 16:27:09  sinnwell
%rm link to unlistToMat, no longer used
%
%Revision 1.4  2007/04/20 15:08:59  sinnwell
%rm na.code, update example
%
%Revision 1.3  2007/04/18 17:16:17  sinnwell
%add locus.label arg
%
%Revision 1.2  2007/04/16 20:13:05  sinnwell
%*** empty log message ***
%
%Revision 1.1  2006/08/11 21:53:53  sinnwell
%Initial revision

\name{geno1to2}
\alias{geno1to2}
\title{
  convert genotype matrix from 1-column 2-column 
}
\description{
convert 1-column genotype matrix to 2-column genotype matrix,
converting from a minor allele count (0,1,2) to (1/1, 1/2, 2/2) where 2 is the
minor allele. (not supported for x-linked markers)
}
\usage{
geno1to2(geno, locus.label=NULL)
}
\arguments{
\item{geno}{
1-column representation of genotype matrix for 2-allele loci.  Values
are 0, 1, or 2, usually the count of minor alleles
}
\item{locus.label }{
Vector of labels for loci, If a locus name is "A", its columns
will be "A.1" and "A.2"
}
}
\value{
a 2-column genotype matrix
}
\examples{
geno1 <- matrix(c(0,0,1,
                  1,0,2,
                  2,1,0), ncol=3, byrow=TRUE)
geno1to2(geno1, locus.label=c("A", "B", "C"))

## demonstrate how NA and 3 will be coded
geno1[1,3] <- NA
geno1[1,1] <- 3
geno1to2(geno1)
}
\keyword{utilities}
% docclass is function
% Converted by Sd2Rd version 37351.
