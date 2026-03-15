% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/na.geno.keep.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% $Log: na.geno.keep.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/09 19:46:11  sinnwell
% Initial revision
%


\name{na.geno.keep}
\alias{na.geno.keep}
\title{
  Remove rows with NA in covariates, but keep genotypes with NAs
}
\description{
  Removes rows with NA in response or covariates, but keeps subjects
  with NAs in their genotypes if not missing all alleles.
}
\usage{
na.geno.keep(m)
}
\arguments{
\item{m}{
model matrix
}
}
\value{
  a model matrix with rows removed if exclusion criteria requires it
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
