% $Author: sinnwell $ 
% $Date: 2009/09/30 20:05:38 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.model.frame.Rd,v 1.4 2009/09/30 20:05:38 sinnwell Exp $ 
% $Locker:  $ 
% $Log: haplo.model.frame.Rd,v $
% Revision 1.4  2009/09/30 20:05:38  sinnwell
% rm keywords, or leave one with one from /doc/KEYWORDS
%
% Revision 1.3  2009/09/30 19:25:04  sinnwell
% remove empty sections
%
% Revision 1.2  2008/04/29 17:20:59  sinnwell
% rm allele.lev and miss.val
%
% Revision 1.1  2008/01/09 17:38:24  sinnwell
% Initial revision
%
%Revision 1.3  2004/03/03 22:14:57  schaid
%added allele.lev to work in R with character alleles
%
%Revision 1.2  2004/02/26 22:46:30  sinnwell
%remove example
%
%Revision 1.1  2004/02/26 22:17:14  sinnwell
%Initial revision


\name{haplo.model.frame}
\alias{haplo.model.frame}
\title{
  Sets up a model frame for haplo.glm
}
\description{
For internal use within the haplo.stats library
}
\usage{
haplo.model.frame(m, locus.label=NA, control=haplo.glm.control())
}
\arguments{

\item{m}{
model.frame from evaluated formula
}
\item{locus.label }{
labels for loci in genotype matrix
}
\item{control}{
control parameters for haplo.glm
}
}
\value{
  A model frame with haplotypes modeled as effects
}
\details{
  See haplo.glm description in help file and user manual
}
\keyword{models}
% docclass is function
% Converted by Sd2Rd version 37351.
