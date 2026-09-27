% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/summary.haplo.glm.Rd,v 1.1 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% $Log: summary.haplo.glm.Rd,v $
% Revision 1.1  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.4  2008/04/04 16:12:33  sinnwell
% add note about returned value by invisible()
%
% Revision 1.3  2008/03/24 21:14:44  sinnwell
% add show.missing argument
%
% Revision 1.2  2008/01/09 21:32:16  sinnwell
% change comments to %
%
% Revision 1.1  2007/11/07 21:46:59  sinnwell
% Initial revision
%
% Revision 1.2  2004/03/16 17:02:24  sinnwell
% add return char to usage

\name{summary.haplo.glm}
\alias{summary.haplo.glm}
\alias{print.summary.haplo.glm}
\title{
  Print and summary of a haplo.glm object
}
\description{
  Do print and summary as in regular glm, then display extra information
  on haplotypes used in the model fit 
}
\usage{
\method{summary}{haplo.glm}(object, show.all.haplo=FALSE,
                            show.missing=FALSE, ...)
\method{print}{summary.haplo.glm}(x, digits = max(getOption("digits")-3,3), ...)
}
\arguments{
\item{x}{
  A haplo.glm object
}
\item{object}{
  A haplo.glm object
}
\item{show.all.haplo }{
Logical.  If TRUE, print all haplotypes considered in the model.
}
\item{show.missing }{
Logical. If TRUE, print number of rows removed because of missing
values (NA) in y or x-covariates, or all alleles missing in geno
}
\item{digits}{
Number of numeric digits to print.
}
\item{\dots }{
Optional arguments for summary method
}
}
\value{
If print is assigned, the object contains a list with the coefficient
and haplotype data.frames which are printed by the method.
}
\details{
  Uses print.glm for the first section, then prints information on the
  haplotypes.
}
\seealso{
  \code{\link{haplo.glm}}
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
