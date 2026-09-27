% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/fitted.haplo.glm.Rd,v 1.1 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% $Log: fitted.haplo.glm.Rd,v $
% Revision 1.1  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%

\name{fitted.haplo.glm}
\alias{fitted.haplo.glm}
\title{
  Fitted values from haplo.glm fit
}
\description{
  The fitted values for each person, collapsed over their expanded
  fitted values due to their multiple possible haplotype pairs
}
\usage{
\method{fitted}{haplo.glm}(object, ...)
}
\arguments{
\item{object}{
  A haplo.glm object
}
\item{\dots }{
Optional arguments for the method
}
}
\value{
vector of fitted values
}
\details{
 Many of the subjects in a haplo.glm fit are expanded in the model
 matrix with weights used to reflect the posterior probability of the
 subject's haplotype pairs given their genotype.  The working fitted
 values within the fitted object are from this expanded model matrix,
 and the fitted values from this method are calculated from the
 weighted fitted value for the subject across all their haplotype pairs.
}
\seealso{
\code{\link{haplo.glm}}
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
