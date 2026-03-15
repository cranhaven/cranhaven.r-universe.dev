% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/residuals.haplo.glm.Rd,v 1.1 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% $Log: residuals.haplo.glm.Rd,v $
% Revision 1.1  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% 

\name{residuals.haplo.glm}
\alias{residuals.haplo.glm}
\title{
  Accessing residuals for haplo.glm fit
}
\description{
  Access the residuals from a haplo.glm model fit
}
\usage{
\method{residuals}{haplo.glm}(object, type=c("deviance", "pearson",
                                "working", "response"), ...)
}
\arguments{
\item{object}{
  A haplo.glm object
}
\item{type}{
Type of residuals to return. Options are "deviance" (default),
"pearson", "working", and "response". Partial residuals not supported
in this method.
}
\item{\dots }{
Optional arguments
}
}
\value{
Residuals for each person in the model.
}
\details{
  Many of the subjects in a haplo.glm fit are expanded in the model
  matrix with weights used to reflect the posterior probability of the
  subject's haplotype pairs given their genotype.  The working residuals
  within the fitted object are from this expanded model matrix, and the
  residuals in this method are calculated from the weighted fitted value
  for the subject across all their haplotype pairs.
}
\seealso{
  \code{\link{haplo.glm}},
  \code{\link{residuals.glm}},
  \code{\link{fitted.haplo.glm}}
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
