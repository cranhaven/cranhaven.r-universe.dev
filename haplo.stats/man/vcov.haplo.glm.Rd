% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/vcov.haplo.glm.Rd,v 1.1 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% $Log: vcov.haplo.glm.Rd,v $
% Revision 1.1  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% 

\name{vcov.haplo.glm}
\alias{vcov.haplo.glm}
\title{variance-covariance matrix of a fitted haplo.glm object
}
\description{
  Returns the variance-covariance matrix of the main parameters of a
  fitted haplo.glm object
}
\usage{
\method{vcov}{haplo.glm}(object, freq=TRUE, ...)
}
\arguments{
\item{object}{
  A haplo.glm object
}
\item{freq}{
  Logical.  If TRUE, return the full covariance matrix including the
  entries for the frequency parameters
}
\item{\dots }{
Optional arguments for print method
}
}
\value{
Variance-covariance matrix of model parameters
}
\details{
  var.mat is pre-computed in haplo.glm, the generalized inverse of the
  Louis information matrix
}
\seealso{
\code{\link{haplo.glm}}
}
\keyword{glm}
% docclass is function
% Converted by Sd2Rd version 37351.
