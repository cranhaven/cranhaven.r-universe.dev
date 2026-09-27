% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/louis.info.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% $Log: louis.info.Rd,v $
% Revision 1.4  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.3  2009/09/30 20:05:38  sinnwell
% rm keywords, or leave one with one from /doc/KEYWORDS
%
% Revision 1.2  2009/09/30 19:32:08  sinnwell
% remove empty sections
%
% Revision 1.1  2008/01/09 19:44:30  sinnwell
% Initial revision
%

\name{louis.info}
\alias{louis.info}
\title{
  Louis Information for haplo.glm
}
\description{
For internal use within the haplo.stats library's haplo.glm function
}
\usage{
louis.info(fit, epsilon=1e-8)
}
\arguments{
\item{fit}{
  glm fitted object
}
\item{epsilon}{
  cut-off for singular values in the generalized inverse of the
  information matrix  
}
}
% docclass is function
% Converted by Sd2Rd version 37351.
