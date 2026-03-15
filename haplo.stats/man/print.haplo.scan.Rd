% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/print.haplo.scan.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% 
%$Log: print.haplo.scan.Rd,v $
%Revision 1.3  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.2  2008/01/08 21:48:39  sinnwell
%branch from sgml
%
%Revision 1.1  2005/03/23 18:07:44  sinnwell
%Initial revision

\name{print.haplo.scan}
\alias{print.haplo.scan}
\title{
  Print a haplo.scan object
}
\description{
Print a haplo.scan object
}
\usage{
\method{print}{haplo.scan}(x, digits=max(options()$digits - 2, 5), ...)
}
\arguments{

\item{x}{
An object created by haplo.scan
}
\item{digits}{
Significant digits shown for numeric data
}
\item{\dots }{
Options parameters for the print function
}
}
\value{
NULL
}
\seealso{
\code{\link{haplo.scan}}
}
% docclass is function
% Converted by Sd2Rd version 37351.
