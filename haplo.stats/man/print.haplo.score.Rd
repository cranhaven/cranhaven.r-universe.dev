% $Author: sinnwell $
% $Date: 2011/11/10 15:29:41 $
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/print.haplo.score.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $
% $Locker:  $
% 
%$Log: print.haplo.score.Rd,v $
%Revision 1.4  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.3  2008/04/04 16:15:59  sinnwell
%add note about tbl returned by invisible
%
%Revision 1.2  2008/01/08 21:53:19  sinnwell
%add comments
%
%Revision 1.6  2004/03/02 15:06:39  sinnwell
%remove example, shown in haplo.score.sgml
%
%Revision 1.5  2004/02/06 18:51:55  sinnwell
%add comment for '...' parameter
%
%Revision 1.4  2004/02/06 16:49:33  sinnwell
%add digits parameter
%
%Revision 1.3  2003/08/27 20:52:38  schaid
%*** empty log message ***
%
%Revision 1.2  2002/09/16 14:58:53  sinnwell
%Fix RCS keywords

\name{print.haplo.score}
\alias{print.haplo.score}
\title{
  Print a haplo.score object
}
\description{
Method function to print a class of type haplo.score
}
\usage{
\method{print}{haplo.score}(x, digits, nlines=NULL, ...)
}
\arguments{
\item{x}{
The object returned from haplo.score (which has class haplo.score).
}
\item{digits}{
Number of digits to round the numeric output.
}
\item{nlines}{
Print the first 'nlines' rows of the large data frame for fast, short
view of the results.  
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the print method.  
}
}
\value{
If print is assigned, the object contains the table of haplotype scores
that was printed by the method
}
\details{
This is a print method function used to print information from
haplo.score class, with haplotype-specific information given in a
table. Because haplo.score is a class, the generic print function 
can be used, which in turn calls this print.haplo.score function.
}
\seealso{
haplo.score
}
% docclass is function
% Converted by Sd2Rd version 37351.
