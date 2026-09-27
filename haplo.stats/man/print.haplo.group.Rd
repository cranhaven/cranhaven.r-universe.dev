% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/print.haplo.group.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% 
%$Log: print.haplo.group.Rd,v $
%Revision 1.3  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.2  2008/01/08 20:38:37  sinnwell
%add log
%
%Revision 1.7  2004/06/09 19:01:37  sinnwell
%haplo.group class, not haplo.score
%
%Revision 1.6  2004/04/06 21:30:30  sinnwell
%add nlines parm
%
%Revision 1.5  2004/03/02 15:05:30  sinnwell
%take out example, already shown in haplo.group.sgml
%
%Revision 1.4  2004/02/27 15:33:19  sinnwell
%add entry for '...' arg
%
%Revision 1.3  2003/03/19 16:17:02  sinnwell
%remove spaces after keywords for Rd conversion
%
%Revision 1.2  2002/09/16 14:59:07  sinnwell
%Fix RCs keywords

\name{print.haplo.group}
\alias{print.haplo.group}
\title{
  Print a haplo.group object
}
\description{
Method function to print a class of type haplo.group
}
\usage{
\method{print}{haplo.group}(x, digits=max(options()$digits-2, 5), nlines=NULL, ...)
}
\arguments{
\item{x}{
The object returned from haplo.group (which has old class haplo.group).
}
\item{digits}{
Set the number of significant digits to print for haplotype probabilities.
}
\item{nlines}{
For shorter output, print first 1:nlines rows of the large data frame
}
\item{\dots}{
Optional arguments for the print method
}
}
\value{
Nothing is returned.
}
\details{
This is a print method function used to print information from the
haplo.group class, with haplotype-specific information given in a
table. Because haplo.group is a class, the generic print function 
can be used, which in turn calls this print.haplo.group function.
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM, Poland  GA.
Expected haplotype frequencies for association of traits with 
haplotypes when linkage phase is ambiguous. 
Submitted to Amer J Hum Genet.
}
\seealso{
haplo.score, haplo.group, haplo.em
}
% docclass is function
% Converted by Sd2Rd version 37351.
