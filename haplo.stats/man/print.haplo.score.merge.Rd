% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/print.haplo.score.merge.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $
% $Locker:  $
% 
%$Log: print.haplo.score.merge.Rd,v $
%Revision 1.3  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%Revision 1.2  2008/01/08 21:55:44  sinnwell
%add comments
%
%Revision 1.14  2004/03/16 17:03:59  sinnwell
%check return chars
%
%Revision 1.13  2004/03/12 18:07:20  sinnwell
%add returns to usage
%
%Revision 1.12  2004/03/02 15:08:22  sinnwell
%remove example, refer to haplo.score.merge.sgml
%
%Revision 1.11  2004/02/16 17:39:45  sinnwell
%change F to FALSE in the call
%
%Revision 1.10  2004/02/06 19:08:27  sinnwell
%*** empty log message ***
%
%Revision 1.9  2004/02/06 18:52:10  sinnwell
%add comments for extra parameters
%
%Revision 1.8  2004/02/06 17:45:06  sinnwell
%add '...' comment
%
%Revision 1.7  2003/06/19 22:04:36  sinnwell
%update for parameter changes in the print method
%
%Revision 1.6  2003/03/19 16:08:42  sinnwell
%remove spaces after sgml keywords for Rd conversion
%
%Revision 1.5  2003/03/13 21:50:43  sinnwell
%fix examples again
%
%Revision 1.3  2003/03/13 21:26:04  sinnwell
%fix sgml comments and try to find error for Rd
%
%Revision 1.2  2003/03/13 14:10:25  sinnwell
%take out comments
%
%Revision 1.1  2003/01/17 16:58:56  sinnwell
%Initial revision


\name{print.haplo.score.merge}
\alias{print.haplo.score.merge}
\title{
  Print a haplo.score.merge object
}
\description{
Method function to print a class of type haplo.score.merge
}
\usage{
\method{print}{haplo.score.merge}(x, order.by="score", all.haps=FALSE, 
                digits=max(options()$digits-2, 5), nlines=NULL, ...)
}
\arguments{
\item{x}{
The object returned from haplo.score.merge (which has old class \{S\} 
haplo.score.merge).
}
\item{order.by}{
Column of the haplo.score.merge object by which to order the results
}
\item{all.haps}{
Logical, if (T)rue prints a row for all haplotypes.  If (F)alse, the
default, only prints the haplotypes kept in haplo.score for modelling.
}
\item{digits}{
Set the number of significant digits to print for the numeric output.
}
\item{nlines}{
Print the first 'nlines' rows of the large data frame for a short
view of the results.
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the print method.  
}
}
\value{
Nothing is returned.
}
\details{
This is a print method function used to print information from the
haplo.score.merge class. Because haplo.score.merge is a class, 
the generic print function can be used, which in turn calls this 
print.haplo.score.merge function.
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM, Poland  GA.
Expected haplotype frequencies for association of traits with 
haplotypes when linkage phase is ambiguous. 
Submitted to Amer J Hum Genet.
}
\seealso{
haplo.score.merge, haplo.score, haplo.group
}
\examples{
#see example for haplo.score.merge
}
\keyword{scores}
% docclass is function
% Converted by Sd2Rd version 37351.
