% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/print.haplo.cc.Rd,v 1.4 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% 
%$Log: print.haplo.cc.Rd,v $
%Revision 1.4  2011/11/10 15:29:41  sinnwell
%major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release


\name{print.haplo.cc}
\alias{print.haplo.cc}
\title{
  Print a haplo.cc object
}
\description{
Display results for a haplotype analysis on a case-control study.  
}
\usage{
\method{print}{haplo.cc}(x, order.by=c("score","haplotype","freq"), digits=max(options()$digits-2, 5),
    nlines=NULL, ...)
}
\arguments{
\item{x}{
A haplo.cc object, made by the haplo.cc function.
}
\item{order.by}{
Order the printed data frame by the column: haplotype score (score), haplotype alleles
(haplotype), or haplotype frequency (freq).
}
\item{digits}{
Number of digits to display for the numeric columns of the data frame.
}
\item{nlines}{
Print the first nlines of the cc.df data frame of the haplo.cc object,
keeps output short if desired.
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the print method.
}
}
\value{
Nothing is returned.
}
\seealso{
\code{\link{haplo.cc}}
}
\examples{
## for a haplo.cc object named cc.test, 
##    order results by haplotype 
# print.haplo.cc(cc.test, order.by="haplotype")
}
% docclass is function
% Converted by Sd2Rd version 37351.
