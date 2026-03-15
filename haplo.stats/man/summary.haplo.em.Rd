% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/summary.haplo.em.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ 
% $Log: summary.haplo.em.Rd,v $
% Revision 1.3  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.2  2008/01/08 22:23:35  sinnwell
% change comments
%
% Revision 1.1  2007/11/07 21:33:46  sinnwell
% Initial revision
%
% Revision 1.2  2004/04/06 20:46:27  sinnwell
% add nlines
%
% Revision 1.1  2004/02/26 22:36:18  sinnwell
% Initial revision

\name{summary.haplo.em}
\alias{summary.haplo.em}
\title{
  Summarize contents of a haplo.em object
}
\description{
Display haplotype pairs and their posterior probabilities by subject.
Also display a table with number of max haplotype pairs for a
subject versus how many were kept (max vs. used).  
}
\usage{
\method{summary}{haplo.em}(object, show.haplo=FALSE, digits=max(options()$digits-2, 5), nlines=NULL, ...)
}
\arguments{
\item{object}{
A haplo.em object
}
\item{show.haplo }{
Logical. If TRUE, show the alleles of the haplotype pairs, otherwise show
only the recoded values.
}
\item{digits}{
number of significant digits to be printed for numeric values
}
\item{nlines}{
To shorten output, print the first 1:nlines rows of the large data frame.  
}
\item{\dots }{
Optional arguments for the summary method
}
}
\value{
  A data.frame with a row for every subject's possible haplotype pairs
  and the posterior probabilities of that pair given their genotypes.
}
\seealso{
\code{\link{haplo.em}}
}
\examples{
data(hla.demo)
geno <- hla.demo[,c(17,18,21:24)]
label <-c("DQB","DRB","B")
keep <- !apply(is.na(geno) | geno==0, 1, any)

save.em.keep <- haplo.em(geno=geno[keep,], locus.label=label)
save.df <- summary(save.em.keep)
save.df[1:10,]
}
% docclass is function
% Converted by Sd2Rd version 37351.
