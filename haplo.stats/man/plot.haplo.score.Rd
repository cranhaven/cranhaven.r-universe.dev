% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/plot.haplo.score.Rd,v 1.3 2011/11/10 15:29:41 sinnwell Exp $
% $Locker:  $ 
% $Log: plot.haplo.score.Rd,v $
% Revision 1.3  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.2  2008/01/08 20:28:21  sinnwell
% fix comment
%
% Revision 1.1  2008/01/08 17:04:14  sinnwell
% Initial revision
%
%Revision 1.6  2004/03/17 21:29:50  sinnwell
%use keep to shorten example
%
%Revision 1.5  2004/03/02 14:53:00  sinnwell
%fix example
%
%Revision 1.4  2004/02/06 17:44:16  sinnwell
%add ... argument
%
%Revision 1.3  2003/09/22 16:33:37  sinnwell
%update reference (?)
%
%Revision 1.2  2002/09/16 14:58:35  sinnwell
%Fix RCS keywords
%
%Revision 1.1 2002/09/16 14:00:00 sinnwell
%initial


\name{plot.haplo.score}
\alias{plot.haplo.score}
\title{
  Plot Haplotype Frequencies versus Haplotype Score Statistics
}
\description{
Method function to plot a class of type haplo.score
}
\usage{
\method{plot}{haplo.score}(x, ...)
}
\arguments{
\item{x}{
The object returned from haplo.score (which has class haplo.score).
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the plot method.  
}
}
\value{
Nothing is returned.
}
\details{
This is a plot method function used to plot haplotype frequencies on
the x-axis and haplotype-specific scores on the y-axis. Because
haplo.score is a class, the generic plot function 
can be used, which in turn calls this plot.haplo.score function.
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
"Score tests for association of traits with haplotypes when
linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\seealso{
haplo.score
}
\examples{
  data(hla.demo)
  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
  keep <- !apply(is.na(geno) | geno==0, 1, any)
  hla.demo <- hla.demo[keep,]
  geno <- geno[keep,]
  attach(hla.demo)
  label <- c("DQB","DRB","B")
 
# For quantitative, normally distributed trait:

  score.gaus <- haplo.score(resp, geno, locus.label=label, 
                            trait.type = "gaussian")

  plot.haplo.score(score.gaus)
  ## try: locator.haplo(1)
}
% docclass is function
% Converted by Sd2Rd version 37351.
