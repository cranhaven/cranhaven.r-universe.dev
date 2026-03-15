%  $Author: sinnwell $
%  $Date: 2013/10/11 20:30:53 $
%  $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/plot.seqhap.Rd,v 1.7 2013/10/11 20:30:53 sinnwell Exp $
%  $Locker:  $ 
%  $Log: plot.seqhap.Rd,v $
%  Revision 1.7  2013/10/11 20:30:53  sinnwell
%  move manual to vignettes
%
%  Revision 1.6  2011/11/10 15:29:41  sinnwell
%  major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
%  Revision 1.5  2008/10/06 15:16:40  sinnwell
%  rm n.sim from example
%
%  Revision 1.4  2008/05/16 18:38:02  sinnwell
%  fix details about 0 perm pval
%
%  Revision 1.3  2008/05/09 20:59:41  sinnwell
%  add minp parameter, more comments about ...
%
%  Revision 1.2  2008/01/08 20:27:11  sinnwell
%  fix comments
%
%  Revision 1.1  2008/01/08 17:14:28  sinnwell
%  Initial revision
%
% Revision 1.5  2007/05/30 19:00:57  sinnwell
% *** empty log message ***
% 
% Revision 1.4  2007/05/30 18:29:47  sinnwell
% fix example to use data
% 
% Revision 1.3  2007/05/25 18:17:45  sinnwell
% rm par optoins cex.axis and las, reserved for users in ...
% 
% Revision 1.2  2007/05/24 19:08:17  sinnwell
% add detail about cex.axis
% 
% Revision 1.1  2007/05/23 17:41:56  sinnwell
% Initial revision

\name{plot.seqhap}
\alias{plot.seqhap}
\title{
  Plot a seqhap object
}
\description{
Method to plot an object of class seqhap. The p-values at each
locus are based on sequentially combined loci, and they
are plotted to visualize the p-values when scanning each locus using
seqhap methods. Plots -log10(p-value) on the y-axis vs. the
loci over which it was computed on the x-axis.
}
\usage{
\method{plot}{seqhap}(x, pval="hap", single=TRUE,
minp=.Machine$double.eps, ...)
}
\arguments{
\item{x}{
The object returned from seqhap
}
\item{pval}{
Character string for the choice of p-value to plot. Options are:
"hap" (sequential haplotype asymptotic p-value), "hap.sim" (sequential
haplotype simulated p-value), "sum" (sequential summary asymptotic
p-value), and "sum.sim" (sequential summary simulated p-value).  
}
\item{single}{
Logical, indicating whether to plot p-values for single-locus
association tests. If TRUE, the pointwise p-values from the
single-locus will be plotted using a dotted line.
}
\item{minp}{
Smallest "allowable" p-value; any p-value smaller will be set to
log10(minp).  The default is the value closest to zero that can be
represented in Splus/R.
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the plot method.  Accept the ylim parameter for plot() and other
parameters for lines(), points(), and axis().
Recommended values to make locus labels vertical on the x-axis:
for R:  las=2,  cex.axis=1.2
for S+: srt=90, cex.axis=1.2, adj=1
}
}
\value{
Nothing is returned.
}
\details{
The x-axis has tick marks for all loci.  The y-axis is the -log10() of
the selected p-value.  For the sequential result for each locus, a
horizontal line at the height of -log10(p-value) is drawn across the
loci combined. The start locus is indicated by a filled triangle and
other loci combined with the start locus are indicated by an asterisk
or circle.

If the permutation p-value is zero, for plotting purposes it is set to
1/(n.sim+1).
}
\section{References}{
Yu Z, Schaid DJ. (2007) Sequential haplotype scan methods for
association analysis. Genet Epidemiol, in print.
}
\seealso{
\code{\link{seqhap}},
\code{\link{print.seqhap}}
}
\examples{
  \dontrun{
    data(seqhap.dat)
    mydata.y <- seqhap.dat[,1]
    mydata.x <- seqhap.dat[,-1]
    data(seqhap.pos)
    myobj <- seqhap(y=mydata.y, geno=mydata.x, pos=seqhap.pos$pos)
    plot(myobj)
  }
}
% docclass is function
% Converted by Sd2Rd version 37351.
