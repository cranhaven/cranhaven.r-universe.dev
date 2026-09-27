% $Author: sinnwell $ -->
% $Date: 2011/11/10 15:29:41 $ -->
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/plot.haplo.score.slide.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $ -->
% $Locker:  $ -->
% 
% $Log: plot.haplo.score.slide.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1.1.1  2011/04/13 15:48:54  sinnwell
% bring in haplo.stats, uses functions from rlocal and mgenet
%
% Revision 1.2  2008/01/08 20:28:31  sinnwell
% fix comment
%
% Revision 1.1  2008/01/08 17:12:10  sinnwell
% Initial revision
%
% Revision 1.14  2007/05/29 15:53:50  sinnwell
% leave cex.axis and las/srt to be given by user
% 
% Revision 1.13  2007/05/22 21:22:25  sinnwell
% cex to cex.axis and srt to las
% 
% Revision 1.12  2007/04/12 21:08:05  sinnwell
% add comments about near-zero p-values.
% 
% Revision 1.11  2005/03/31 19:23:24  sinnwell
% *** empty log message ***
% 
% Revision 1.10  2004/03/16 22:26:38  sinnwell
% comment example, point to haplo.score.slide
% 
% Revision 1.9  2004/03/03 17:33:43  sinnwell
% T to TRUE
% 
% Revision 1.8  2004/03/03 15:47:05  sinnwell
% examples update
% 
% Revision 1.7  2004/03/02 15:03:23  sinnwell
% fix example
% 
% Revision 1.6  2004/02/06 19:30:30  sinnwell
% *** empty log message ***
% 
% Revision 1.5  2004/02/06 17:44:37  sinnwell
% add cex,srt,... documentation
% 
% Revision 1.4  2004/01/27 21:26:26  sinnwell
% update for marker distance option
% 
% Revision 1.3  2003/09/16 21:09:33  sinnwell
% edit pval explanations
% 
% Revision 1.2  2003/08/27 21:17:13  schaid
% *** empty log message ***
% 
% Revision 1.1  2003/08/22 20:37:14  sinnwell
% Initial revision


\name{plot.haplo.score.slide}
\alias{plot.haplo.score.slide}
\title{
  Plot a haplo.score.slide Object 
}
\description{
Method function to plot an object of class haplo.score.slide.  The
p-values from haplo.score.slide are for sub-haplotypes of a larger
chromosomal region, and these are plotted to visualize the change in
p-values as the sub-haplotype "slides" over a chromosome. Plot
-log10(p-value) on the y-axis vs. the loci over which it was computed
on the x-axis.
}
\usage{
\method{plot}{haplo.score.slide}(x, pval="global", dist.vec=1:x$n.loci, ...)
}
\arguments{
\item{x}{
The object returned from haplo.score.slide
}
\item{pval}{
Character string for the choice of p-value to plot. Options are:
"global" (the global score statistic p-value based on an asymptotic
chi-square distribution), "global.sim" (the global score statistic
simulated p-value), and "max.sim" (the simulated p-value for the
maximum score statistic).
}
\item{dist.vec }{
Numeric vector for position (i.e., in cM) of the loci along a
chromosome. Distances on x-axis will correspond to these positions. 
}
\item{\dots }{
Dynamic parameter for the values of additional parameters for 
the plot method. Some useful options for manageing the x-axis labels
are cex.axis, las, and srt.
}
}
\value{
Nothing is returned.
}
\details{
The x-axis has tick marks for all loci.  The y-axis is the -log10() of
the selected p-value.  For each haplo.score result, plot a horizontal
line at the height of -log10(p-value) drawn across the loci over which
it was calculated.  Therefore a p-value of 0.001 for the first 3 loci
will plot as a horizontal line plotted at y=3 covering the first three
tick marks.  
If the p-value for a set of loci is zero or very near zero, it is set
to a minimum.  Global asymptotic p-values of zero are set to the
minimum of an epsilon or the lowest non-zero p-value in the region.
Simulated p-values equal to zero are set to 0.5 divided by the total number of
simulations performed.
}
\section{References}{
Schaid DJ, Rowland CM, Tines DE, Jacobson RM,  Poland  GA.
"Score tests for association of traits with haplotypes when
linkage phase is ambiguous." Amer J Hum Genet. 70 (2002):  425-434.
}
\seealso{
\code{\link{haplo.score.slide}}
}
\examples{
# This example has a long run-time, therefore it is commented

#   data(hla.demo)
#   attach(hla.demo)
#   geno.11 <- hla.demo[,-c(1:4)]
#   label.11 <- c("DPB","DPA","DMA","DMB","TAP1","TAP2","DQB","DQA","DRB","B","A")

#For an ordinal trait, slide by 3 loci, and simulate p-values:
#   y.ord <- as.numeric(resp.cat)
#   slide.ord.sim <-  haplo.score.slide(y.ord, geno.11, trait.type = "ordinal",
#                              n.slide=3, locus.label=label.11, simulate=TRUE,
#                              sim.control=score.sim.control(min.sim=500))

#   print(slide.ord.sim)
#   plot(slide.ord.sim)
#   plot(slide.ord.sim, pval="global.sim", las=2, cex.axis=.8)
#   plot(slide.ord.sim, pval="max.sim", srt=90, cex.axis=.8)
}
\keyword{scores}
% docclass is function
% Converted by Sd2Rd version 37351.
