% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/haplo.score.merge.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $
% $Log: haplo.score.merge.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/09 19:36:04  sinnwell
% Initial revision
%
%Revision 1.4  2004/03/01 22:36:18  sinnwell
%fix example
%
%Revision 1.3  2003/06/25 15:40:52  sinnwell
%parameter names fixed in examples
%
%Revision 1.2  2003/03/12 22:33:13  sinnwell
%fix comments%
%
%Revision 1.1  2003/01/17 16:57:06  sinnwell
%Initial revision


\name{haplo.score.merge}
\alias{haplo.score.merge}
\title{
  Merge haplo.score And haplo.group Objects
}
\description{
Combine information from returned objects of haplo.score and 
haplo.group, 'score' and 'group' respectively.  'score' and
'group' are sorted differently and 'score' keeps a subset of 
all the haplotypes while 'group' has all of them.  To combine
results from the two objects, merge them by haplotype and sort 
by score of the haplotype. The merged object includes all haplotypes; 
i.e. those appearing in 'group', but the print default only shows 
haplotypes which have a score.
}
\usage{
haplo.score.merge(score, group)
}
\arguments{
\item{score}{
Object returned from haplo.score of class "haplo.score". 
}
\item{group}{
Object returned from haplo.group of class "haplo.group".
}
}
\value{
Data frame including haplotypes, score-statistics, score p-value, 
estimated haplotype frequency for all subjects, and haplotype 
frequency from group subsets.
}
\section{Side Effects}{
Warning:  The merge will not detect if the group and score objects
resulted from different subject phenotypes selected by memory-usage
parameters, rm.geno.na and enum.limit.  Users must use the same values
for these parameters in haplo.score and haplo.group so the merged
objects are consistent.
}
\details{
Haplo.score returns score statistic and p-value for haplotypes with
an overall frequency above the user-specified threshold, skip.haplo.  
For haplotypes with frequencies below the threshold, the score and 
p-value will be NA.  Overall haplotype frequencies and for sub-groups
are estimated by haplo.group.    
}
\seealso{
\code{\link{haplo.score}},
\code{\link{haplo.group}}
}
\examples{
  data(hla.demo)
  geno <- as.matrix(hla.demo[,c(17,18,21:24)])
  keep <- !apply(is.na(geno) | geno==0, 1, any)
  hla.demo <- hla.demo[keep,]
  geno <- geno[keep,]
  attach(hla.demo)
  y.ord <- as.numeric(resp.cat)
  y.bin <-ifelse(y.ord==1,1,0)
  
  group.bin <- haplo.group(y.bin, geno, miss.val=0)
  score.bin <- haplo.score(y.bin, geno, trait.type="binomial")
  score.merged <- haplo.score.merge(score.bin, group.bin)

  print(score.merged)
}
\keyword{scores}
% docclass is function
% Converted by Sd2Rd version 37351.
