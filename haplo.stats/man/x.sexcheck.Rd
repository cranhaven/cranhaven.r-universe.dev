% $Author: sinnwell $ 
% $Date: 2011/11/10 15:29:41 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/x.sexcheck.Rd,v 1.2 2011/11/10 15:29:41 sinnwell Exp $ 
% $Locker:  $ 
% $Log: x.sexcheck.Rd,v $
% Revision 1.2  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.1  2008/01/09 19:57:03  sinnwell
% Initial revision
%

\name{x.sexcheck}
\alias{x.sexcheck}
\title{
  consistency checks for x.linked locus  
}
\description{
Given an x.linked locus object and a vector of gender codes, the
function will check to make sure the gender codes match the codes used
to originally define the locus, and that no individuals defined as
males are heterozygous.
}
\usage{
x.sexcheck(x, sex, stop=FALSE)
}
\arguments{
\item{x}{
an object of class locus
}
\item{sex}{
a vector of codes identifying the gender of each individual contained
in the locus object
}
\item{stop}{
if T , any warnings are converted to errors and execution is
halted immediately
}
}
\value{
T if one or more errors were found
F if no errors were found
}
\seealso{
\code{\link{locus}}
}
\examples{
c1 <- c(101,10, 112,112,21,112)
c2 <- c(101,101,112,100,21, 10)

gender <- rep(c("M","F"),3)
loc2 <- locus(c1,c2,chrom="X",locus.alias="DXS1234", x.linked=TRUE, sex=gender)

loc2
}
% docclass is function
% Converted by Sd2Rd version 37351.
