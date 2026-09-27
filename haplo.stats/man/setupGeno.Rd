% $Author: sinnwell $ 
% $Date: 2011/12/12 17:02:15 $ 
% $Header: /projects/genetics/cvs/cvsroot/haplo.stats/man/setupGeno.Rd,v 1.4 2011/12/12 17:02:15 sinnwell Exp $ 
% $Locker:  $ 
% $Log: setupGeno.Rd,v $
% Revision 1.4  2011/12/12 17:02:15  sinnwell
% notes about LC-COLLATE, and update see-also
%
% Revision 1.3  2011/11/10 15:29:41  sinnwell
% major update to hapglm, minor changes to Rd files, prepare for version 1.5.0 release
%
% Revision 1.2  2008/01/09 21:21:56  sinnwell
% *** empty log message ***
%
% Revision 1.1  2008/01/09 19:54:34  sinnwell
% Initial revision
%

\name{setupGeno}
\alias{setupGeno}
\title{
Create a group of locus objects from a genotype matrix, assign to
'model.matrix' class.  
}
\description{
The function makes each pair of columns a locus object, which
recodes alleles to numeric and saves the original alleles as an
attribute of the model.matrix. 
}
\usage{
setupGeno(geno, miss.val=c(0,NA), locus.label=NULL)
}
\arguments{
\item{geno}{
Matrix of alleles, such that each locus has a pair of adjacent
columns of alleles, and the order of columns corresponds to the
order of loci on a chromosome. If there are K loci, then
ncol(geno) = 2*K. Rows represent alleles for each subject.
}
\item{miss.val }{
A vector of codes denoting missing values for allele1 and
allele2. Note that NA will always be treated as a missing value, even
if not specified in miss.val.  Also note that if multiple missing
value codes are specified, the original missing value code for a
specific individual can not be retrieved from the loci object.
}
\item{locus.label}{
vector of labels for the loci
}
}
\value{
A 'model.matrix' object with the alleles recoded to numeric values,
and the original values are stored in the 'unique.alleles' attribute.
The ith item of the unique.alleles list is a vector of unique alleles
for the ith locus.
}
\details{
  This function contains the essential parts of the loci function,
  which is no longer within haplo.stats
}
\note{
A matrix that contains all elements of mode character will be sorted
in alphabetic order. This order may differ across platforms according
to your setting of LC_COLLATE.  See the note in \code{haplo.em} about
how this sort order affects results.
}
\seealso{
  \code{\link{locus}},
  \code{\link{haplo.glm}},
  \code{\link{haplo.em}}
}
\examples{
# Create some loci to work with
a1 <- 1:6
a2 <- 7:12

b1 <- c("A","A","B","C","E","D")
b2 <-c("A","A","C","E","F","G")

c1 <- c("101","10","115","132","21","112")
c2 <- c("100","101","0","100","21","110")

myGeno <- data.frame(a1,a2,b1,b2,c1,c2)
myGeno <- setupGeno(myGeno)
myGeno

attributes(myGeno)$unique.alleles
}
% docclass is function
% Converted by Sd2Rd version 37351.
