% $Author: sinnwell $ 
% $Date: 2011/10/26 15:03:59 $ 
% $Header: /projects/genetics/cvs/cvsroot/mgenet/man/locus.Rd,v 1.2 2011/10/26 15:03:59 sinnwell Exp $ 
% $Locker:  $ 
% $Log: locus.Rd,v $
% Revision 1.2  2011/10/26 15:03:59  sinnwell
% take out unused sections in Rd
%
% Revision 1.1.1.1  2011/02/21 20:09:27  sinnwell
% initial, genetics functions for Mayo BSI
%
% Revision 1.1  2008/01/09 19:43:36  sinnwell
% Initial revision
%

\name{locus}
\alias{locus}
\title{
  Creates an object of class "locus"
}
\description{
Creates an object containing genotypes for multiple individuals. The object can then use method functions developed for
objects of class "locus".
}
\usage{
locus(allele1, allele2, chrom.label=NULL,locus.alias=NULL,
     x.linked=FALSE, sex=NULL, male.code="M", female.code="F", miss.val=NA)
}
\arguments{
\item{allele1}{
A vector containing the labels for 1 allele for a set of individuals, or optionally a matrix with 2 columns each containing an allele for each person.
}
\item{allele2}{
A vector containing the labels for the second allele for a set of individuals.  If allele 1 is a matrix, allele 2 need not be specified.
}
\item{chrom.label }{
A label describing the chromosome the alleles belong to
}
\item{locus.alias }{
A vector containing one or more aliases describing the locus. The
first alias in the vector will be used as a label for printing in
some functions such as multilocus.print().  
}
\item{x.linked }{
A logical value denoting whether the chromosome is x linked
}
\item{sex}{
A vector containing the gender of each individual (required if x.linked=T)
}
\item{male.code }{
The code denoting a male in the sex vector
}
\item{female.code }{
The code denoting a female in the sex vector
}
\item{miss.val }{
a vector of codes denoting missing values for allele1 and
allele2. Note that NA will always be treated as a missing value, even
if not specified in miss.val.  Also note that if multiple missing
value codes are specified, the original missing value code for a
specific individual can not be retrieved from the locus object. 
}
}
\value{
Returns an object of class locus which inherits from class model.matrix containing the following elements:

\item{geno}{
a matrix with 2 columns where each row contains numeric codes for the 2 alleles for an individual.
}
\item{chrom.label}{
a chromosome label
}
\item{locus.alias}{
a vector of aliases for the locus
}
\item{x.linked}{
a logical value specifying if the locus is x-linked or not
}
\item{allele.labels}{
a vector of labels corresponding to the numeric codes in matrix geno (similar to levels in a factor)
}
\item{male.code}{
a code to be used to identify males for an x.linked locus.
}
\item{female.code}{
a code to be used to identify females for an x.linked locus.
}
}
\examples{
b1 <- c("A","A","B","C","E","D")
b2 <- c("A","A","C","E","F","G")
loc1 <- locus(b1,b2,chrom=4,locus.alias="D4S1111")

loc1

# a second example which uses more parameters, some may not be supported.
c1 <- c(101,10, 112,112,21,112)
c2 <- c(101,101,112,  100,21, 10)

gender <- rep(c("M","F"),3)
loc2 <- locus(c1,c2,chrom="X",locus.alias="DXS1234", x.linked=TRUE, sex=gender)

loc2
}
\keyword{classes}
% docclass is function
% Converted by Sd2Rd version 37351.
