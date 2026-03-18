\name{AlgebraicHaploPackage-package}
\alias{AlgebraicHaploPackage-package}
\alias{AlgebraicHaploPackage}
\docType{package}
\title{
AlgebraicHaploPackage--- The package haplotype a contingency of two pairs of snips of a sample.
}
\description{
Assume a group of patients of an unordered pair of data per snips.
The  contingency table of 2 snips on diffrent positions are calculated. A haplotype 2x2 most likely contingency table is guessed.
}
\details{
\tabular{ll}{
Package: \tab AlgebraicHaploPackage\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2015-10-26\cr
License: \tab GPL2.0
}
}
\examples{
print("The second example: \n")
dd=matrix(c(1212, 2, 0, 679, 0,0,75,0,0), byrow=TRUE, nrow=3)
colnames(dd)=c("CC","CT","TT")
rownames(dd)=c("CC","CT","TT")
callhaplotype(dd)
### Check the result of the cubic equation of the second example
print("##############################")
print("Check the result of the cubic equation of the second example: \n")
temp2haplo =as.numeric(t(dd));
t2h=temp2haplo
haplotypeit(t2h[1],t2h[2],t2h[3],t2h[4],t2h[5],t2h[6],t2h[7],t2h[8],t2h[9]);
rm(temp2haplo)
rm(t2h)
### Third example
print("##############################")
print("Third example : \n")
dd3=matrix(c(1030,678,123,1,1,0,0,0,0),ncol=3,byrow=TRUE)
colnames(dd3)=c("AA","AG","GG")
rownames(dd3)=c("CC","CT","TT")
callhaplotype(dd3)
###  Check for alternative solutions
print("##############################")
print("Check for alternative solutions: \n")
temp2haplo =as.numeric(t(dd3));
t2h=temp2haplo;
haplotypeit(t2h[1],t2h[2],t2h[3],t2h[4],t2h[5],t2h[6],t2h[7],t2h[8],t2h[9]);
rm(temp2haplo)
rm(t2h)
print("##############################")
print("##############################")
print("This tests the result of the first example of the article \n")
dd2=matrix(c(4,0,0,0,30,0,0,0,23),ncol=3,byrow=TRUE)
callhaplotype(dd2)
callhaplotype(dd2)/(2*57)
print("##############################")
}
\author{
Jan Wolfertz.

Maintainer: jan.wolfertz@uni-duesseldorf.de
}
\references{
[1] David Clayton.      
    "An r    package for analysis of whole-genome association   
     studies."Human Heredity, 64(1):45 - 51, 2007.     
     doi: doi:10.1001/archgenpsychiatry.2010.25. 
     http://archpsyc.jamanetwork.com/article.aspx?articleid=210679.
[2] Nathan Jacobson. 
    Basic Algebra I: Second Edition (Dover Books on Mathematics).   
    Dover Publication, 2009.
[3] Montgomery Slatkin Laurent Excofie.          
    Maximum-likelihood esti-mation of molecular haplotype
    frequencies in a diploid population. 
    Molecular biology and evolution, 12(5):921 - 927, 1995. 
    URL
    http://mbe.oxfordjournals.org/content/12/5/921.full.pdf.
[4] Tianhua Niu. 
    Algorithms for inferring haplotypes. 
    Genetic Epidemiology, 27:334347, 2004. 
    doi: DOI: 10.1002/gepi.20024. 
    URL
    http://biostat.gru.edu/Journal%20Club/Niu 2004.pdf.
[5] Werner A. Stahel. 
    Statistische Datenanalyse Eine. Vieweg Verlag, 2002.

}

\keyword{ package }
