\name{showers}
\alias{showers}
\title{
Location of clustered mutations in the cancer genome.
}
\description{
\code{showers()} identifies all groups of closely spaced mutations using the anti-Robinson matrix. Hyper-mutated regions are defined as those segments containing a number (min = 6) or more mutations with a distance that is less than or equal to a number (max=1000) of bp, and referred to as mutation showers (Drake 2007a; Wang et al. 2007), clustered mutations (Drake 2007a; Drake 2007b; Roberts et al. 2012), or kataegis (from the Greek word for shower or thunderstorm) (Alexandrov et al. 2013; Nik-Zainal et al. 2012). \code{showers()} can be used to locate complex mutations (Roberts et al. 2012; Roberts et al. 2013) (min = 2; max=10). 
}
\usage{
showers(data = NULL, chr = NULL, position, min = 6, max = 5000)
}
\arguments{
  \item{data}{: somatic substitution mutations of the cancer genome data set.}
  \item{chr}{: chromosome where the somatic mutation is located.}
  \item{position}{: position of somatic mutations in the DNA sequence of the cancer genome.}
  \item{min}{: a number \code{min} of consecutive mutations.}
  \item{max}{: a distance less than or equal to a number \code{max} of bp.}
}
\details{
By default, \code{showers()} identifies the hyper-mutated zones (min = 6; max=5000). Complex mutations are those segments containing >= 2 consecutive mutations with a distance =< 100 bp.
}
\value{
\code{showers()} returns a data set with all hyper-mutated zones in the DNA sequence of tumor cells.
\item{chr}{: the shower mutations data set contains seven variables: chromosome.}
\item{pend}{: the last position in the chromosome of the mutation shower.}
\item{pstart}{: the first position in the chromosome of the mutation shower.}
\item{nend}{: the last number of a consecutive mutation shower.}
\item{nstart}{: the first number of a consecutive mutation shower.}
\item{distance}{: the length of a hyper-mutated zone and the number of mutations in the clustered mutation.}
}
\references{
Alexandrov LB, Nik-Zainal S, Wedge DC, et al. Signatures of mutational processes in human cancer. Nature. 2013 Aug 22;500(7463):415-21.

Drake JW. Mutations in clusters and showers. Proc Natl Acad Sci U S A. 2007 May 15;104(20):8203-4. 

Drake JW. Too many mutants with multiple mutations. Crit Rev Biochem Mol Biol. 2007 Jul-Aug;42(4):247-58.

Nik-Zainal S, Alexandrov LB, Wedge DC, et al; Breast Cancer Working Group of the International Cancer Genome Consortium. Mutational processes molding the genomes of 21 breast cancers. Cell. 2012 May 25;149(5):979-93.

Roberts SA, Sterling J, Thompson C, et al. Clustered mutations in  yeast and in human cancers can arise from damaged long single-strand DNA regions. Mol Cell. 2012 May 25;46(4):424-35.

Roberts SA, Lawrence MS, Klimczak LJ, et al. An APOBEC cytidine deaminase mutagenesis pattern is widespread in human cancers. Nat Genet. 2013 Sep;45(9):970-6. 

Wang J, Gonzalez KD, Scaringe WA, et al. Evidence for mutation showers. Proc Natl Acad Sci U S A. 2007 May 15;104(20):8403-8.
}
\author{
David Lora.
}
\examples{
###Example 1:
example1<-c(1,101,201,299,301,306,307,317,318,320,418,518,528,628)
showers(position=example1, min=5, max=100)

###Example 2:
example2<-c(1,101,201,299,301,306,307,317,318,320,402,404,406,628)
showers(position=example2, min=5, max=100)

###Example 3:
#data(PD4107a)

###Locate the clustered mutations;
#showers(data=PD4107a,chr=Chr,position=Position)

###Locate the complex mutations;
#complex.showers<-showers(data=PD4107a,chr=Chr,position=Position,min=2,max=10)
#nrow(complex.showers)
#table(complex.showers$chr)
}
