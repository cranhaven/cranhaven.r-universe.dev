\name{features}
\alias{features}
\title{
Describe the features of mutation in the hyper-mutated zones.
}
\description{
Several features were observed in the hyper-mutated zones, for example, kataegis is the proposed name for the hyper-mutated zones with a cluster of C>T and/or C>G mutations that are substantially enriched at TpCpN trinucleotides, on the same DNA strand and that co-localize with large-scale genomic structural variation (Alexandrov et al. 2013; Nik-Zainal et al. 2012).
}
\usage{
features(data = NULL, chr = NULL, position, refbase, mutantbase,
						 min = 6, max = 5000)
}
\arguments{
  \item{data}{: somatic substitution mutations of the cancer genome data set.}
  \item{chr}{: chromosome where the somatic mutation is located.}
  \item{position}{: position of somatic mutations in the DNA sequence of the cancer genome.}
  \item{refbase}{: reference base in the chromosome.}
  \item{mutantbase}{: the mutant base in the chromosome.}
  \item{min}{: a number \code{min} or more consecutive mutations.}
  \item{max}{: a distance less than or equal to a number \code{max} of bp.}
}
\details{
By default, \code{features()} identifies the mutations in the hyper-mutated zones (min = 6; max=5000). Complex mutations (Roberts et al. 2012; Roberts et al. 2013) are those segments containing >= 2 consecutive mutations with a distance =< 100 bp.
}
\value{
\code{features()} returns a data set with all mutations in the hyper-mutated zones. The data set contains five variables: 
\item{clustered}{: number of cluster.}
\item{chr}{: chromosome.}
\item{position}{: the position of mutation in the chromosome.}
\item{ref_base}{: reference base in the chromosome.}
\item{mutant_base}{: the mutant base in the chromosome.}
}
\references{
Alexandrov LB, Nik-Zainal S, Wedge DC, et al. Signatures of mutational processes in human cancer. Nature. 2013 Aug 22;500(7463):415-21.

Nik-Zainal S, Alexandrov LB, Wedge DC, et al; Breast Cancer Working Group of the International Cancer Genome Consortium. Mutational processes molding the genomes of 21 breast cancers. Cell. 2012 May 25;149(5):979-93.

Roberts SA, Sterling J, Thompson C, et al. Clustered mutations in  yeast and in human cancers can arise from damaged long single-strand DNA regions. Mol Cell. 2012 May 25;46(4):424-35.

Roberts SA, Lawrence MS, Klimczak LJ, et al. An APOBEC cytidine deaminase mutagenesis pattern is widespread in human cancers. Nat Genet. 2013 Sep;45(9):970-6. 
}
\author{
David Lora
}
\examples{
data(PD4107a)
###Show the features of mutations in the hypermutated zones;
features(data=PD4107a,chr=Chr,position=Position,refbase=Ref_base,
		mutantbase=Mutant_base)

###Locate complex mutations in the hypermutated zones;
kataegis<-features(data=PD4107a,chr=Chr,position=Position,refbase=Ref_base,
		mutantbase=Mutant_base)
complex <-features(data=PD4107a,chr=Chr,position=Position,
			refbase=Ref_base,mutantbase=Mutant_base,min=2,max=10)
sub.complex <-subset(complex,select=c(clustered,chr,position))
data.hyperm  <-merge(kataegis,sub.complex,by=c("chr","position"),all.x=TRUE) 

###Summary of the number of complex mutations in the hypermutated zones;
table(data.hyperm$clustered.x)
table(data.hyperm$clustered.y)
table(data.hyperm$clustered.y,data.hyperm$clustered.x)
data.hyperm<-transform(data.hyperm,clustered = 
				ifelse(is.na(clustered.y)==FALSE,1,0))
table(data.hyperm$clustered.x)
table(data.hyperm$clustered,data.hyperm$clustered.x)
###All hypermutated zones had more than 6 mutations (without complex mutations).
}
