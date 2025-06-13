\name{PD4107a}
\alias{PD4107a}
\docType{data}
\title{
Somatic mutations data set from a primary breast cancer genome.
}
\description{
PD4107a is a data set of somatic substitution mutations from a primary breast cancer whole genome with a germline mutation in BRCA1 (Nik-Zainal et al. 2012). The data set contains five variables: sample name, chromosome where the somatic mutation is located, location of the somatic mutation, the reference base and the mutated base. 

The complete set of somatic mutations from a patient with breast cancer (PD4107a) was provided by the Cancer Genome Project group at the Wellcome Trust Sanger Institute (Alexandrov et al. 2013). Mutations with Indel labels were deleted (only subs).
}
\usage{data(PD4107a)}
\format{
  A data frame with 9879 observations on the following 5 variables.
  \describe{
    \item{\code{Sample_id}}{: PD4107a.}
    \item{\code{Chr}}{: From chromosome 1 to chromosome X.}
    \item{\code{Position}}{: Mutation locations on the chromosome.}
    \item{\code{Ref_base}}{: The reference base in the mutation locations.}
    \item{\code{Mutant_base}}{: The mutated base in the mutation locations.}
  }
}
\details{
Patient PD4107a has been described throughout the scientific literature (Alexandrov et al 2013; Fischer et al 2013; Muino et al 2014; Nik-Zainal et al 2012; Roberts et al 2013). 
}
\source{
\url{ftp://ftp.sanger.ac.uk/pub/cancer/AlexandrovEtAl/somatic_mutation_data/Breast/Breast_clean_somatic_mutations_for_signature_analysis.txt}
}
\references{
Alexandrov LB, Nik-Zainal S, Wedge DC, et al. Signatures of mutational processes in human cancer. Nature. 2013 Aug 22;500(7463):415-21.

Hahsler M and Hornik K. Dissimilarity plots: A visual exploration tool for partitional clustering. Journal of Computational and Graphical Statistics, 10(2):335--354, June 2011.

Fischer A, Illingworth CJ, Campbell PJ, et al. EMu: probabilistic inference of mutational processes and their localization in the cancer genome. Genome Biol. 2013 Apr 29;14(4):R39.

Muino JM, Kuruoglu EE, Arndt PF. Evidence of a cancer type-specific distribution for consecutive somatic mutation distances. Comput Biol Chem. 2014 Aug 23. pii: S1476-9271(14)00091-7.

Nik-Zainal S, Alexandrov LB, Wedge DC, et al; Breast Cancer Working Group of the International Cancer Genome Consortium. Mutational processes molding the genomes of 21 breast cancers. Cell. 2012 May 25;149(5):979-93.

Roberts SA, Lawrence MS, Klimczak LJ, et al. An APOBEC cytidine deaminase mutagenesis pattern is widespread in human cancers. Nat Genet. 2013 Sep;45(9):970-6. 
}
\examples{
data(PD4107a)

###PD4107a data set;
head(PD4107a,12)

###generate new data set with intermutational distance;
#rainfall<-imd(data=PD4107a,chr=Chr,position=Position)
###Rainfall plot for PD4107a cancer sample;
#plot(rainfall$number, rainfall$log10distance,pch=20,
#	ylab="Intermutation distance (bp)",xlab="PD4107a",yaxt="n", 
#	col=c(rep(c("black","red"),14)[rainfall$chr]))
#axis(2, at=c(0,1,2,3,4,6), labels=c("1","10","100","1000","10000","1000000"),
#	las=2, cex.axis=0.6)

###Locate the clustered mutations;
#showers(data=PD4107a,chr=Chr,position=Position)

###Visualizes a dissimilarity mutation matrix using seriation and matrix shading 
### using the method developed by Hahsler and Hornik (2011).
###Chromosome 6;
#mut.matrix <- dissmutmatrix(data=PD4107a,chr=Chr,position=Position,subset=6)
#dissplot(mut.matrix, method=NA, options=list( col = 
#  c("black","navy","blue","cyan","green","yellow","orange","red",
#	"darkred","darkred","white")))
}
\keyword{datasets}
