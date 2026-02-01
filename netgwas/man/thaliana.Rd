\name{thaliana}
\alias{thaliana}
\docType{data}
\title{Arabidopsis thaliana phenotype and genotype data }
\description{ 
  The genotype data of the Kend-L x Col Recombinant Inbred Line (RIL) population along with flowering time and leaf numbers phenotype information.
}

\usage{data(thaliana)}
\format{
  The format is a matrix containing 181 single-nucleotide polymorphism (SNP) markers and 8 phenotypes information for 197 individuals. 
}  

\details{
The accession Kend-L (Kendalville-Lehle; Lehle-WT-16-03) is crossed to the common lab strain Col (Co\-lum\-bi\-a).
The resulting lines were taken through six rounds of selfing without any intentional selection. The resulting 282 KendC (Kend-L \eqn{\times} Col) 
lines were genotyped at 181 markers.  The flowering time was measured for 197 lines of this population in both long days, which promote rapid 
flowering in many A. \emph{thaliana} strains, and in short days. Flowering time was measured using days to flowering (DTF) as well as the 
total number of leaves (TLN), partitioned into rosette and cauline leaves. In total eight phenotypes have been measured, namely days to 
flowering (DTF), cauline leaf number (CLN), rosette leaf number (RLN), and total leaf number (TLN) in long days (LD), and DTF, CLN, RLN, 
and TLN in short days (SD). Thus, the final dataset consist of 197 observations for 189 variables (8 phenotypes and 181 genotypes - SNP markers) \cr
This data set can be used to reconstruct network among SNP markers and the measured phenotypes.
}

\source{
Balasubramanian, Sureshkumar, et al. (2009). "QTL mapping in new Arabidopsis thaliana advanced intercross-recombinant inbred lines." PLoS One 4.2: e4318.
}

\examples{
\dontrun{
data(thaliana)

# Graph path
out <- netphenogeno(thaliana, ncores=1)
plot(out)

sel <- selectnet(out)
plot(sel, vis= "interactive")
}
}

\keyword{datasets}
