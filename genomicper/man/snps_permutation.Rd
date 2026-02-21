\name{snps_permutation}
\alias{snps_permutation}
\title{
SNP-level permutations
}
\description{
Performs SNP-level circular genomic permutations. In each permutation,\cr 
the complete set of SNP association p-values are permuted by rotation\cr
with respect to the SNPs' genomic locations.\cr
Once these 'simulated' p-values are assigned,the proportion of SNPs per\cr
set above a pre-defined threshold is calculated
}
\usage{
snps_permutation(ordered_alldata = "", pers_ids = "", ntraits = "", 
nper = 100, threshold = 0.05, seed=10,saveto = "workspace", 
gs_locs = "",envir ="")
}
\arguments{
  \item{ordered_alldata}{
Return variable from "genome_order". Ordered genome and trait p-values
}
  \item{gs_locs}{
Return variable from "genome_order". SNP indexes
}
  \item{pers_ids}{
Return variable "per_ors" from "read2_paths". SNP indexes
}
  \item{ntraits}{
Traits INDEX to be analysed. Index according to "ordered_alldata". \cr
Trait Columns index must start at 7. Example: ntraits=c(7:9), ntraits=7
}
  \item{nper}{
Number of permutations.Example: nper=1000
}
  \item{threshold}{
Threshold to be set by the hypergeometric test. threshold=0.05
}
  \item{seed}{
Set number for random sampling
}
  \item{saveto}{
Save permutation results to "workspace" OR "directory"
}
  \item{envir}{
R environment to save the Permutations to when saveto is set to "workspace"
}
}
\value{
Returns "Permus_genesetsname" variables or files (permutation datasets). 
}
\seealso{
\code{\link{genes_permutation}}
}
\examples{

data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
genome_results <-genome_order(all_data=all_data)
	# Results from genome_order
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs

# Create new environment to save the permutations to:
gper.env <- new.env()

data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)

paths_res <- read2_paths(ordered_alldata=ordered_alldata,gs_locs=gs_locs,
sets_from="workspace",sets_prefix="RHSA",level="snp",envir=gper.env)
		pers_ids <- paths_res$per_ors
		pathways<- paths_res$pathways

		
# SNP permutations
snps_permutation(ordered_alldata=ordered_alldata,pers_ids=pers_ids,
ntraits=c(7,9),nper=10,saveto="workspace",threshold=0.05,
gs_locs=gs_locs,envir = gper.env)

# Get results						
results <- get_results(res_pattern="Permus",level="snp",
from="workspace",threshold=0.05,envir = gper.env)

}
\keyword{ permutations }
\keyword{ snp-level }
