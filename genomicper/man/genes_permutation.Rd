\name{genes_permutation}
\alias{genes_permutation}
\title{
Gene-level Permutations 
}
\description{
Performs gene-level circular genomic permutations. In each permutation,the complete set of\cr 
SNP association p-values are permuted by rotation with respect to the SNPs' genomic locations.\cr
Once these 'simulated' p-values are assigned,the joint gene p-values are calculated using\cr
Fisher's combination test,and pathways' association tested using the hypergeometric test\cr
}
\usage{
genes_permutation(ordered_alldata = "", pers_ids = "", pathways = "", 
ntraits = "", nper = 100, threshold = 0.05, seed=10,saveto = "workspace", 
gs_locs="", envir = "")
}
\arguments{
  \item{ordered_alldata}{
Return variable from "genome_order". Ordered genome and trait p-values
}
  \item{gs_locs}{
Return variable from "genome_order". SNP indexes
}
  \item{pers_ids}{
Return variable "per_ors" from "read2_paths". Gene indexes
}
  \item{pathways}{
Return variable "pathways" from "read2_paths"
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
Set a number for random sampling
}
  \item{saveto}{
Save permutation results to "workspace" OR "directory"
}
  \item{envir}{
R environment to save the data to when saveto is set to "workspace"
}
}
\value{
Returns "Permus_trait" variables or files (permutation datasets). 
}
\references{
Imports phyper (from stats)
}
\seealso{
\code{\link{snps_permutation}}
}
\examples{
#load data
data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)

# Prepare Genome
genome_results <-genome_order(all_data=all_data)
	# Results from genome_order
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs

# Create new environment to save data:
gper.env <- new.env()

# Get pathways
data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)

# Map Genes to pathways
paths_res <- read2_paths(ordered_alldata=ordered_alldata,gs_locs=gs_locs,
sets_from="workspace",sets_prefix="RHSA",level="gene",envir=gper.env)
pers_ids <- paths_res$per_ors
pathways<- paths_res$pathways

# Perform Permutations:
genes_permutation(ordered_alldata=ordered_alldata,
pers_ids=pers_ids,pathways=pathways,ntraits=c(7:9),
nper=10,threshold=0.05, saveto="workspace",
gs_locs=gs_locs,envir = gper.env)

# Results
results <- get_results(res_pattern="Permus",level="gene",
from="workspace",threshold=0.05,envir= gper.env)
}
\keyword{ permutations }
\keyword{ gene-level }
