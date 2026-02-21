\name{get_results}
\alias{get_results}
\title{
Circular Permutation Results
}
\description{
Creates a summary dataframe of the genomic permutations datasets
}
\usage{
get_results(res_pattern="Permus",level="snp",from="workspace",
threshold=0.05,envir = "")
}
\arguments{
  \item{res_pattern}{
Pattern of the Permutation files/variable. eg. res=pattern="Permus"
}
  \item{level}{
Permutation level performed.level values "snp" or "gene"
}
  \item{from}{
Location of the permutation datasets.from values "workspace" or "directory" 
}
  \item{threshold}{
Threshold of significance set 
}
  \item{envir}{
R environment where save the data to
}
}
\value{
\item{results}{
Data frame with Pathway ID, Trait, Threshold set by permutations,\cr 
Gene results include the theoretical hypergeometric p-value and the,\cr 
observed (Empirical Hypergeometric p-values)\cr
SNP results include the count of significan SNPs and the overall score\cr
Score is the proportion of tests observed with more significant results\cr
}
}
\section{Format}{
\preformatted{
## SNP level results
     PathID    Trait Threshold RealCount Score
1  hsa00010     abpi         0         0 0.037
2  hsa00010 abpildfa         0         0 0.040
3  hsa04720     abpi         2         0 0.311	
## Gene level results	
     PathID Trait   Threshold     P-Value  Observed
1  hsa00010  abpi 0.040441176 0.058823529 1.0000000
2  hsa00020  abpi 0.000000000 0.000000000 0.1666667
3  hsa00030  abpi 0.040441176 0.058823529 1.0000000
}
}
\examples{
data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
genome_results <-genome_order(all_data=all_data)
	# Results from genome_order
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs

# Create new environment to save data
gper.env <- new.env()

# Get pathways
data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)

paths_res <- read2_paths(ordered_alldata=ordered_alldata,gs_locs=gs_locs,
sets_from="workspace",sets_prefix="RHSA",level="snp",envir=gper.env)
		pers_ids <- paths_res$per_ors
		pathways<- paths_res$pathways

snps_permutation(ordered_alldata=ordered_alldata,pers_ids=pers_ids,
ntraits=c(7,9),nper=10,saveto="workspace",threshold=0.05,
gs_locs=gs_locs,envir= gper.env)

results <- get_results(res_pattern="Permus",level="snp",
from="workspace",threshold=0.05,envir = gper.env)
}
\keyword{ results }

