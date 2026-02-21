\name{read2_paths}
\alias{read2_paths}
\title{
Read to SNPs to sets; Map SNPs to gene-sets/pathways
}
\description{
Reads the sets/pathways, map the SNPs and genes to the gene-sets/pathways
read2_paths uses the "genome_order" output(ordered_alldata, gs_locs) to assign genomic location indexes
to each element in the gene-set. The permutation method must be defined (i.e. level = "snp" OR level = "gene").

}
\usage{
read2_paths(ordered_alldata="",gs_locs="",sets_from="workspace",
sets_prefix="RHSA",level="snp",envir="")
}
\arguments{
  \item{ordered_alldata}{
 Ordered data according to the SNPs genomic location. Traits start at column 7\cr
 Return variable from:\cr
 	genome_results <-genome_order(all_data=all_data)\cr
 	ordered_alldata <- genome_results$ordered_alldata\cr
}
  \item{gs_locs}{
 Gene annotation,indexes and number of observations\cr
 Return variable from genome_order():\cr
 	genome_results <-genome_order(all_data=all_data)\cr
 	gs_locs <- genome_results$gs_locs\cr
}
  \item{sets_from}{
 Location of the gene-sets. Default set to "workspace" \cr
 sets_from="workspace" OR sets_from="directory"\cr
 "directory", only will search for information in the working directory.
}
  \item{sets_prefix}{
 Prefix of the gene-set variables or files.\cr
 Default set to sets_prefix= "RHSA" 
 e.g. Variables "RHSA164843","RHSA446343","RHSA8876384"\cr
 each variable/file contains the list of gene identifiers part of that pathway
}
  \item{level}{
 The level at which the permutations will be performed. Assigns the indexes according to snps or genes\cr
 Default value "snp" 
 level values = "snp" OR "gene"
}
  \item{envir}{
 R environment where pathway data is stored. e.g(envir=.GlobalEnv, envir=gper.env)
}
}
\value{
\item{pathways}{
 Pathway Id, Description,Number of Genes in the pathway, Number of genes found in the dataset,
 Number of SNPs found in the dataset}
\item{per_ors}{
 A list of identifiers mapped to each pathway
 }
}
\section{Format}{
\preformatted{
Input: Ordered_alldata
name     Chromosome  Location GENE_ID   Symbol Orientation Trait1 Trait2
rs1001567          1 9194614     <NA>     <NA>        <NA> 0.96 0.89
rs1000313          1 15405489   23254 KIAA1026           + 0.93 0.57
rs1002365          1 19797248    <NA>     <NA>        <NA> 0.68 0.58
rs1002706          1 25051153    <NA>     <NA>        <NA> 0.71 0.02
rs1002487          1 26865971    6195  RPS6KA1           + 0.98 0.78

Input:gs_locs
      Symbol   Chromosome Location    Gene_ID Start_Indx Observations
 [1,] "ACYP2"  "2"        "54399633"  "98"    "35"       "1"         
 [2,] "AMPD3"  "11"       "10514707"  "272"   "898"      "1"         
 [3,] "ANK2"   "4"        "113830885" "287"   "479"      "4"
 
Input:pathway example
RHSA8964572
[1]   1149 128486 161247  29923 345275  63924
          
Output:pathways
     ID            GenesInPath GenesFound SNPsInPath
"RHSA109582"  "681"       "8"        "11"      
"RHSA1474244" "418"       "7"        "10"      
"RHSA164843"  "11"        "0"        "0"       
"RHSA446343"  "4"         "1"        "1"       
"RHSA8876384" "32"        "1"        "1"       
"RHSA8964572" "6"         "1"        "1"       

}
}
\seealso{
\code{\link{genes_permutation}}
\code{\link{snps_permutation}}
\code{\link{genome_order}}
}
\examples{
## DEMO - SNP Level data stored in workspace #######################
# library(genomicper)
data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
genome_results <-genome_order(all_data=all_data)
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs
	
# Create new environment to save variables (e.g. pathways, permutations):
gper.env <- new.env()

data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)

paths_res <- read2_paths(ordered_alldata=ordered_alldata,
gs_locs=gs_locs,sets_from="workspace",sets_prefix="RHSA",
level="snp",envir=gper.env)		
	pers_ids <- paths_res$per_ors
	pathways<- paths_res$pathways
####################################################################
}
\keyword{ gene-set }
\keyword{ pathways }
\keyword{ annotation }
