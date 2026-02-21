\name{genomicper-package}
\alias{genomicper-package}
\alias{genomicper}
\docType{package}
\title{
Circular Genomic Permutations
}
\description{
Description: Circular genomic permutation approach uses genome wide association studies (GWAS) results to establish the significance of pathway/gene-set associations whilst accounting for genomic structure. All single nucleotide polymorphisms (SNPs) in the GWAS are placed in a 'circular genome' according to their location. Then the complete set of SNP association p-values are permuted by rotation with respect to the SNPs' genomic locations. Two testing frameworks are available: permutations at the gene level, and permutations at the SNP level. The permutation at the gene level uses Fisher's combination test to calculate a single gene p-value, followed by the hypergeometric test. The SNP count methodology maps each SNP to pathways/gene-sets and calculates the proportion of SNPs for the real and the permutated datasets above a pre-defined threshold. Genomicper requires a matrix of GWAS association p-values and SNPs annotation to genes. Pathways can be obtained from within the package or can be provided by the user.
}
\details{
\tabular{ll}{
Package: \tab genomicper\cr
Type: \tab Package\cr
Version: \tab 1.7\cr
Date: \tab 2020-05-06\cr
License: \tab GPL-2\cr
}
}

\author{
Claudia P. Cabrera, Pau Navarro, Chris S. Haley \cr
Maintainer: Claudia Cabrera <c.cabrera@qmul.ac.uk>
}

\references{
SNP-level Permutations:\cr
Genomicper: genome-wide association SNP-set analysis\cr
Claudia P. Cabrera*, Pau Navarro*, Jennifer E. Huffman, Alan F. Wright, Caroline Hayward,Harry Campbell, James F. Wilson, Igor Rudan, Nicholas D. Hastie, Veronique Vitart, Chris S. Haley*\cr	
\cr
Gene-level Permutations:\cr
Uncovering Networks from Genome-Wide Association Studies via\cr 
Circular Genomic Permutation. G3: Genes|Genomes|Genetics 2, 1067-1075.\cr
Claudia P. Cabrera*, Pau Navarro*, Jennifer E. Huffman, Alan F. Wright, Caroline Hayward,Harry Campbell, James F. Wilson, Igor Rudan, Nicholas D. Hastie, Veronique Vitart, Chris S. Haley*
}
\seealso{
Genomicper functions:	
1)  \code{\link{read_pvals}},
2)  \code{\link{genome_order}},
3)  \code{\link{get_pathways}},
4)  \code{\link{read2_paths}},
5A) \code{\link{snps_permutation}},
5B) \code{\link{genes_permutation}},
6)  \code{\link{get_results}},
7)  \code{\link{plot_results}}
}
\keyword{ package }
\examples{
#############################################################################
#  Genomicper functions                                            ##########
# 1)  read_pvals(data_name="",snps_ann="")
# 2)  genome_order(all_data="")
# 3)  get_pathways(source="reactome",all_paths="",envir="")
# 4)  read2_paths(ordered_alldata="",gs_locs="",sets_from="",sets_prefix="RHSA",level="")
# 5A) snps_permutation(ordered_alldata="",pers_ids="",ntraits="",nper="",saveto="",
#		threshold="",gs_locs=gs_locs,envir = "")	
# 5B) genes_permutation(ordered_alldata="",pers_ids="",pathways="",
#		ntraits="",nper="",threshold="",saveto="",gs_locs=gs_locs,envir = "")
# 6)  get_results(res_pattern="Permus",level="snp",from="workspace",
#		threshold=0.05, envir = "")
# 7) plot_results(results = "", by = "", plot_all = TRUE, var = "", save_plot = TRUE, 
# 						plot_name = "", bf = FALSE, save_qq = TRUE)  
#############################################################################
############## DEMO: #######################################################

#### SNP-level  #############################################################
# SNPs annotation and Pathways provided by user
# all data stored at the WORKSPACE

### Load files for analysis
data(demo,SNPsAnnotation)

# Read & format GWAS pvalues
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)

# Order data according to the genome
genome_results <-genome_order(all_data=all_data)
	# Results from genome_order
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs

# Create new environment to save variables (e.g. pathways, permutations):
gper.env <- new.env()

# Pathways can be downloaded using the function get_pathways()  
# Load example pathways into the new environment. 
data(RHSA164843,RHSA446343,RHSA8876384,RHSA8964572,RHSA109582,RHSA1474244,envir=gper.env)

# Map SNPs to pathways 
paths_res <- read2_paths(ordered_alldata=ordered_alldata,
gs_locs=gs_locs,sets_from="workspace",sets_prefix="RHSA",
level="snp",envir=gper.env)
	# Results from read2_paths:		
	pers_ids <- paths_res$per_ors
	pathways<- paths_res$pathways

# Perform permutations:
snps_permutation(ordered_alldata=ordered_alldata,
pers_ids=pers_ids,ntraits=c(7:13),nper=10,saveto="workspace",
threshold=0.05,gs_locs=gs_locs,envir = gper.env)		  

# Get results						
results <- get_results(res_pattern="Permus",level="snp",
from="workspace",threshold=0.05,envir = gper.env)

# Plot results
\dontrun{
#saves plots to working directory
qq <- plot_results(results=results,by="set",plot_all=TRUE)
qq <- plot_results(results=results,by="trait",
plot_all=FALSE,var="trait1")
# Displays interactive plot. Select a trait/set to plot and 
# set arguments save_plot=FALSE, plot_all = FALSE
# IMPORTANT: to EXIT interactive plot, RIGHT CLICK on the
# plot and STOP.
qq <- plot_results(results=results,by="set",plot_all=FALSE,
var="RHSA109582",save_plot=FALSE)
}
# -- END OF DEMO 
###############################################
}
