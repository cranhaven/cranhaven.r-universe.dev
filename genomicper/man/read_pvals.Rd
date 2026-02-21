\name{read_pvals}
\alias{read_pvals}
\title{
Read GWAS p-values of association and Merge with SNP annotations
}
\description{
Read GWAS p-values of association and Merge with SNP annotations for analysis
}
\usage{
read_pvals(data_name="",snps_ann="",from="workspace")
}
\arguments{
  \item{data_name}{
    GWAS p_values (tab delimited file)(SNP_IDs Trait1 Trait2 ...TraitN)
}
  \item{snps_ann}{
	SNPs Annotation (SNPsAnnotation). Genomicper uses entrez gene ids to annotate associate SNPs-to genes-pathways\cr
	The annotation MUST match your data input (coordinates and chromosome format)\cr
	Any SNP ID is valid, as long the ID is set as character\cr
	The examples below show an option on how to annotate the SNPs prior the use of genomicper\cr
}
  \item{from}{
	Datasets location. Values "workspace" OR "directory" \cr
}
}
\value{
Dataframe: 
name; chromosome; Location; GeneID; Symbol; Orientation; Trait1; TraitN
}
\section{Formats}{
\preformatted{
GWAS p_values (tab delimited file)(SNP_IDs Trait1 Trait2 ...TraitN)
name       Trait1      Trait2    TraitN  
rs10000010 0.9122360 0.30088096 0.2332038
rs10000023 0.8642906 0.52064064 0.9243443 
rs10000030 0.2832705 0.99021664 0.8359339

SNPs Annotation (SNPsAnnotation)
name        Chromosome  Location  GENE_ID   Symbol    Orientation
rs1000313   1           15405489  23254     KIAA1026  +
rs1000533   1           168282491 9095      TBX19     +
rs1000731   1           231963491 27185     DISC1     +

Output:
name       Chromosome  Location GENE_ID Symbol Orientation   Trait1  
rs10000010          4  21618674   80333 KCNIP4           - 0.9122360
rs10000023          4  95733906     658 BMPR1B           + 0.8642906
rs10000030          4 103374154      NA   <NA>        <NA> 0.2832705 
}
}
\seealso{
\code{\link{genome_order}}
}
\examples{
## DEMO // WORKSPACE
data(demo,SNPsAnnotation)
all_data <- read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
}
\keyword{ gwas }
\keyword{ pvalues }
