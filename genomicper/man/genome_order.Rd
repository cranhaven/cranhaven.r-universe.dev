\name{genome_order}
\alias{genome_order}
\title{
Genome Order
}
\description{
Orders the SNPs according to their genomic location
}
\usage{
genome_order(all_data = "")
}
\arguments{
  \item{all_data}{
SNPs to Genes Annotation and Trait Pvalues of Association\cr
all_data = (read_pvals output) OR matrix/dataframe.\cr
}
}
\details{
\preformatted{
Input Columns with "*" must be included for analysis
 
NOTE: Trait p-values must start at Column #7

# *Column 1: "name" (SNP_IDs - any SNP ID as character)
# *Column 2: Chromosome Location
# *Column 3: SNP Location 
# *Column 4: Gene ID 
# Column 5: Symbol (OR Annotation Field 1)
# Column 6: Annotaiton Field 2
# *Column 7: First trait pvalues of association
# Column N: Next trait pvalues of association
# Example Input Data:
name       Chromosome  Location GENE_ID Symbol Orientation abpi
rs10000010          4  21618674   80333 KCNIP4           - 0.91
rs10000023          4  95733906     658 BMPR1B           + 0.86
rs10000092          4  21895517   80333 KCNIP4           - 0.20
rs1000022          13  100461219 171425 CLYBL            + 0.26
rs10000300          4  40466547   54502 RBM47            - 0.58
}
}
\value{
\item{ordered_alldata}{
SNPs annotated to Genes and Trait p-values
}
\item{gs_locs}{
Gene annotations, location indexes and number of observations
}
}
\section{Format}{
\preformatted{	
	SNPs annotated to Genes and Trait p-values
#ordered_alldata[1:5,1:8]
name   Chromosome	Location GENE_ID  Symbol Orientation Trait1 Trait2
rs3934834	1  		1005806  NA     <NA>        <NA>  	 0.97  0.92
rs3737728	1		1021415  54991 	C1orf159      - 	 0.91  0.69
rs6687776	1		1030565  54991  C1orf159      - 	 0.71  0.45
rs9651273	1		1031540  54991  C1orf159      - 	 0.22  0.60
rs4970405	1		1048955  54991  C1orf159      - 	 0.77  0.56

	Gene annotations, location indexes and number of observations
#gs_locs[1:5,]
#      Symbol     Chromosome Location    Gene_ID Start_Indx Observations
# [1,] "A1BG"     "19"       "58864479"  "1"     "293976"   "1"         
# [2,] "A2M"      "12"       "9232268"   "2"     "215264"   "5"         
# [3,] "NAT1"     "8"        "18077310"  "9"     "151804"   "1"         
# [4,] "NAT2"     "8"        "18257280"  "10"    "151831"   "2"         
# [5,] "SERPINA3" "14"       "95080803"  "12"    "249519"   "2"         
}
}
\seealso{
\code{\link{read2_paths}}
}
\examples{

## DEMO WORKSPACE 

data(demo,SNPsAnnotation)
all_data<-read_pvals(data_name=demo,snps_ann=SNPsAnnotation)
# GENOME ORDER
genome_results <- genome_order(all_data=all_data)
	ordered_alldata <- genome_results$ordered_alldata
	gs_locs <- genome_results$gs_locs
}
\keyword{ genome_order }