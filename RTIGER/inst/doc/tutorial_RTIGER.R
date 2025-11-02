## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ----echo= TRUE, eval=FALSE---------------------------------------------------
#  if (!require("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  BiocManager::install(version = "3.14")
#  
#  BiocManager::install(c("GenomicRanges", "GenomeInfoDb", "TailRank", "IRanges", "Gviz"))

## ----echo= TRUE, eval=FALSE---------------------------------------------------
#  install.packages("RTIGER")

## ----echo= TRUE, eval=FALSE---------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("rfael0cm/RTIGER", ref = 'main')

## ----echo=FALSE, results='asis'-----------------------------------------------
library(knitr)
chrom = paste0(rep('Chr'), c(1,1,2,2,2,3))
pos = c("37388", "71348", "18057", "38554", "75348", "32210")
ref = c("C", "T", "A", "G", "A", "T")
refC = c(0, 1, 0, 0 , 1, 2)
alt = c("T", "G", "C", "A", "T", "G")
altC = c(2, 3, 1, 2 , 0, 0)
df = data.frame(chrom, pos, ref, refC, alt, altC)
colnames(df) <- NULL
kable(df, align = "l", caption='Example allele frequency file', justify='centred', width='minimum')

## ----echo= TRUE, eval=FALSE---------------------------------------------------
#  library(RTIGER)
#  setupJulia()

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  sourceJulia()

## ----echo = TRUE--------------------------------------------------------------
# Get paths to example allele count files originating from a
# cross between Col-0 and Ler accession of the A.thaliana
file_paths = list.files(system.file("extdata",  package = "RTIGER"), full.names = TRUE)

# Get sample names
sampleIDs <- basename(file_paths)

# Create the expDesign object
expDesign = data.frame(files=file_paths, name=sampleIDs)

print(expDesign)

## ----echo=TRUE----------------------------------------------------------------
# Get chromosome lengths for the example data included in the package
chr_len <- RTIGER::ATseqlengths
names(chr_len) <- c('Chr1' , 'Chr2', 'Chr3', 'Chr4', 'Chr5')
print(chr_len)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  myres = RTIGER(expDesign = expDesign,
#                 outputdir = "/PATH/TO/OUTPUT/DIRECTORY",
#                 seqlengths = chr_len,
#                 rigidity = 200)

## ----fig.cap="Effect of different R values on CO identificaion. The top panel shows the allele count for parent 1 (red) and parent 2 (blue) at the marker positions on a chormosomes. Remaining panels show the annotation of genomic regions by RTIGER for different values of R. Here, red blocks are regions that are homozygous for parent 1, purple blocks are for heterozygous regions, and blue blocks are for regions homozygous for parent 2.", out.width = "300px", fig.align='center', echo=FALSE----
# knitr::include_graphics("rtiger_out_bt2_3647_AA_run513_GTAGAGGA_S20_L007_PE_MQ05.pdf")
knitr::include_graphics("0001.jpg")

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  myres = RTIGER(expDesign = expDesign,
#                 outputdir = "PATH/TO/OUTPUT/DIR",
#                 seqlengths = chr_len,
#                 rigidity = 200,
#                 nstates=2)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
sessionInfo()

