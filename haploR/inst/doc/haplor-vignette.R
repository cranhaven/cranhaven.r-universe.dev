## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  install.packages("haploR", dependencies = TRUE)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  devtools::install_github("izhbannikov/haplor")

## ----echo=TRUE, message=FALSE-------------------------------------------------
library(haploR)
x <- queryHaploreg(query=c("rs10048158","rs4791078"))
x

## ----echo=TRUE, message=FALSE-------------------------------------------------
if(length(x)!=0) {
  subset.high.LD <- x[as.numeric(x$r2) > 0.9, c("rsID", "r2", "chr", "pos_hg38", "is_query_snp", "ref", "alt")]
  subset.high.LD
}

## ----echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  require(openxlsx)
#  write.xlsx(x=subset.high.LD, file="subset.high.LD.xlsx")

## ----echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  if(length(x)!=0) {
#      x[, c("Motifs", "rsID")]
#      x[, c("eQTL", "rsID")]
#  }

## ----echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  library(haploR)
#  x <- queryHaploreg(file=system.file("extdata/snps.txt", package = "haploR"))
#  x

## ----echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  library(haploR)
#  # Getting a list of existing studies:
#  studies <- getStudyList()
#  # Let us look at the first element:
#  if(!is.null(studies)) {
#      studies[[1]]
#      # Let us look at the second element:
#      studies[[2]]
#      # Query Hploreg to explore results from
#      # this study:
#      x <- queryHaploreg(study=studies[[1]])
#      x
#  }

## ----echo=TRUE, eval=FALSE, message=FALSE-------------------------------------
#  library(haploR)
#  tables <- getExtendedView(snp="rs10048158")
#  tables

## ----echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
#  library(haploR)
#  
#  # With RsIDs:
#  x <- queryRegulome(c("rs4791078","rs10048158"))
#  x
#  
#  # With region:
#  y <- queryRegulome("chr1:39492461-39492462")
#  y

## ----echo=TRUE----------------------------------------------------------------
sessionInfo()

