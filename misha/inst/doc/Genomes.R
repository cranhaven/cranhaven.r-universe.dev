## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(misha)

## ----eval = FALSE-------------------------------------------------------------
#  ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/hg19"
#  gdb.create(
#      "hg19",
#      paste(ftp, "chromosomes", paste0("chr", c(1:22, "X", "Y", "M"), ".fa.gz"), sep = "/"),
#      paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#      paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#      c(
#          "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#          "refseq", "protAcc", "description", "rfamAcc",
#          "tRnaName"
#      )
#  )
#  gdb.init("hg19")

## ----eval = FALSE-------------------------------------------------------------
#  ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/hg38"
#  gdb.create(
#      "hg38",
#      paste(ftp, "chromosomes", paste0("chr", c(1:22, "X", "Y", "M"), ".fa.gz"), sep = "/"),
#      paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#      paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#      c(
#          "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#          "refseq", "protAcc", "description", "rfamAcc",
#          "tRnaName"
#      )
#  )
#  gdb.init("hg19")

## ----eval = FALSE-------------------------------------------------------------
#  ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/mm9"
#  gdb.create(
#      "mm9",
#      paste(ftp, "chromosomes", paste0("chr", c(1:19, "X", "Y", "M"), ".fa.gz"), sep = "/"),
#      paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#      paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#      c(
#          "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#          "refseq", "protAcc", "description"
#      )
#  )
#  gdb.init("mm9")

## ----eval = FALSE-------------------------------------------------------------
#  ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/mm10"
#  gdb.create(
#      "mm10",
#      paste(ftp, "chromosomes", paste0("chr", c(1:19, "X", "Y", "M"), ".fa.gz"), sep = "/"),
#      paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#      paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#      c(
#          "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#          "refseq", "protAcc", "description", "rfamAcc",
#          "tRnaName"
#      )
#  )
#  gdb.init("mm10")

## ----eval = FALSE-------------------------------------------------------------
#  ftp <- "ftp://hgdownload.soe.ucsc.edu/goldenPath/mm39"
#  gdb.create(
#      "mm39",
#      paste(ftp, "chromosomes", paste0("chr", c(1:19, "X", "Y", "M"), ".fa.gz"), sep = "/"),
#      paste(ftp, "database/knownGene.txt.gz", sep = "/"),
#      paste(ftp, "database/kgXref.txt.gz", sep = "/"),
#      c(
#          "kgID", "mRNA", "spID", "spDisplayID", "geneSymbol",
#          "refseq", "protAcc", "description", "rfamAcc",
#          "tRnaName"
#      )
#  )
#  gdb.init("mm39")

