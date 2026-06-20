## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "# "
)

## ----epmc_ftxt------------------------------------------------------------------------------------
library(tidypmc)
doc <- pmc_xml("PMC2231364")
doc

## ----pmc_text, message=FALSE, echo=-1-------------------------------------------------------------
options(width=100)
library(dplyr)
txt <- pmc_text(doc)
txt
count(txt, section)

## ----pmc_caption, echo=-1-------------------------------------------------------------------------
options(width=100)
cap1 <- pmc_caption(doc)
filter(cap1, sentence == 1)

## ----pmc_table, echo=-1---------------------------------------------------------------------------
options(width=100)
tab1 <- pmc_table(doc)
sapply(tab1, nrow)
tab1[[1]]

## ----attributes-----------------------------------------------------------------------------------
attributes(tab1[[1]])

## ----collapserows, echo=-1------------------------------------------------------------------------
options(width=100)
collapse_rows(tab1, na.string="-")

## ----pmc_ref, echo=-1-----------------------------------------------------------------------------
options(width=100)
ref1 <- pmc_reference(doc)
ref1

## ----pmc_metadata---------------------------------------------------------------------------------
pmc_metadata(doc)

## ----separate_text, echo=-1-----------------------------------------------------------------------
options(width=100)
separate_text(txt, "[ATCGN]{5,}")

## ----separate_refs, echo=-1-----------------------------------------------------------------------
options(width=100)
x <- separate_refs(txt)
x
filter(x, id == 8)

## ----locus_tags, echo=-1--------------------------------------------------------------------------
options(width=100)
collapse_rows(tab1, na="-") %>%
  separate_tags("YPO")

## ----catchar--------------------------------------------------------------------------------------
library(xml2)
refs <- xml_find_all(doc, "//ref")
refs[1]
cat(as.character(refs[1]))

## ----pmcdoc1, message=FALSE-----------------------------------------------------------------------
# doc1 <- pmc_xml("PMC6385181")
doc1 <- read_xml(system.file("extdata/PMC6385181.xml", package = "tidypmc"))
gsub(".*\\. ", "", xml_text( xml_find_all(doc1, "//sec/p"))[2])

## ----bib------------------------------------------------------------------------------------------
bib <- xml_find_all(doc1, "//xref[@ref-type='bibr']")
bib[1]
xml_text(bib) <- paste0(" [", xml_text(bib), "]")
bib[1]

## ----pmc_text2, message=FALSE---------------------------------------------------------------------
gsub(".*\\. ", "", xml_text( xml_find_all(doc1, "//sec/p"))[2])

## ----italicgenes----------------------------------------------------------------------------------
library(tibble)
x <- xml_name(xml_find_all(doc, "//*"))
tibble(tag=x) %>%
  count(tag, sort=TRUE)
it <- xml_text(xml_find_all(doc, "//sec//p//italic"), trim=TRUE)
it2 <- tibble(italic=it) %>%
  count(italic, sort=TRUE)
it2
filter(it2, nchar(italic) == 3)
separate_text(txt, c("fur", "cys", "hmu", "ybt", "yfe", "yfu", "ymt"))

