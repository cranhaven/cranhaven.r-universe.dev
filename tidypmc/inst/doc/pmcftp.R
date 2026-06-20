## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "# "
)

## ----load---------------------------------------------------------------------
library(readr)
pmcfile <- system.file("extdata/PMC6358576_PMC6358589.xml", package = "tidypmc")
pmc <- read_lines(pmcfile)

## ----startnode----------------------------------------------------------------
a1 <- grep("^<article ", pmc)
head(a1)
n <- length(a1)
n

## ----read1, echo=-1-------------------------------------------------------------------------------
options(width=100)
library(xml2)
x1 <- paste(pmc[2:29], collapse="\n")
doc <- read_xml(x1)
doc

## ----loop-----------------------------------------------------------------------------------------
library(tidypmc)
a1 <- c(a1, length(pmc))
met1 <- vector("list", n)
txt1 <- vector("list", n)
for(i in seq_len(n)){
  doc <- read_xml(paste(pmc[a1[i]:(a1[i+1]-1)], collapse="\n"))
  m1 <- pmc_metadata(doc)
  id <- m1$PMCID
  message("Parsing ", i, ". ", id)
  met1[[i]] <- m1
  txt1[[i]] <- pmc_text(doc)
}

## ----combine, echo=-1, message=FALSE--------------------------------------------------------------
options(width=100)
library(dplyr)
met <- bind_rows(met1)
names(txt1) <- met$PMCID
txt <- bind_rows(txt1, .id="PMCID")
met
txt

