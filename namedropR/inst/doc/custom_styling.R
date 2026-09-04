## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(namedropR)

## ---- message=FALSE, warning=FALSE--------------------------------------------
# load required packages
library(bib2df)
library(dplyr)

# read the file
bib_file <- system.file("testdata", "library.bib", package = "namedropR")
bib <- bib2df::bib2df(bib_file) %>% 
  select(BIBTEXKEY, AUTHOR, YEAR, TITLE, JOURNAL, URL)


## ---- eval=FALSE--------------------------------------------------------------
#  namedropR::drop_name(
#    bib,
#    cite_key = "fantastic1998",
#    export_as = "png",
#    style = "clean",
#    author_color = "FF00FF",
#    title_font = "Playfair Display"
#  )
#  

## ----eval=FALSE---------------------------------------------------------------
#  namedropR::drop_name(
#    bib,
#    cite_key = "fantastic1998",
#    export_as = "png",
#    style = "modern",
#    qr_size = 150,
#    qr_color = "#AAAAAA"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  namedropR::drop_name(
#    bib,
#    cite_key = "fantastic1998",
#    export_as = "png",
#    author_color = "#FF0000",
#    author_weight = "normal",
#    author_size = "12pt",
#    author_font = "Roboto",
#    title_color = "#00FF00",
#    title_weight = "bold",
#    title_size = "2.5rem",
#    title_font = "Playfair Display",
#    journal_color = "#0000FF",
#    journal_weight = "bold",
#    journal_size = "8pt",
#    journal_font = "Fira Sans",
#    qr_size = 150,
#    qr_color = "#AAAAAA"
#  )

