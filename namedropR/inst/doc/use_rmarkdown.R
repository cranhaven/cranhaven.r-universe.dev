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


## -----------------------------------------------------------------------------
dplyr::glimpse(bib)

## ---- eval=FALSE--------------------------------------------------------------
#  knitr::include_graphics(drop_name(bib, cite_key = "doe2021", export_as = "png"))

## ---- eval=FALSE--------------------------------------------------------------
#  # all entries
#  knitr::include_graphics(drop_name(bib, export_as = "png"))
#  
#  # some entries
#  knitr::include_graphics(drop_name(bib, cite_key = c("doe2021", "fantastic1998") export_as = "png"))
#  

## ---- eval=FALSE--------------------------------------------------------------
#  htmltools::img(
#    src = drop_name(bib, cite_key = "active2021", style = "classic", export_as = "png")
#    )

## ---- eval=FALSE--------------------------------------------------------------
#  htmltools::div(
#    style = "box-shadow: 5px 5px 6px grey;border: solid;border-color: #EEEEEE; border-width: 1px;",
#    htmltools::img(src = drop_name(bib, cite_key = "fantastic1998", export_as = "png"))
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  htmltools::includeHTML(
#    drop_name(bib,
#      cite_key = "active2021",
#      export_as = "html",
#      use_xaringan = TRUE
#    )
#  )

