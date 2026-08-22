## ----fig setup, include=FALSE-------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
knitr::opts_chunk$set(fig.width = 12, fig.height = 6)

## ----INSTALL LPR, eval = FALSE------------------------------------------------
# # Install the LAPOP Lab package:
# devtools::install_github("https://github.com/lapop-central/lapop",
#                          force=TRUE,
#                          build_vignettes = TRUE)

## ----LOAD LPR-----------------------------------------------------------------
library(lapop)
# Note: no need for lapop_fonts() 
# because we are not plotting figures

## ----LOAD DATA CHUNK, eval = FALSE--------------------------------------------
# # READSTATA13 PACKAGE (RECOMMENDED FOR LABELS)
# data1 <- read.dta13("./BRA 2023 LAPOP AmericasBarometer (v1.0s).dta")
# names(attributes(data1))
# 
# # HAVEN PACKAGE (RECOMMENDED FOR PLOTTING)
# data2 <- read_dta("./BRA 2023 LAPOP AmericasBarometer (v1.0s).dta")
# names(attributes(data2))
# 
# # RIO PACKAGE (NOT RECOMMENDED)
# data3 <- import("./BRA 2023 LAPOP AmericasBarometer (v1.0s).dta")
# names(attributes(data3))

## ----QWORDING, eval = TRUE, warnings = F--------------------------------------
data(bra23)

# Extracting attributes from expasion.fields
names(attributes(bra23))
varlabels = attr(bra23, "expansion.fields")

## ----STREAMLINED APPROACH, eval = TRUE, warnings = F--------------------------
# Base R approach:
# The base R version uses lapply to iterate over the list and returns NULL when 
# the condition isn't met, then Filter(Negate(is.null), ...) removes those NULLs.
head(
  Filter(Negate(is.null), lapply(attr(bra23, "expansion.fields"), 
  function(x) {
    if (x[1] == "ing4") list(note_id = x[2], note_value = x[3])})), n = 3)

# purrr package approach
#  The purrr version uses map() to do the same, and also remove NULL results. 
tail(
  purrr::map_dfr(attr(bra23, "expansion.fields"), ~ {
  if (.x[1] == "ing4") 
    data.frame(
      note_id = .x[2],
      note_value = .x[3],
      stringsAsFactors = FALSE
    )
})
)


## ----NOTES EXTRACT, eval = TRUE, warnings = F---------------------------------
# Extract AB notes with lpr function
BRA23notes <- lpr_extract_notes(bra23)
head(BRA23notes, n = 10) # columns names
table(BRA23notes$note_id) # note information available!

## ----SET ATTRIBUTES, eval = TRUE, warnings = F, warning=F---------------------
# English
bra23<-lpr_set_attr(bra23, BRA23notes, verbose = F,
                    noteid = "note2", 
                    attribute_name = "qwording_en") 
# Spanish
bra23<-lpr_set_attr(bra23, BRA23notes, verbose = F,
                    noteid = "note1", 
                    attribute_name = "qwording_es")

# Portuguese
bra23<-lpr_set_attr(bra23, BRA23notes, verbose = F,
                    noteid = "note3", 
                    attribute_name = "qwording_pt") 

# Printing languages
attr(bra23$ing4, "qwording_en") # English
attr(bra23$ing4, "qwording_es") # Spanish
attr(bra23$ing4, "qwording_pt") # Portuguese

## ----ROs, eval = TRUE, warnings = F-------------------------------------------
bra23 <- lpr_set_ros(bra23) # Default English

bra23 <- lpr_set_ros(bra23, lang_id = "es", 
                     attribute_name = "respuestas") # Spanish

bra23 <- lpr_set_ros(bra23, lang_id = "pt", 
                     attribute_name = "ROsLabels_pt") # Portuguese

# Printing ROs
attr(bra23$ing4, "roslabel") # English
attr(bra23$ing4, "respuestas") # Spanish
attr(bra23$ing4, "ROsLabels_pt") # Portuguese

