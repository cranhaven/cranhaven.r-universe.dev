## -----------------------------------------------------------------------------
library(rfars)
library(dplyr)
library(stringr)

## ----eval=F, echo=T-----------------------------------------------------------
#  View(rfars::fars_codebook)
#  View(rfars::gescrss_codebook)

## -----------------------------------------------------------------------------
rfars::fars_codebook %>%
  filter(if_any(everything(), ~ str_detect(as.character(.), "speed"))) %>%
  DT::datatable(caption = "Codebook records pertaining to speed")

