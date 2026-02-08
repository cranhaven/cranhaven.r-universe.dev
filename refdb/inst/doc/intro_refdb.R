## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 7
)

## ----setup, eval=FALSE--------------------------------------------------------
# library(refdb)
# library(tidyverse)

## ----echo=FALSE, message=FALSE------------------------------------------------
library(refdb)
library(readr)
library(ggplot2)
library(dplyr)
library(forcats)

## ----include=FALSE------------------------------------------------------------
lib <- read_csv(system.file("extdata", "ephem.csv", package = "refdb"))

## ----eval=FALSE---------------------------------------------------------------
# lib <- read_csv("my_path/ephem.csv")

## -----------------------------------------------------------------------------
lib <- refdb_set_fields(lib,
                        taxonomy = c(family = "family_name",
                                     genus = "genus_name",
                                     species = "species_name"),
                        sequence = "DNA_seq",
                        marker = "marker")

## -----------------------------------------------------------------------------
refdb_get_fields(lib)

## -----------------------------------------------------------------------------
lib [91, 1:4]

## -----------------------------------------------------------------------------
lib_na <- refdb_clean_tax_NA(lib)
lib_na [91, 1:4]

## -----------------------------------------------------------------------------
lib [32, 1:4]

## -----------------------------------------------------------------------------
lib_na_extra <- refdb_clean_tax_remove_extra(lib_na)
lib_na_extra [32, 1:4]

## -----------------------------------------------------------------------------
head(lib)

lib_nogap <- refdb_clean_seq_remove_gaps(lib)
head(lib_nogap)

## ----fig.width=4.5, fig.height=3.5--------------------------------------------
refdb_plot_seqlen_hist(lib)

## -----------------------------------------------------------------------------
lib_long <- refdb_filter_seq_length(lib, min_len = 500)

nrow(lib)
nrow(lib_long)

## -----------------------------------------------------------------------------
refdb_filter_seq_primer(lib, primer_forward = "GGWACWGGWTGAACWGTWTAYCCYCC")

## -----------------------------------------------------------------------------
refdb_filter_seq_primer(lib, primer_forward = "GGWACWGGWTGAACWGTWTAYCCYCC", max_error_forward = 0.2)

## -----------------------------------------------------------------------------
lib %>% 
  group_by(family_name) %>% 
  count() %>% 
  ggplot(aes(fct_reorder(family_name, n, .desc = TRUE), n)) +
  geom_col() +
  xlab("Family") +
  ylab("Number of records") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

## ----message=FALSE, warning=FALSE---------------------------------------------
refdb_plot_tax_treemap(lib)

## ----message=FALSE, warning=FALSE---------------------------------------------
refdb_plot_tax_tree(lib)

## ----message=FALSE, warning=FALSE---------------------------------------------
lib <- refdb_set_fields(lib, latitude = "lat", longitude = "lon")
refdb_plot_map(lib)

## ----eval=FALSE---------------------------------------------------------------
# refdb_report(lib)

## ----eval=FALSE---------------------------------------------------------------
# write_csv(lib, "my_reference_library.csv")

## ----eval=FALSE---------------------------------------------------------------
# refdb_write_fields(lib, "my_reference_fields.yml")

## ----eval=FALSE---------------------------------------------------------------
# lib <- read_csv("my_reference_library.csv")
# lib <- refdb_set_fields(lib, config_yaml = "my_reference_fields.yml")

