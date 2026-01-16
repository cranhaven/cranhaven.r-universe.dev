## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(reptiledbr)

## -----------------------------------------------------------------------------
library(reptiledbr)

## -----------------------------------------------------------------------------
# Create a list of species to search
species_list <- c(
  "Lachesis muta",
  "Python bivittatus",
  "Crotalus atrox"
)

## -----------------------------------------------------------------------------
# Get data for these species
reptile_data <- get_reptiledb_data(species_list)

## -----------------------------------------------------------------------------
reptile_data

## -----------------------------------------------------------------------------
format_synonyms(reptile_data)

## -----------------------------------------------------------------------------
format_distribution(reptile_data)

## -----------------------------------------------------------------------------
format_higher_taxa(reptile_data)

## -----------------------------------------------------------------------------
format_subspecies(reptile_data)

## -----------------------------------------------------------------------------
format_common_names(reptile_data)

## -----------------------------------------------------------------------------
format_reproduction(reptile_data)

## -----------------------------------------------------------------------------
format_types(reptile_data)

## -----------------------------------------------------------------------------
format_diagnosis(reptile_data)

## -----------------------------------------------------------------------------
format_comments(reptile_data)

## -----------------------------------------------------------------------------
format_etymology(reptile_data)

## -----------------------------------------------------------------------------
format_references(reptile_data)

## -----------------------------------------------------------------------------
species_list <- c(
  "Lachesis muta",
  "Bothrops atrox insularis"  # Trinomial name
)
reptile_data <- get_reptiledb_data(species_list)

## -----------------------------------------------------------------------------
species_list <- c(
  "Lachesis muta",
  "Lachesis sp"  # Incomplete species name
)
reptile_data <- get_reptiledb_data(species_list)

