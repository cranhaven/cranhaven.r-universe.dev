## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(reptiledbr)
species_list <- c(
  "Lachesis muta",
  "Python bivittatus",
  "Crotalus atrox",
  "Bothrops atrox insularis", # Trinomial (con subespecie) - debería dar error
  "Lachesis sp"
)

# 
reptile_data <- get_reptiledb_data(species_list)

# 
reptile_data

# Format all attributes
all_attributes <- format_all_attributes(reptile_data)

# Access specific attribute categories
all_attributes$distribution
all_attributes$common_names

## -----------------------------------------------------------------------------
# Extract only synonyms, higher taxa, and common names
selected_info <- format_selected_attributes(
  reptile_data = reptile_data,
  attributes = c("Synonym", "Higher Taxa", "Common Names"),
  quiet = TRUE
)

# Access the selected information
selected_info$Synonym
selected_info$`Higher Taxa`
selected_info$`Common Names`

## -----------------------------------------------------------------------------
# Find all venomous species
all_attributes$comments |> 
  dplyr::filter(stringr::str_detect(comment_detail, "Venomous"))


# Extract distribution information for a specific species
all_attributes$distribution |>
  dplyr::filter(input_name == "Crotalus atrox")

