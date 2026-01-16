## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(perumammals)

## ----install, eval=FALSE------------------------------------------------------
# # Using pak (recommended)
# pak::pak("PaulESantos/perumammals")
# 
# # Or using remotes
# remotes::install_github("PaulESantos/perumammals")

## ----setup--------------------------------------------------------------------
library(perumammals)

## ----datasets-----------------------------------------------------------------
# Main species backbone
data(peru_mammals)
head(peru_mammals)


## ----validate-basic-----------------------------------------------------------
# Single species
species_list <- c(
  "Puma concolor",           # Valid name
  "Tremarctos ornatus",      # Valid name  
  "Panthera onca",           # Valid name
  "Lycalopex sechurae",      # Valid name
  "Odocoileus virginianus",  # Valid name
  "Puma concolar"            # Misspelled
)

results <- validate_peru_mammals(species_list)
results

## ----is-peru------------------------------------------------------------------
# Returns TRUE/FALSE
is_peru_mammal(species_list)

## ----endemic------------------------------------------------------------------
# Check which species are endemic to Peru
species_list <- c("Thomasomys notatus", "Tremarctos ornatus", "Eptesicus mochica", "Puma concolar")

is_endemic_peru(species_list)

# Get endemic status as character
endemic_status <- ifelse(
  is_endemic_peru(species_list) == "Endemic to Peru",
  "Endémica",
  "No endémica"
)
endemic_status

## ----match-quality------------------------------------------------------------
# Get match quality levels
match_quality_peru(species_list)

## ----dataframe, warning=FALSE, message=FALSE----------------------------------
library(dplyr)

# Create a sample dataset
my_data <- tibble(
  species = species_list,
  abundance = c(5, 3, 2, 8)
)

# Add validation results
my_data_validated <- my_data |> 
  mutate(
    in_peru = is_peru_mammal(species),
    endemic = is_endemic_peru(species),
    match_quality = match_quality_peru(species)
  )

my_data_validated

## ----families-----------------------------------------------------------------
# Get summary of all families
families <- pm_list_families()
families

# Families with highest species richness
families |> 
  arrange(desc(n_species)) |> 
  head(10)

## ----family-filter------------------------------------------------------------
# Get summary for bat species (Phyllostomidae)
pm_list_families() |> 
  filter(family == "Phyllostomidae")

# Get species list for a specific family
 pm_species(family = "Phyllostomidae")

## ----endemic-list-------------------------------------------------------------
# List all endemic species
endemic_mammals <- pm_species(endemic = TRUE)
endemic_mammals

# Endemic species by family
endemic_mammals |> 
  group_by(family) |> 
  summarise(n_species = n_distinct(scientific_name)) |> 
  arrange(desc(n_species)) |> 
  head(10)

## ----endemic-ecoregion--------------------------------------------------------
# Compare endemism across ecoregions
endemic_rate <- pm_list_ecoregions(include_endemic = TRUE)
endemic_rate

# Endemic species in Yungas
pm_by_ecoregion(ecoregion = "YUN", endemic = TRUE)


## ----ecoregion-dist-----------------------------------------------------------

# Count species per ecoregion
pm_list_ecoregions()

## ----wide-distribution--------------------------------------------------------
# Species occurring in most ecoregions
peru_mammals_ecoregions |> 
  count(scientific_name, name = "n_ecoregions") |> 
  arrange(desc(n_ecoregions)) |> 
  top_n(10)

## ----cleaning-----------------------------------------------------------------
# Messy species list from field observations
field_data <- tibble(
  location = c("Manu", "Tambopata", "Paracas", "Cusco", "Lima"),
  species_name = c(
    "puma concolor",           # lowercase
    "Tremarctos ornatu",       # missing 's'
    "Otaria flavescens",       # marine mammal
    "Lycalopex sechure",       # missing 'ae'
    "Unknown bat"              # invalid
  ),
  count = c(2, 1, 15, 3, 8)
)

# Validate and clean
field_data_clean <- field_data %>%
  mutate(
    # Validate names
    validated = validate_peru_mammals(species_name)$Matched.Name,
    # Check if in Peru
    in_checklist = is_peru_mammal(species_name),
    # Get match quality
    quality = match_quality_peru(species_name)
  )

field_data_clean

## ----endemic-summary----------------------------------------------------------
# Get all endemic mammals
endemic_species <- pm_species(endemic = TRUE)
endemic_species
# Total endemic species by order
endemic_species |> 
  count(order, name = "n_endemic") |> 
  arrange(desc(n_endemic))

## ----ecoregion-analysis-------------------------------------------------------
# Focus on Selva Baja (Amazon lowlands)

selva_baja_species <- pm_by_ecoregion(ecoregion = "SB")
selva_baja_species

# Endemic species in Selva Baja
pm_by_ecoregion(ecoregion = "SB", endemic = TRUE) |> 
  count(family, name = "n_species") |> 
  arrange(desc(n_species))

## ----fuzzy-details------------------------------------------------------------
# Examples of different match levels
test_names <- c(
  "Puma concolor",              # Level: Exact
  "Tremarctos ornatus Cuvier",  
  "Lycalopex sechure",          # Level: Genus + fuzzy species
  "Lyclopex sechurae",          # Level: Fuzzy genus + exact species
  "Panthera onca"               # Level: Exact
)

validate_peru_mammals(test_names) |> 
  select(Orig.Name, Matched.Name, matched)

## ----citation, eval=FALSE-----------------------------------------------------
# citation("perumammals")

