## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(reptiledbr)

# Define a vector of species names to search
species_names <- c(
  "Lachesis muta",
  "Python bivittatus",
  "Crotalus atrox",
  "Bothrops atrox insularis", # Trinomial (with subspecies) - will generate an error
  "Lachesis sp",
  "Crotalus atroxx", # Intentional typo
  "Anolis liogaster",
  "Lachesis mutta", # Intentional typo
  "Python bivitatus" # Intentional typo
)

# Perform exact search
reptiledbr_exact(species_names)

## -----------------------------------------------------------------------------
# Perform search with partial matching
reptiledbr_partial(species_names)

## -----------------------------------------------------------------------------
# Define a list of valid species
subspecies_names <- c("Lachesis muta", 
                     "Anilius scytale",
                     "Anolis bahorucoensis",
                     "Anolis baleatus",
                     "Crotalus atrox",
                     "Anolis barahonae",
                     "Anolis bremeri")

# Exact search (without fuzzy matching)
search_reptiledbr(subspecies_names, use_fuzzy = FALSE)

## -----------------------------------------------------------------------------
# List all subspecies for the found species
search_reptiledbr(subspecies_names, use_fuzzy = FALSE) |> 
  list_subspecies_reptiledbr()

## -----------------------------------------------------------------------------
# Partial matching search followed by subspecies listing
reptiledbr_partial(c("Lachesis mutta", 
                    "Python bivitatus",
                    "Anolis barahonae")) |> 
  list_subspecies_reptiledbr()

