## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
# Start scryr and helpers
library(dplyr)
library(scryr)

## -----------------------------------------------------------------------------
# Legendary vampires
vampires <- scry_cards("t:vampire t:legend")

# There are many, many columns
print(vampires)

## -----------------------------------------------------------------------------
# Get Anje's related cards
vampires %>%
  filter(name == "Anje, Maid of Dishonor") %>%
  pull(all_parts)

# Get Anje's color identity
vampires %>%
  filter(name == "Anje Falkenrath") %>%
  pull(color_identity)

## -----------------------------------------------------------------------------
# Using an ID
scry_card("913dd06f-ed2f-4128-9c9d-9cd0d8a55425")$name

# Using a name
scry_card_name("Anje Falkenrath")$name

# Using a collector number and a set
scry_card_number(37, "c19")$name

# Just get a random vampire commander
scry_card_random("t:vampire t:legend")$name

## -----------------------------------------------------------------------------
# There she is
autocomplete_name("falken")[12]

## -----------------------------------------------------------------------------
# Get all sets
scry_sets()

# Get a single set with an ID
scry_set("vow")

## -----------------------------------------------------------------------------
# Get information from a catalog
head(scry_catalog("keyword-actions"))

# Get rulings for a card
scry_ruling("913dd06f-ed2f-4128-9c9d-9cd0d8a55425")

# Get information about symbols
scry_symbols()

# Parse mana costs
parse_cost("2g2")$cost

# Get names of bulk files
scry_bulk_files()$name

# Download (and parse) bulk rulings
scry_bulk_file("Rulings")

