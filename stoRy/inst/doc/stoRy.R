## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("stoRy")

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages("devtools")
#  # devtools::install_github("theme-ontology/stoRy")

## ---- eval = FALSE------------------------------------------------------------
#  library(stoRy)

## ---- eval = FALSE------------------------------------------------------------
#  help(package = "stoRy")

## ---- eval = FALSE------------------------------------------------------------
#  ?get_similar_stories

## ---- eval = FALSE------------------------------------------------------------
#  citation("stoRy")

## ---- eval = FALSE------------------------------------------------------------
#  which_lto()

## ---- eval = FALSE------------------------------------------------------------
#  set_lto(version = "demo")

## ---- eval = FALSE------------------------------------------------------------
#  print_lto()

## ---- eval = FALSE------------------------------------------------------------
#  ?`lto-demo`

## ---- eval = FALSE------------------------------------------------------------
#  demo_metadata_tbl <- clone_active_metadata_tbl()
#  demo_themes_tbl <- clone_active_themes_tbl()
#  demo_stories_tbl <- clone_active_stories_tbl()
#  demo_collections_tbl <- clone_active_collections_tbl()

## ---- eval = FALSE------------------------------------------------------------
#  theme <- Theme$new(theme_name = "mass hysteria")

## ---- eval = FALSE------------------------------------------------------------
#  # Print stylized text:
#  theme
#  
#  # Print in plain text .th.txt file format:
#  theme$print(canonical = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  theme$annotations()

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages("dplyr")
#  suppressMessages(library(dplyr))
#  # install.packages("stringr")
#  library(stringr)
#  demo_themes_tbl <- clone_active_themes_tbl()
#  demo_themes_tbl %>% filter(str_detect(theme_name, "mass"))

## ---- eval = FALSE------------------------------------------------------------
#  story <- Story$new(story_id = "tz1959e1x22")

## ---- eval = FALSE------------------------------------------------------------
#  # In stylized text format:
#  story
#  
#  # In plain text .st.txt file format:
#  story$print(canonical = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  themes <- story$themes()
#  themes

## ---- eval = FALSE------------------------------------------------------------
#  title <- "The Monsters Are Due on Maple Street"
#  demo_stories_tbl <- clone_active_stories_tbl()
#  story_id <- demo_stories_tbl %>% filter(title == !!title) %>% pull(story_id)
#  story_id

## ---- eval = FALSE------------------------------------------------------------
#  story$collections()

## ---- eval = FALSE------------------------------------------------------------
#  collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")

## ---- eval = FALSE------------------------------------------------------------
#  # Print stylized text:
#  collection
#  
#  # Print in plain text .st.txt file format:
#  collection$print(canonical = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  demo_collections_tbl <- clone_active_collections_tbl()
#  demo_collections_tbl

## ---- eval = FALSE------------------------------------------------------------
#  collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
#  result_tbl <- get_featured_themes(collection)
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  result_tbl <- get_featured_themes()
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  test_collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
#  result_tbl <- get_enriched_themes(test_collection)
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  result_tbl <- get_enriched_themes(test_collection, weights = list(choice = 1, major = 1, minor = 0))
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  query_story <- Story$new(story_id = "tz1959e1x22")
#  result_tbl <- get_similar_stories(query_story)
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(123)
#  result_tbl <- get_story_clusters()
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 3
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 5
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 7
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 10
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 11
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 13
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  lto_version_statuses()

## ---- eval = FALSE------------------------------------------------------------
#  configure_lto(version = "latest")

## ---- eval = FALSE------------------------------------------------------------
#  set_lto(version = "latest")

## ---- eval = FALSE------------------------------------------------------------
#  which_lto()

