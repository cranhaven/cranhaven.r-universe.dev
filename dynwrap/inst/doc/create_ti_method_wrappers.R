## ----setup, echo = FALSE, message = FALSE-------------------------------------
library(dplyr)
library(tibble)
library(dynwrap)

## ----echo = FALSE-------------------------------------------------------------
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "A", "B", 1, FALSE,
  "B", "C", 2, FALSE,
  "B", "D", 1, FALSE,
  "C", "E", 1, FALSE,
  "C", "F", 1.5, FALSE
)
milestone_network$from <- paste0("Milestone_", milestone_network$from)
milestone_network$to <- paste0("Milestone_", milestone_network$to)
milestone_ids <- paste0("Milestone_", c("A", "B", "C", "D", "E", "F"))
milestone_network

## ----echo = FALSE-------------------------------------------------------------
cell_ids <- paste0("Cell_", letters)
progressions <- milestone_network %>% 
  sample_n(length(cell_ids), replace = TRUE, weight = length) %>% 
  mutate(
    cell_id = cell_ids,
    percentage = runif(n())
  ) %>% 
  select(cell_id, from, to, percentage)
milestone_percentages <- dynwrap::convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network, progressions) %>% arrange(cell_id, milestone_id)
head(milestone_percentages, 10)

## ----echo = FALSE-------------------------------------------------------------
head(progressions, 10)

## ----echo = FALSE-------------------------------------------------------------
divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "Divergence_1", "Milestone_B", TRUE,
  "Divergence_1", "Milestone_C", FALSE,
  "Divergence_1", "Milestone_D", FALSE
)
head(divergence_regions)

## -----------------------------------------------------------------------------
trajectory <- wrap_data(cell_ids = cell_ids) %>% 
  add_trajectory(
    milestone_network = milestone_network, 
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

