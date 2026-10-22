## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(querychat)
# library(palmerpenguins)

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(
#   orders, "orders",
#   data_dict = "data-dict.yaml"
# )
# qc$add_table(customers, "customers")

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(
#   penguins,
#   data_description = "data_description.md"
# )

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(
#   penguins,
#   extra_instructions = "instructions.md"
# )

## -----------------------------------------------------------------------------
# instructions <- "
# - Use British spelling conventions
# - Stay on topic and only discuss the data
# - Refuse to answer unrelated questions
# "
# 
# qc <- QueryChat$new(
#   penguins,
#   extra_instructions = instructions
# )

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(penguins)
# cat(qc$system_prompt)

