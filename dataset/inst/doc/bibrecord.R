## ----setupvignette, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----bibrecord----------------------------------------------------------------
person_jane <- person("Jane", "Doe", role = "cre")
person_alice <- person("Alice", "Smith", role = "dtm")

rec <- bibrecord(
  title = "GDP of Small States",
  author = list(person_jane),
  contributor = list(person_alice),
  publisher = "Tinystat",
  identifier = "doi:10.1234/example",
  date = "2023-05-01",
  subject = "Economic indicators"
)

## ----print--------------------------------------------------------------------
print(rec)

