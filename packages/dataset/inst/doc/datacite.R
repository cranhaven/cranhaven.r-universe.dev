## ----setup-vignette, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)
iris_datacite <- datacite(
   Title = "Iris Dataset",
   Creator = person(family="Anderson", given ="Edgar", role = "aut"),
   Publisher = "American Iris Society",
   PublicationYear = 1935,
   Geolocation = "US",
   Language = "en")

## ----Bibtex, results='markup'-------------------------------------------------
print(iris_datacite, "Bibtex")

## ----as_datacite-example, results='markup'------------------------------------
as_datacite(iris_dataset, "list")

