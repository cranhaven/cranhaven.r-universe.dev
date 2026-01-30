## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

if (!requireNamespace("rdflib", 
                      quietly = TRUE)) {
  stop("Please install 'rdflib' to run this vignette.")
}

## ----definegdpdataset---------------------------------------------------------
library(dataset)

gdp <- defined(
  c(2355, 2592, 2884),
  label = "Gross Domestic Product",
  unit = "CP_MEUR",
  concept = "http://data.europa.eu/83i/aa/GDP"
)

geo <- defined(
  rep("AD", 3),
  label = "Geopolitical Entity",
  concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
  namespace = "https://www.geonames.org/countries/$1/"
)

gdp
geo

## ----smalldatasetexample------------------------------------------------------
small_dataset <- dataset_df(
  geo = geo,
  gdp = gdp,
  identifier = c(gdp = "http://example.com/dataset#gdp"),
  dataset_bibentry = dublincore(
    title = "Small GDP Dataset",
    creator = person("Jane", "Doe", role = "aut"),
    publisher = "Small Repository",
    subject = "Gross Domestic Product"
  )
)

small_dataset

## ----dublincoremetadata-------------------------------------------------------
as_dublincore(small_dataset)

## ----triplesexample-----------------------------------------------------------
triples <- dataset_to_triples(small_dataset,
  format = "nt"
)
triples

## ----ntexample----------------------------------------------------------------
mycon <- tempfile("my_dataset", 
                  fileext = "nt")
my_description <- describe(x = small_dataset, 
                           con = mycon)

# Only three statements are shown:
readLines(mycon)[c(4, 8, 12)]

## ----provenancexample---------------------------------------------------------
## Show two lines of provenance:
provenance(small_dataset)[c(6, 7)]

## ----smalldf------------------------------------------------------------------
small_df <- as.data.frame(small_dataset, 
              strip_attributes = FALSE)

attr(small_dataset, "subject")

## ----smalltbl-----------------------------------------------------------------
small_tbl <- as_tibble(
  small_dataset, 
  strip_attributes = TRUE)

small_tbl

