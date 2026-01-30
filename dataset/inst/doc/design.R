## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----dataframe----------------------------------------------------------------
data.frame(
  geo = c("LI", "SM"),
  CPI = c("0.8", "0.9"),
  GNI = c("8976", "9672")
)

## ----attributes---------------------------------------------------------------
x <- 2457
attr(x, "unit") <- "CP_MEUR"
attr(x, "concept") <- "http://data.europa.eu/83i/aa/GDP"

## -----------------------------------------------------------------------------
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
  concept = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
  namespace = "https://www.geonames.org/countries/$1/"
)

## -----------------------------------------------------------------------------
var_concept(gdp)
var_unit(gdp)
var_namespace(geo)

## ----smalldataset-------------------------------------------------------------
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

## ----bibrecord----------------------------------------------------------------
as_dublincore(small_dataset)
as_datacite(small_dataset)

## ----provenance---------------------------------------------------------------
provenance(small_dataset)

## ----description--------------------------------------------------------------
description_nt <- tempfile(pattern = "small_dataset", fileext = ".nt")
describe(small_dataset, description_nt)

# Only a few lines shown:
readLines(description_nt)[5:8]

## ----eurogpd------------------------------------------------------------------
euro_gdp <- defined(
  c(2355, 2592),
  label = "Gross Domestic Product",
  unit = "CP_MEUR",
  concept = "http://data.europa.eu/83i/aa/GDP"
)

geo_europe <- defined(
  c("AD", "LI"),
  label = "Geopolitical Entity",
  concept = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
  namespace = "https://www.geonames.org/countries/$1/"
)

euros_dataset <- dataset_df(
  geo = geo_europe,
  gdp = euro_gdp,
  dataset_bibentry = dublincore(
    title = "European Microstates GDP",
    creator = person("Statistical Unit", role = "aut"),
    publisher = "Eurostat",
    subject = "Gross Domestic Product"
  )
)

## ----usdgdp-------------------------------------------------------------------
usd_gdp <- defined(
  56,
  label = "Gross Domestic Product",
  unit = "USD_MILLIONS",
  concept = "http://data.europa.eu/83i/aa/GDP"
)

geo_tuvalu <- defined(
  "TV",
  label = "Geopolitical Entity",
  concept = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
  namespace = "https://www.geonames.org/countries/$1/"
)

tuvalu_dataset <- dataset_df(
  geo = geo_tuvalu,
  gdp = usd_gdp,
  dataset_bibentry = dublincore(
    title = "Tuvalu GDP (USD)",
    creator = person("Island", "Bureau", role = "aut"),
    publisher = "PacificStats",
    subject = "Gross Domestic Product"
  )
)

## -----------------------------------------------------------------------------
binded <- try(bind_defined_rows(euros_dataset, tuvalu_dataset), silent = TRUE)

## ----mutate-------------------------------------------------------------------
exchange_rate <- 1.02
eur_tuv_gdp <- defined(
  56 * exchange_rate,
  label = "Gross Domestic Product",
  unit = "CP_MEUR",
  concept = "http://data.europa.eu/83i/aa/GDP"
)

tuvalu_dataset <- dataset_df(
  geo = geo_tuvalu,
  gdp = eur_tuv_gdp,
  dataset_bibentry = dublincore(
    title = "Tuvalu GDP (USD)",
    creator = person("Island", "Bureau", role = "aut"),
    publisher = "PacificStats",
    subject = "Gross Domestic Product"
  )
)

## -----------------------------------------------------------------------------
var_unit(eur_tuv_gdp) <- "M_EUR"

## ----metadatachanges----------------------------------------------------------
global_dataset <- bind_defined_rows(euros_dataset, tuvalu_dataset)
dataset_title(global_dataset, overwrite = TRUE) <- "Global Microstates GDP"
publisher(global_dataset) <- "My Research Institute"
creator(global_dataset) <- person("Jane Doe", role = "aut")
language(global_dataset) <- "en"
description(global_dataset) <- "A dataset created from various sources about the GDP of very small states."
global_dataset

## -----------------------------------------------------------------------------
as_dublincore(global_dataset)

## -----------------------------------------------------------------------------
dataset_to_triples(global_dataset)

## -----------------------------------------------------------------------------
dataset_to_triples(global_dataset, format = "nt")

