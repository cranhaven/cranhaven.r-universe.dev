## ----setupvignette, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loaddata-----------------------------------------------------------------
library(dataset)
data("gdp")

## ----printgdp-----------------------------------------------------------------
print(gdp)

## ----createdataasetdf---------------------------------------------------------
small_country_dataset <- dataset_df(
  geo = defined(
    gdp$geo,
    label = "Country name",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
    namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/geo/$1"
  ),
  year = defined(
    gdp$year,
    label = "Reference Period (Year)",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
  ),
  gdp = defined(
    gdp$gdp,
    label = "Gross Domestic Product",
    unit = "CP_MEUR",
    concept = "http://data.europa.eu/83i/aa/GDP"
  ),
  unit = defined(
    gdp$unit,
    label = "Unit of Measure",
    concept = "http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure",
    namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/unit/$1"
  ),
  freq = defined(
    gdp$freq,
    label = "Frequency",
    concept = "http://purl.org/linked-data/sdmx/2009/code"
  ),
  dataset_bibentry = dublincore(
    title = "Small Country Dataset",
    creator = person("Jane", "Doe"),
    publisher = "Example Inc.",
    datasource = "https://doi.org/10.2908/NAIDA_10_GDP",
    rights = "CC-BY",
    coverage = "Andorra, Liechtenstein, San Marino and the Feroe Islands"
  )
)

## ----varlabel-----------------------------------------------------------------
var_label(small_country_dataset$gdp)

## ----varunit------------------------------------------------------------------
var_unit(small_country_dataset$gdp)

## ----language-----------------------------------------------------------------
language(small_country_dataset) <- "en"

## ----bibentry-----------------------------------------------------------------
print(get_bibentry(small_country_dataset), "bibtex")

## ----feroedf------------------------------------------------------------------
feroe_df <- data.frame(
  geo = rep("FO", 3),
  year = 2020:2022,
  gdp = c(2523.6, 2725.8, 3013.2),
  unit = rep("CP_MEUR", 3),
  freq = rep("A", 3)
)

## ----notevaluatedrbind, eval=FALSE--------------------------------------------
# rbind(small_country_dataset, feroe_df)

## ----fereodataset-------------------------------------------------------------
feroe_dataset <- dataset_df(
  geo = defined(
    feroe_df$geo,
    label = "Country name",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
    namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/geo/$1"
  ),
  year = defined(
    feroe_df$year,
    label = "Reference Period (Year)",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
  ),
  gdp = defined(
    feroe_df$gdp,
    label = "Gross Domestic Product",
    unit = "CP_MEUR",
    concept = "http://data.europa.eu/83i/aa/GDP"
  ),
  unit = defined(
    feroe_df$unit,
    label = "Unit of Measure",
    concept = "http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure",
    namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/unit/$1"
  ),
  freq = defined(
    feroe_df$freq,
    label = "Frequency",
    concept = "http://purl.org/linked-data/sdmx/2009/code"
  )
)

## ----binddefinedrows----------------------------------------------------------
joined_dataset <- bind_defined_rows(small_country_dataset, feroe_dataset)
joined_dataset

## ----backwardcompatibility----------------------------------------------------
attributes(as.data.frame(joined_dataset))

## ----coercion-----------------------------------------------------------------
as.data.frame(small_country_dataset)

## ----richdataframe------------------------------------------------------------
as.data.frame(small_country_dataset, 
              strip_attributes = FALSE)

## ----astibble-----------------------------------------------------------------
as_tibble(orange_df)

