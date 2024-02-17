## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dataset)

## ----data-frame-example-------------------------------------------------------
foo <- data.frame( x = c(1,2), y = c(3,4))
attr(foo, "Title") <- "My Foo Object"
attributes(foo)

## ----mtcars-------------------------------------------------------------------
head (dataset(mtcars, 
        title="The Motor Trend [mtcar] Dataset", 
        author=person("Motor Trend Magazine"), 
        year=1974, 
        publisher="Motor Trend Magazine" ))


## ----mtcars-ttl, warning=FALSE------------------------------------------------
temp_ttl_file  <- file.path(tempdir(), "temp_ttl.ttl")
mtcars_dataset <- dataset(mtcars, 
        title="The Motor Trend [mtcar] Dataset", 
        author=person("Motor Trend Magazine"), 
        year=1974, 
        publisher="Motor Trend Magazine")

mtcars_namespace <- dataset_namespace[
  dataset_namespace$prefix %in% c("owl:", "rdf:", "rdfs:", "qb:", "eg:"), ]

mtcars_dataset   <- id_to_column(mtcars_dataset, prefix = "eg:", ids = NULL)
mtcars_dataset   <- dataset_to_triples(mtcars_dataset, idcol = "rowid")
mtcars_dataset$p <- paste0("eg:mtcars#", mtcars_dataset$p)
mtcars_dataset$o <- xsd_convert(mtcars_dataset$o)
dataset_ttl_write(mtcars_dataset, 
                  ttl_namespace = mtcars_namespace, 
                  file_path = temp_ttl_file)
readLines(temp_ttl_file, 25)

## ----tibbble------------------------------------------------------------------
library(tibble)
ds_tibble <- dataset(as_tibble(mtcars), 
                     title = "The Motor Trend [mtcar] Dataset", 
                     author = person("Motor Trend Magazine"), 
                     year = 1974, 
                     publisher  =  "Motor Trend Magazine" )

rowid_to_column(ds_tibble)

## ----tsibble, message=FALSE, warning=FALSE------------------------------------
library("nycflights13")
library("tsibble")
library("dplyr")
library("tidyr")

data("weather")
weather <- weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)

## ----tsbl---------------------------------------------------------------------
ds_weather <- dataset(weather_tsbl, 
                      title = "Weather dataset", 
                      author = person("Jane", "Doe"), 
                      creator = person("Jane", "Doe"))

dataset_bibentry(ds_weather)

## ----workwithtsibble----------------------------------------------------------
full_weather <- ds_weather %>%
  fill_gaps(precip = 0) %>% 
  group_by_key() %>% 
  tidyr::fill(temp, humid, .direction = "down")

## ----full-weather-------------------------------------------------------------
full_weather %>%
  group_by_key() %>%
  index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    ttl_precip = sum(precip, na.rm = TRUE)
  )

## ----weather-dataset----------------------------------------------------------
ds_full_weather <- dataset (full_weather %>%
  group_by_key() %>%
  index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    ttl_precip = sum(precip, na.rm = TRUE)
  ), 
  title = "Weather tsibble",
  author = person("Jane", "Doe"), 
  creator = person("Jane", "Doe"), 
  language = "eng", 
  description = "A replication of the tsibble README example."
  )

## ----print-datacite-citation-format-------------------------------------------
print(as_datacite(ds_full_weather), "citation")

## ----weather-ttl, warning=FALSE-----------------------------------------------
temp_weather_file  <- file.path(tempdir(), "temp_weather.ttl")
weather_namespace <- dataset_namespace[
  dataset_namespace$prefix %in% c("owl:", "rdf:", "rdfs:", "qb:", "eg:"), ]

ds_weather    <- id_to_column(ds_full_weather, prefix = "eg:", ids = NULL)
#ds_weather    <- xsd_convert(ds_weather, idcol = "rowid")
ds_weather    <- dataset_to_triples(ds_weather, idcol = "rowid")
ds_weather$p <- paste0("eg:weather#", ds_weather$p)
ds_weather$o <- xsd_convert(ds_weather$o)
dataset_ttl_write(ds_weather, 
                  ttl_namespace = weather_namespace, 
                  file_path = temp_weather_file)
readLines(temp_weather_file, 25)

