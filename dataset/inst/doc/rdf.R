## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)
library(rdflib)
data("gdp")

## ----minimaldf----------------------------------------------------------------
small_geo <- dataset_df(
  geo = defined(
    gdp$geo[1:3],
    label = "Geopolitical entity",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
    namespace = "https://www.geonames.org/countries/$1/"
  ),
  identifier = c(
    obs = "https://dataset.dataobservatory.eu/examples/dataset.html#"
  )
)

## ----printsmallgeodf----------------------------------------------------------
print(small_geo)

## ----triplesdf, eval=FALSE----------------------------------------------------
# triples_df <- dataset_to_triples(small_geo)
# knitr::kable(triples_df)

## ----triplesdfprintsmall, echo=FALSE------------------------------------------
triples_df <- dataset_to_triples(small_geo)
knitr::kable(triples_df)

## ----createntriples-----------------------------------------------------------
ntriples <- dataset_to_triples(small_geo, format = "nt")

## ----pritriples, eval=FALSE---------------------------------------------------
# cat(ntriples, sep = "\n")

## ----printsmaller-------------------------------------------------------------
cat(ntriples, sep = "\n")

## ----ntripleexample-----------------------------------------------------------
n_triple(
  s = "https://dataset.dataobservatory.eu/examples/dataset.html#obs1",
  p = "http://purl.org/dc/terms/title",
  o = "Small Country Dataset"
)

## ----readrdf------------------------------------------------------------------
# We write to a temporary file our Ntriples created earlier
temp_file <- tempfile(fileext = ".nt")
writeLines(ntriples, con = temp_file)

rdf_graph <- rdf()
rdf_parse(rdf_graph, doc = temp_file, format = "ntriples")
rdf_graph

## ----cleanup------------------------------------------------------------------
# Clean up: delete file and clear RDF graph
unlink(temp_file)
rm(rdf_graph)
gc()

## ----scaleup------------------------------------------------------------------
small_country_dataset <- dataset_df(
  geo = defined(
    gdp$geo,
    label = "Country name",
    concept = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
    namespace = "https://www.geonames.org/countries/$1/"
  ),
  year = defined(
    gdp$year,
    label = "Reference Period (Year)",
    concept = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
  ),
  gdp = defined(
    gdp$gdp,
    label = "Gross Domestic Product",
    unit = "https://dd.eionet.europa.eu/vocabularyconcept/eurostat/unit/CP_MEUR",
    concept = "http://data.europa.eu/83i/aa/GDP"
  ),
  unit = gdp$unit,
  freq = defined(
    gdp$freq,
    label = "Frequency",
    concept = "http://purl.org/linked-data/sdmx/2009/code"
  ),
  identifier = c(
    obs = "https://dataset.dataobservatory.eu/examples/dataset.html#"
  ),
  dataset_bibentry = dublincore(
    title = "Small Country Dataset",
    creator = person("Jane", "Doe"),
    publisher = "Example Inc.",
    datasource = "https://doi.org/10.2908/NAIDA_10_GDP",
    rights = "CC-BY",
    coverage = "Andorra, Lichtenstein and San Marino"
  )
)

## ----smallcountrydfnt---------------------------------------------------------
small_country_df_nt <- dataset_to_triples(
  small_country_dataset,
  format = "nt"
)

## ----smallcountrydfntsample---------------------------------------------------
## See rows 1,11,21
small_country_df_nt[c(1, 11, 21, 31, 41)]

## ----readrdf2-----------------------------------------------------------------
# We write to a temporary file our Ntriples created earlier
temp_file <- tempfile(fileext = ".nt")
writeLines(small_country_df_nt,
  con = temp_file
)

rdf_graph <- rdf()
rdf_parse(rdf_graph, doc = temp_file, format = "ntriples")

## ----readrdf2print, eval = FALSE----------------------------------------------
# rdf_graph

## ----readrdf2printsmaller-----------------------------------------------------
rdf_graph

## ----readjsonld---------------------------------------------------------------
# Create temporary JSON-LD output file
jsonld_file <- tempfile(fileext = ".jsonld")

# Serialize (export) the entire graph to JSON-LD format
rdf_serialize(rdf_graph, doc = jsonld_file, format = "jsonld")

## ----readjsonldprint----------------------------------------------------------
cat(readLines(jsonld_file)[1:30], sep = "\n")

## ----clenup2, echo=FALSE, message=FALSE---------------------------------------
unlink(temp_file)
rm(rdf_graph)
gc()

