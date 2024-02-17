## ----setupRDFvignette, include = FALSE----------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE,
  message  = FALSE,
  comment = "#>",
  out.width = '90%'
)
if (file.exists(file.path(tempdir(), "example_ttl.ttl"))) {
  file.remove(file.path(tempdir(), "example_ttl.ttl"))
}

library(here)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----head-iris----------------------------------------------------------------
head(iris_dataset, 2)

## ----head-iris-xsd------------------------------------------------------------
xsd_convert(head(iris_dataset, 2))

## ----dataset-to-triples-------------------------------------------------------
iris_triples <- dataset_to_triples(xsd_convert(head(iris_dataset,2)))
iris_triples

## ----define-vars--------------------------------------------------------------
iris_triples$p <- paste0("iris:", iris_triples$p)
iris_triples

## ----row-names----------------------------------------------------------------
row.names(head(iris_dataset,2))

## ----write-to-ttle------------------------------------------------------------
vignette_temp_file <- file.path(tempdir(), "example_ttl.ttl")
dataset_ttl_write(dataset_to_triples(iris_triples), 
                  file_path = vignette_temp_file)

## ----read-ttl-----------------------------------------------------------------
# Only first 23 lines are read and printed:
readLines(vignette_temp_file, n = 23)

## ----prefix-5-----------------------------------------------------------------
readLines(vignette_temp_file, n = 5)

## ----further-prefix-----------------------------------------------------------
data("dataset_namespace")
unique(get_prefix(row.names(head(iris_dataset,2))))

## ----define-all-prefixes------------------------------------------------------
used_prefixes <- which(dataset_namespace$prefix %in% c(
  "owl:", "rdf:", "rdfs:", "qb:", "xsd:")
  )

vignette_namespace <- rbind(
  dataset_namespace[used_prefixes, ], 
       data.frame (prefix = "iris:", 
                   uri = '<www.example.com/iris#>')
) 

vignette_namespace

## ----write-to-ttle2-----------------------------------------------------------
dataset_ttl_write(
  iris_triples, 
  ttl_namespace = vignette_namespace,
  file_path = vignette_temp_file, 
  overwrite = TRUE)

## ----prefix2------------------------------------------------------------------
readLines(vignette_temp_file, n = 23)

## ----parse-ttl----------------------------------------------------------------
require(rdflib)
example_rdf <- rdf_parse(vignette_temp_file, format = "turtle")
example_rdf

## ----sparql_example-----------------------------------------------------------
sparql <-
'PREFIX iris: <www.example.com/iris#> 
 SELECT ?observation ?value
 WHERE { ?observation iris:Sepal.Length ?value . }'

rdf_query(example_rdf, sparql)

## ----convert-to-jsonld--------------------------------------------------------
temp_jsonld_file <- file.path(tempdir(), "example_jsonld.json")
rdf_serialize(rdf=example_rdf, doc = temp_jsonld_file, format = "jsonld")

## ----read-first-12-lines------------------------------------------------------
readLines(temp_jsonld_file, 12)

