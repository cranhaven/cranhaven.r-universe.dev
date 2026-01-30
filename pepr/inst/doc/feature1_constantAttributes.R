## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,warning=FALSE,message=FALSE-----------------------------------
branch = "master"
library(knitr)
library(pepr)
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_append",
"sample_table_pre.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
knitr::kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_append",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE,warning=FALSE-------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_append",
  "sample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig = system.file(
  "extdata",
  paste0("example_peps-", branch), 
  "example_append", 
  "project_config.yaml", 
  package = "pepr"
)
p = Project(projectConfig)

## -----------------------------------------------------------------------------
sampleTable(p)

## -----------------------------------------------------------------------------
config(p)

