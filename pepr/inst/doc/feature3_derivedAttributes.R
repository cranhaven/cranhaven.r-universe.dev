## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
branch = "master"
library(knitr)
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive",
"sample_table_pre.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
knitr::kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
library(pepr)
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive",
"project_config.yaml",
package = "pepr"
)
pepr::.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive",
"sample_table.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
library(pepr)
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive",
"project_config.yaml",
package = "pepr"
)
p = Project(projectConfig)

## -----------------------------------------------------------------------------
sampleTable(p)

## -----------------------------------------------------------------------------
config(p)

