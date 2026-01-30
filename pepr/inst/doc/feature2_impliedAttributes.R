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
"example_imply",
"sample_table_pre.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
knitr::kable(sampleAnnotationDF, format = "html")

## ----echo=FALSE,message=FALSE,collapse=TRUE,comment=" "-----------------------
library(pepr)
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_imply",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_imply",
  "sample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_imply",
"project_config.yaml",
package = "pepr"
)
p = Project(projectConfig)

## -----------------------------------------------------------------------------
sampleTable(p)

## -----------------------------------------------------------------------------
config(p)

