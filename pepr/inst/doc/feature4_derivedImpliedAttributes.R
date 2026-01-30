## ----echo=FALSE---------------------------------------------------------------
branch = "master"
library(knitr)
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive_imply",
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
"example_derive_imply",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_derive_imply",
  "sample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  knitr::kable(sampleAnnotationDF, format = "html") 

## ----collapse=TRUE------------------------------------------------------------
library(pepr)
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_derive_imply",
"project_config.yaml",
package = "pepr"
)
p = Project(projectConfig)

## ----collapse=T---------------------------------------------------------------
sampleTable(p)

