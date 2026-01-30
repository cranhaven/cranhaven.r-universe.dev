## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
branch = "master"
library(pepr)
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable1",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable1",
"sample_table.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_subtable1",
  "subsample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig1 = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable1",
"project_config.yaml",
package = "pepr"
)
p1 = Project(projectConfig1)
# Check the files
p1Samples = sampleTable(p1)
p1Samples$file
# Check the subsample names
p1Samples$subsample_name

## ----echo=FALSE---------------------------------------------------------------
kable(p1Samples)

## -----------------------------------------------------------------------------
sampleName = "frog_1"
subsampleName = "sub_a"
getSubsample(p1, sampleName, subsampleName)

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable2",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable2",
"sample_table.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_subtable2",
  "subsample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig2 = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable2",
"project_config.yaml",
package = "pepr"
)
p2 = Project(projectConfig2)
# Check the files
p2Samples = sampleTable(p2)
p2Samples$file

## ----echo=FALSE---------------------------------------------------------------
kable(p2Samples)

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
projectConfig = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable3",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(projectConfig))

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable3",
"sample_table.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_subtable3",
  "subsample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig3 = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable3",
"project_config.yaml",
package = "pepr"
)
p3 = Project(projectConfig3)
# Check the files
p3Samples = sampleTable(p3)
p3Samples$file

## ----echo=FALSE---------------------------------------------------------------
kable(p3Samples)

## ----echo=FALSE,message=TRUE,collapse=TRUE,comment=" "------------------------
project_config = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable4",
"project_config.yaml",
package = "pepr"
)
.printNestedList(yaml::read_yaml(project_config))

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable4",
"sample_table.csv",
package = "pepr"
)
sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
kable(sampleAnnotationDF, format = "html") 

## ----echo=FALSE---------------------------------------------------------------
sampleAnnotation = system.file(
  "extdata",
  paste0("example_peps-", branch),
  "example_subtable4",
  "subsample_table.csv",
  package = "pepr"
  )
  sampleAnnotationDF = read.table(sampleAnnotation, sep = ",", header = T)
  kable(sampleAnnotationDF, format = "html") 

## -----------------------------------------------------------------------------
projectConfig4 = system.file(
"extdata",
paste0("example_peps-", branch),
"example_subtable4",
"project_config.yaml",
package = "pepr"
)
p4 = Project(projectConfig4)
# Check the read1 and read2 columns
p4Samples = sampleTable(p4)
p4Samples$read1
p4Samples$read2

## ----echo=FALSE---------------------------------------------------------------
kable(p4Samples)

