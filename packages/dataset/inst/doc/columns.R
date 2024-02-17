## ----setupknitr, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----iris---------------------------------------------------------------------
head(iris)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----addlabels----------------------------------------------------------------
dsd_iris <- DataStructure(iris_dataset)
dsd_iris$Sepal.Length$label <- "The sepal length measured in centimeters."
dsd_iris$Sepal.Width$label  <- "The sepal width measured in centimeters."
dsd_iris$Petal.Length$label <- "The petal length measured in centimeters."
dsd_iris$Petal.Width$label  <- "The petal width measured in centimeters."
dsd_iris$Species$label      <- "The name of the Iris species in the observation."

iris_dataset_labelled <- DataStructure_update(iris_dataset, dsd_iris)

## ----retrievelabel------------------------------------------------------------
DataStructure(iris_dataset_labelled)[["Sepal.Length"]]$label

## ----retrieverange------------------------------------------------------------
DataStructure(iris_dataset_labelled)[["Sepal.Length"]]$range

## ----rdf-example-9, eval=FALSE------------------------------------------------
#  # Example 9 of the RDF Data Cube Vocabulary definition
#  
#  eg:dataset-le1 a qb:DataSet;
#      rdfs:label "Life expectancy"@en;
#      rdfs:comment "Life expectancy within Welsh Unitary authorities - extracted from Stats Wales"@en;
#      qb:structure eg:dsd-le ;
#      .
#  
#  eg:o1 a qb:Observation;
#      qb:dataSet  eg:dataset-le1 ;
#      eg:refArea                 ex-geo:newport_00pr ;
#      eg:refPeriod               <http://reference.data.gov.uk/id/gregorian-interval/2004-01-01T00:00:00/P3Y> ;
#      sdmx-dimension:sex         sdmx-code:sex-M ;
#      sdmx-attribute:unitMeasure <http://dbpedia.org/resource/Year> ;
#      eg:lifeExpectancy          76.7 ;
#      .

