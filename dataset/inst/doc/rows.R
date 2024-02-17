## ----setupknitr, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----rdf-example-9-continued, eval=FALSE--------------------------------------
#  # Example 9 of the RDF Data Cube Vocabulary definition
#  
#  eg:o1 a qb:Observation;
#      qb:dataSet  eg:dataset-le1 ;
#      eg:refArea                 ex-geo:newport_00pr ;
#      eg:refPeriod               <http://reference.data.gov.uk/id/gregorian-interval/2004-01-01T00:00:00/P3Y> ;
#      sdmx-dimension:sex         sdmx-code:sex-M ;
#      sdmx-attribute:unitMeasure <http://dbpedia.org/resource/Year> ;
#      eg:lifeExpectancy          76.7 ;
#      .

## ----iris-observation-ids-----------------------------------------------------
data("iris")
eg_iris <-iris
row.names(eg_iris) <- paste0("eg:o", row.names(iris))
head(eg_iris)[1:6,]

## ----iris-observation-ids2----------------------------------------------------
row.names(eg_iris) <- paste0("https://doi.org/10.5281/zenodo.10396807:o", row.names(iris))
head(eg_iris)[1:6,]

