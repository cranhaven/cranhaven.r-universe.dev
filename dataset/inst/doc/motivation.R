## ----setupknitr, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dataset-constructor------------------------------------------------------
library(dataset)
my_dataset <- dataset (x = data.frame (
  time = rep(c(2019:2022),4),
  geo = c(rep("NL",8), rep("BE",8)),
  sex = c(rep("F", 4), rep("M", 4), rep("F", 4), rep("M", 4)),
  value = c(1,3,2,4,2,3,1,5, NA_real_, 4,3,2,1, NA_real_, 2,5),
  unit = rep("NR",8),
  freq = rep("A",8)),
  title = "Example dataset",
  author = person("Jane", "Doe")
)

my_dataset

## ----datacite, results='asis'-------------------------------------------------
iris_datacite  <- datacite(
  Title = "Iris Dataset",
  Creator = person("Anderson", "Edgar", role = "aut"),
  Publisher = "American Iris Society",
  Identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  PublicationYear = 1935,
  Description = "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.",
  Language = "en")



## ----printdatacite------------------------------------------------------------
print(iris_datacite, "Bibtex")

## ----adduri, eval=FALSE-------------------------------------------------------
#  my_dataset_uri <- dataset_uri(my_dataset,
#                                prefix = "https:://example.org/my_iris",
#                                keep_local_id = FALSE)
#  my_dataset_uri

## ----subsettotriple, eval=FALSE-----------------------------------------------
#  nq_file <- file.path(tempdir(), "triple_file.nq")
#  my_triple <- subset(my_dataset_uri, select = c("URI", "value", "unit"))

## ----rdflib, eval=FALSE-------------------------------------------------------
#  library(rdflib)
#  rdf <- rdf()
#  
#  for ( i in seq_len(nrow(my_triple))) {
#    rdf_add(rdf = rdf,
#            subject = "",
#            predicate = my_triple$URI[i],
#            object = my_triple$value[i])
#  }
#  
#  rdf_serialize(rdf, doc = nq_file)

## ----parsenqfile, eval=FALSE--------------------------------------------------
#  rdf_parse(nq_file)

