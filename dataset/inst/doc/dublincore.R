## ----setupknitr, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dataset)

## ----createdublincore---------------------------------------------------------

dc <- dublincore(
  title = "Iris Dataset",
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set gives the
  measurements in centimeters of the variables sepal length and width and petal length
  and width, respectively, for 50 flowers from each of 3 species of iris.
  The species are Iris setosa, versicolor, and virginica."
)

## ----printdublincore, results='markup'----------------------------------------
print(dc, "Bibtex")

## ----as_dublincore-example, results='markup'----------------------------------
as_dublincore(iris_dataset, "list")

