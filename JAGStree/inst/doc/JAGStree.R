## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE-----------------------------------------------------
library(JAGStree)

## ----example------------------------------------------------------------------
data <- data.frame("from" = c("Z", "Z", "A", "A"),
                       "to" = c("A", "B", "C", "D"),
                       "Estimate" = c(4, 34, 9, 1),
                       "Total" = c(11, 70, 10, 10),
                       "Count" = c(NA, 500, NA, 50),
                       "Population" = c(FALSE, FALSE, FALSE, FALSE),
                       "Description" = c("First child of the root", "Second child of the root",
                                         "First grandchild", "Second grandchild"))

# optional use of the AutoWMM package to show tree structure
Sys.setenv("RGL_USE_NULL" = TRUE)
library(AutoWMM)
library(DiagrammeR)
tree <- makeTree(data)
drawTree(tree)

## ----eval=FALSE---------------------------------------------------------------
#  makeJAGStree(data1, filename=file.path(tempdir(), "data1_JAGSscript.mod"))
#  makeJAGStree(data1, filename=file.path(tempdir(), "data1_JAGSscript.txt", prior="uniform"))

