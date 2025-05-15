#' @name regressoR
#' @aliases regressoR
#' @docType package
#' @title Regression Data Analysis System
#' @author Oldemar Rodriguez Rojas \cr
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#' @description
#' Perform a supervised data analysis on a database through a 'shiny' graphical interface. 
#' It includes methods such as linear regression, penalized regression, k-nearest neighbors, decision trees, ada boosting, 
#' extreme gradient boosting, random forest, neural networks, deep learning and support vector machines.
#' @details
#' \tabular{ll}{
#' Package: \tab regressoR\cr
#' Type: \tab Package\cr
#' Version: \tab 4.0.3\cr
#' Date: \tab 2024-11-15\cr
#' License: \tab GPL (>=2)\cr
#' }
#' @keywords package
"_PACKAGE"

NULL
utils::globalVariables(c(
  "name", "value", "color", "vare", "x", "y", "X", "Modelo", "RMSE", "lower",
  "upper", "ID", "nombre", "importancia"
))