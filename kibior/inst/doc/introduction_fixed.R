## ----setup, include = TRUE, echo = FALSE, results = FALSE, message = FALSE, warning = FALSE----
library(knitr)
knitr::opts_chunk$set(include = TRUE, echo = TRUE, warning = FALSE, 
                      message = FALSE, results = "markup", collapse = TRUE, 
                      cache = FALSE, comment = "##")
library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(magrittr)
library(kibior)

## ---- echo = FALSE, results = "markup"----------------------------------------
dplyr::starwars[1:5,]

## ---- echo = FALSE, results = "markup"----------------------------------------
dplyr::storms[1:5,]

## ---- echo = FALSE, results = "markup"----------------------------------------
datasets::iris[1:5,]

## ---- echo = FALSE, results = "markup"----------------------------------------
ggplot2::diamonds[1:5,]

## -----------------------------------------------------------------------------
sessionInfo()

