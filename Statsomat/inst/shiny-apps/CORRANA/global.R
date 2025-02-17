options(shiny.maxRequestSize = 5*1024^2)
options(shiny.sanitize.errors = TRUE)

library(shiny)
library(rmarkdown)
library(data.table)
library(readr)
library(shinydisconnect)

source("chooser.R")
source("Functions.R")

