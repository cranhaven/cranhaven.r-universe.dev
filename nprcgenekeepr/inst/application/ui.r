# nolint start: undesirable_function_linter
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(stringi)
# nolint end: undesirable_function_linter
source("../application/uitpInput.R")
#source("../application/uitpErrorTab.R") # nolint: commented_code_linter
source("../application/uitpPedigreeBrowser.R")
source("../application/uitpGeneticValueAnalysis.R")
source("../application/uitpSummaryStatistics.R")
source("../application/uitpBreedingGroupFormation.R")
source("../application/uitpPyramidPlot.R")
source("../application/uitpGvAndBgDesc.R")
if (getSiteInfo()$center == "ONPRC") {
  source("../application/uitpOripReporting.R")
  navbarPageArgs <- list(
    title = stri_c("Genetic Management Tools - Version ", getVersion()),
    uitpInput,
    uitpPedigreeBrowser,
    uitpPyramidPlot,
    uitpGeneticValueAnalysis,
    uitpSummaryStatistics,
    uitpBreedingGroupFormation,
    # uitpOripReporting, # nolint: commented_code_linter
    uitpGvAndBgDesc,
    id = "tab_pages"
  )
} else {
  navbarPageArgs <- list(
    title = stri_c("Genetic Management Tools - Version ", getVersion()),
    uitpInput,
    uitpPedigreeBrowser,
    uitpPyramidPlot,
    uitpGeneticValueAnalysis,
    uitpSummaryStatistics,
    uitpBreedingGroupFormation,
    uitpGvAndBgDesc,
    id = "tab_pages"
  )
}

shinyUI(tagList(
  tags$head(tags$style(src = "app_style.js")),
  #tags$head(tags$script(src = "add_tabs.js")), # nolint: commented_code_linter
  # Important! : 'Freshly baked' tabs first enter here.
  # uiOutput("navbarPage", style = "display: none;"), # nolint: commented_code_linter
  # End Important

  do.call(navbarPage, navbarPageArgs)

))
