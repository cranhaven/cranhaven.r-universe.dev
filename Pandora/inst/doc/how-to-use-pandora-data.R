## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

os_linux <- Sys.info()["sysname"] == "Linux"

## ----setup--------------------------------------------------------------------
library(Pandora)
library(magrittr)

## ----networks-----------------------------------------------------------------
networks <- getNetworks()
networks %>% 
  knitr::kable()

## ----repositories-------------------------------------------------------------
reposIsomemo <- getRepositories(
  network = "IsoMemo"
  )

reposIsomemo[c("Repository")]  %>% 
  knitr::kable()

## ----filetypes----------------------------------------------------------------
fileTypesIsomemo <- getFileTypes(network = "isomemo")
fileTypesIsomemo  %>% 
  knitr::kable()

## ----resources----------------------------------------------------------------
resourcesPlants <- getResources(
  fileType = c("csv"),
  pattern = "plant"
  )

resourcesPlants[c("name", "format")]  %>% 
  knitr::kable()

## ----getdata------------------------------------------------------------------
isotopicData <- getData(
  name = "CIMA Animals 29.05.2021 CSV",
  options = dataOptions(sep = ";")) 
isotopicData  %>% 
  head(5)  %>% 
  dplyr::select(c("Entry_ID", "General_Category_Family", "Common_Name", "Sampled_Element", "Analysed_Component")) %>%
  knitr::kable()

