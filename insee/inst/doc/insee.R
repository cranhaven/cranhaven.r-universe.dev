## ----setup,echo=FALSE, include=FALSE------------------------------------------
# setup chunk
# Sys.setenv("NOT_CRAN" = "TRUE")
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")),"true")
knitr::opts_chunk$set(purl = NOT_CRAN)
library(insee)
library(tidyverse)

embed_png <- function(path, dpi = NULL) {
  meta <- attr(png::readPNG(path, native = TRUE, info = TRUE), "info")
  if (!is.null(dpi)) meta$dpi <- rep(dpi, 2)
  knitr::asis_output(paste0(
    "<img src='", path, "'",
    " width=", round(meta$dim[1] / (meta$dpi[1] / 96)),
    " height=", round(meta$dim[2] / (meta$dpi[2] / 96)),
    " />"
  ))}

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(kableExtra)
library(magrittr)
library(htmltools)
library(prettydoc)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  Sys.setenv(http_proxy = "my_proxy_server")
#  Sys.setenv(https_proxy = "my_proxy_server")

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  Sys.setenv(INSEE_download_option_method = "mymethod")
#  Sys.setenv(INSEE_download_option_port = "1234")
#  Sys.setenv(INSEE_download_option_extra = "-U : --proxy-myprotocol --proxy myproxy:1234")
#  Sys.setenv(INSEE_download_option_proxy = "myproxy")
#  Sys.setenv(INSEE_download_option_auth = "myprotocol")

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  # Get the development version from GitHub
#  # install.packages("devtools")
#  devtools::install_github("InseeFr/R-Insee-Data")
#  
#  # CRAN version
#  # install.packages("insee")
#  
#  # library Loading
#  library(insee)
#  library(tidyverse)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  insee_dataset = get_dataset_list()

## ----echo = FALSE, message = FALSE, warning = FALSE, eval = FALSE-------------
#  rownames(insee_dataset) <- NULL
#  
#  insee_dataset %>%
#    select(id, Name.en, Name.fr, url, n_series) %>%
#    slice(1:10) %>%
#    kable(row.names=NA) %>%
#    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- message=FALSE,warning=FALSE,eval=FALSE----------------------------------
#  idbank_list = get_idbank_list('BALANCE-PAIEMENTS')

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=FALSE----------------------
#  idbank_list = get_idbank_list()
#  
#  rownames(idbank_list) <- NULL
#  
#  idbank_list %>%
#            select(nomflow, idbank, cleFlow) %>%
#            group_by(nomflow) %>%
#            slice(1) %>%
#            ungroup() %>%
#            head(10) %>%
#            kable(row.names=NA) %>%
#            kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
#  
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  
#  idbank_list_selected =
#    get_idbank_list("IPI-2015") %>% #industrial production index dataset
#    filter(FREQ == "M") %>% #monthly
#    filter(NATURE == "INDICE") %>% #index
#    filter(CORRECTION == "CVS-CJO") %>% #Working day and seasonally adjusted SA-WDA
#    #automotive industry and overall industrial production
#    filter(str_detect(NAF2,"^29$|A10-BE")) %>%
#    add_insee_title()
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  # search multiple patterns
#  dataset_survey_gdp = search_insee("Survey|gdp")
#  
#  # data about paris
#  data_paris = search_insee('paris')
#  
#  # all data
#  data_all = search_insee()

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  library(tidyverse)
#  library(insee)
#  
#  # the user can make a manual list of idbanks to get the data
#  # example 1
#  
#  data =
#    get_insee_idbank("001558315", "010540726") %>%
#    add_insee_metadata()
#  
#  # using a list of idbanks extracted from the insee idbank dataset
#  # example 2 : household's confidence survey
#  
#  df_idbank =
#    get_idbank_list("ENQ-CONJ-MENAGES") %>%  #monthly households' confidence survey
#    add_insee_title() %>%
#    filter(CORRECTION == "CVS") #seasonally adjusted
#  
#  list_idbank = df_idbank %>% pull(idbank)
#  
#  data =
#    get_insee_idbank(list_idbank) %>%
#    split_title() %>%
#    add_insee_metadata()
#  
#  # example 3 : get more than 1200 idbanks
#  
#  idbank_dataset = get_idbank_list()
#  
#  df_idbank =
#    idbank_dataset %>%
#    slice(1:1201)
#  
#  list_idbank = df_idbank %>% pull(idbank)
#  
#  data = get_insee_idbank(list_idbank, firstNObservations = 1, limit = FALSE)
#  

## ----message=FALSE, warning=FALSE,eval=FALSE----------------------------------
#  
#  insee_dataset = get_dataset_list()
#  
#  # example 1 : full dataset
#  data = get_insee_dataset("CLIMAT-AFFAIRES")
#  
#  # example 2 : filtered dataset
#  # the user can filter the data
#  data = get_insee_dataset("IPC-2015", filter = "M+A.........CVS.", startPeriod = "2015-03")
#  
#  # in the filter, the + is used to select several values in one dimension, like an "and" statement
#  # the void means "all" values available
#  
#  # example 3 : only one series
#  # by filtering with the full SDMX series key, the user will get only one series
#  data =
#    get_insee_dataset("CNA-2014-CPEB",
#                      filter = "A.CNA_CPEB.A38-CB.VAL.D39.VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
#                      lastNObservations = 10)
#  

