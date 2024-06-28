## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
 # CRAN limite CPU usage
data.table::setDTthreads(2)
library(antaresEditObject)

## ----init---------------------------------------------------------------------
dir_path <- tempdir()
createStudy(path = dir_path, 
            study_name = "test860", 
            antares_version = "8.6.0")

## ----areas--------------------------------------------------------------------
createArea(name = "fr")
createArea(name = "it")

## ----st-storage---------------------------------------------------------------
inflows_data <- matrix(3, 8760)
ratio_values <- matrix(0.7, 8760)

createClusterST(area = "fr", 
                cluster_name = "test_storage", 
                storage_parameters = storage_values_default(), 
                PMAX_injection = ratio_values, 
                PMAX_withdrawal = ratio_values, 
                inflows = inflows_data, 
                lower_rule_curve = ratio_values, 
                upper_rule_curve = ratio_values, 
                overwrite = TRUE)

createClusterST(area = "it", 
                cluster_name = "test_storage", 
                storage_parameters = storage_values_default(), 
                PMAX_injection = ratio_values, 
                PMAX_withdrawal = ratio_values, 
                inflows = inflows_data, 
                lower_rule_curve = ratio_values, 
                upper_rule_curve = ratio_values, 
                overwrite = TRUE)

## ----study options------------------------------------------------------------
opts <- simOptions()
opts$areasWithSTClusters


## ----read st-storage param----------------------------------------------------
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)

## ----read st-storage data-----------------------------------------------------
data_st_storage <- readInputTS(st_storage = "all")
rmarkdown::paged_table(head(data_st_storage))

## ----edit st-storage----------------------------------------------------------
# edit parameters values 
list_params_st <- storage_values_default()
list_params_st$efficiency <- 0.5
list_params_st$reservoircapacity <- 50

# edit data values
inflows_data <- matrix(4, 8760)

editClusterST(area = "fr", 
              cluster_name = "test_storage", 
              storage_parameters = list_params_st,
              inflows = inflows_data,
              add_prefix = TRUE)

# read parameters
tab <- readClusterSTDesc()
rmarkdown::paged_table(tab)

# read data
data_st_storage <- readInputTS(st_storage = "all")
rmarkdown::paged_table(head(data_st_storage))

## ----remove st-storage opts---------------------------------------------------
# remove cluster
removeClusterST(area = "fr", 
                cluster_name = "test_storage", 
                add_prefix = TRUE)

# delete control 
opts <- simOptions()
opts$areasWithSTClusters

## ----remove st-storage--------------------------------------------------------
# control removed parameters
tab <- readClusterSTDesc()
rmarkdown::paged_table(head(tab))

# control removed data
data_st_storage <- readInputTS(st_storage = "all")
rmarkdown::paged_table(head(data_st_storage))

unique(data_st_storage$area)

## -----------------------------------------------------------------------------
# create cluster with pollutants

# pollutants
all_param_pollutants <- list_pollutants_values(multi_values = 0.25)

createCluster(area = "fr", 
              cluster_name = "test_pollutant", 
              unitcount = 1L, 
              marginal_cost = 50,
              list_pollutants = all_param_pollutants, 
              time_series = matrix(rep(c(0, 8000), each = 24*364), ncol = 2),
              prepro_modulation = matrix(rep(c(1, 1, 1, 0), each = 24*365), ncol = 4) 
              )

## ----pollutants param---------------------------------------------------------
# read parameters
param_th_cluster <- readClusterDesc()
rmarkdown::paged_table(param_th_cluster)

## ----edit pollutants----------------------------------------------------------
# editing
edit_param_pollutants <- list_pollutants_values(multi_values = 0.3)[1:3]

editCluster(area = "fr", 
            cluster_name = "test_pollutant",
            unitcount = 2L, 
            list_pollutants = edit_param_pollutants)

# read parameters
param_th_cluster <- readClusterDesc()
rmarkdown::paged_table(param_th_cluster)

## ----schema combinatoire , echo=FALSE, fig.cap="", out.width = '50%', fig.align='center'----
knitr::include_graphics("schemas/mingen_hydro_rules.png")

## ----schema, echo=FALSE, fig.cap="", out.width = '75%', fig.align='center'----
# path_image <- sourcedir860 <- system.file("doc/schemas", package = "antaresEditObject")
# knitr::include_graphics(file.path(path_image,"mingen.png"))
knitr::include_graphics("schemas/mingen_draw.png")

## ----hydro ini----------------------------------------------------------------
# see hydro parameters 
path_file_hydro <- file.path("input", "hydro", "hydro.ini")
hydro_ini_values <- readIni(pathIni = path_file_hydro)
hydro_params <- c('follow load', 'use heuristic', "reservoir")
hydro_ini_values[hydro_params]

## ----edit mod-----------------------------------------------------------------
# Initialize mingen data (time series)
mingen_data = matrix(0.06,8760,5)

# 1 - edit mod file (time series)
mod_data = matrix(6,365,5)
suppressWarnings(
  writeInputTS(area = "fr", type = "hydroSTOR", 
             data = mod_data, 
             overwrite = TRUE)
)


## ----edit maxpower------------------------------------------------------------
# 2 - edit maxpower 
maxpower_data <- matrix(6,365,4)
suppressWarnings(
  writeHydroValues(area = "fr", 
                 type = "maxpower", 
                 data = maxpower_data)
)


## ----edit mingen--------------------------------------------------------------
# 3 - edit mingen
suppressWarnings(
  writeInputTS(area = "fr", type = "mingen", 
             data = mingen_data, 
             overwrite = TRUE)
)


## ----read mingen--------------------------------------------------------------
# read input time series
read_ts_file <- readInputTS(mingen = "all")
rmarkdown::paged_table(head(read_ts_file))

## ----delete study, include=FALSE----------------------------------------------
# Delete study
    unlink(opts$studyPath, 
           recursive = TRUE)
# clean global options
options(antares = NULL)

