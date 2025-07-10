## ----install-bioc, message=FALSE, warning = FALSE, eval=FALSE-----------------
#  BiocManager::install("splatter")
#  
#  library(devtools)
#  install_github("thecailab/SCRIP")

## ----quickstart, message=FALSE, warning = FALSE-------------------------------
library(splatter)
library(SCRIP)
 
data(acinar.data)
params <- splatEstimate(acinar.data)

sim_trend <-  SCRIPsimu(data=acinar.data, params=params, mode="GP-trendedBCV")
sim_trend

## ---- message=FALSE, warning = FALSE------------------------------------------
########################### GP-commonBCV model/Splatter ##########################
##################################################################################
sim_GPcommon <-  SCRIPsimu(data=acinar.data, params=params, mode="GP-commonBCV")
sim_GPcommon

## ---- message=FALSE, warning = FALSE------------------------------------------
############################### GP-trendedBCV model ##############################
##################################################################################
sim_GPtrend <-  SCRIPsimu(data=acinar.data, params=params, mode="GP-trendedBCV")

## ---- message=FALSE, warning = FALSE------------------------------------------
############################### BP-commonBCV model ##############################
##################################################################################
sim_BP <-  SCRIPsimu(data=acinar.data, params=params, mode="BP")

## ---- message=FALSE, warning = FALSE------------------------------------------
############################### BP-commonBCV model ##############################
##################################################################################
sim_BGPcommon <-  SCRIPsimu(data=acinar.data, params=params, mode="BGP-commonBCV")

## ---- message=FALSE, warning = FALSE------------------------------------------
############################### BP-trendedBCV model ##############################
##################################################################################
sim_BGPtrend <-  SCRIPsimu(data=acinar.data, params=params, mode="BGP-trendedBCV")

## ---- message=FALSE, warning = FALSE------------------------------------------
sim.SCRIP2 <- SCRIPsimu(data=acinar.data, params=params, method="groups", 
                        batchCells=300, group.prob = c(0.25, 0.25, 0.25, 0.25), 
                        de.prob = c(0.2, 0.2, 0.2, 0.2), 
                        de.downProb = c(0.5, 0.5, 0.5, 0.5),
                        de.facLoc = c(0.2, 0.3, 0.4, 0.5), 
                        de.facScale=c(0.2, 0.2, 0.2, 0.2)) 

## ---- message=FALSE, warning = FALSE------------------------------------------
sim.SCRIP3 <- SCRIPsimu(data=acinar.data, params=params, method="groups", 
                        batchCells=c(150, 150), 
                        batch.facLoc = c(0.1, 0.1), 
                        batch.facScale = c(0.1, 0.1), 
                        group.prob = c(0.25, 0.25, 0.25, 0.25), 
                        de.prob = c(0.2, 0.2, 0.2, 0.2), 
                        de.downProb = c(0.5, 0.5, 0.5, 0.5),
                        de.facLoc = c(0.2, 0.3, 0.4, 0.5), 
                        de.facScale=c(0.2, 0.2, 0.2, 0.2)) 

