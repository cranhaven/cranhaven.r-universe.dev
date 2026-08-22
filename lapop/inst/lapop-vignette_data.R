## ----setup, include=FALSE-----------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
#rm(list=ls()); gc()
## ----packages-----------------------------------------------------------------
library(lapop)
library(dplyr)

## ----filter data, evaluate=F, include=F---------------------------------------

# # -----------------------------------------------------------------------
# GRAND-MERGE (TOO LARGE FOR R PACKAGE)
# # -----------------------------------------------------------------------
#gm <- readstata13::read.dta13("C:/users/vidigar/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.1s).dta", generate.factors=T) # grand-merge v1.1 wealth variable fix
#gm <- lpr_data("C:/users/rob/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.1s).dta") # grand-merge v1.1 wealth variable fix

# Filter Countries and Variables
#gm23 <- gm %>% filter(!(pais %in% c(16, 25, 26, 27, 28, 30, 40, 41)))
#gm23 <- gm23 %>% select("aoj11", "ing4", "b13", "b21", "b31", "b12", "q14f",
#                        "wave", "pais_lab", "pais", "year")

#qs::qsave(gm23, paste0(Sys.getenv("HOME"), "\\GitHub\\lapop\\data\\gm23.qs", preset = "high"))

#saveRDS(gm, file=paste0(Sys.getenv("HOME"), "\\GitHub\\lapop\\data\\gm23.rds"))
#save(gm23, file=paste0(Sys.getenv("HOME"), "\\GitHub\\lapop\\data\\gm23.rda"))

# # -----------------------------------------------------------------------
# BRAZIL SINGLE-COUNTRY SINGLE-YEAR MERGE
# # -----------------------------------------------------------------------
bra <- readstata13::read.dta13("C:/Users/vidigar/Box/LAPOP Shared/2_Projects/2023 AB/BRA/Data Processing/BRA merge 2007-2023 LAPOP AmericasBarometer (v1.0s).dta")
bra23 <- bra %>% select("ing4", "b13", "b21", "b31", "b12", "fs2", "idio2", "wealth",
                        "wave", "pais", "year", "upm", "strata", "wt") %>%
                                                        filter(wave==2023)
bra23lpr<-lpr_data(bra23, wt = TRUE)

# rda
save(bra23, file="C:\\Users\\vidigar\\Documents\\GitHub\\lapop\\data\\bra23.rda")

# # -----------------------------------------------------------------------
# BRAZIL SINGLE-COUNTRY MULTI-YEAR MERGE
# # -----------------------------------------------------------------------
cm <- readstata13::read.dta13("C:/Users/vidigar/Box/LAPOP Shared/2_Projects/2023 AB/BRA/Data Processing/BRA merge 2007-2023 LAPOP AmericasBarometer (v1.0s).dta")
cm23 <- cm %>% select("ing4", "b13", "b21", "b31",
                      "wave", "pais", "year","upm", "strata", "weight1500")

cm23lpr<-lpr_data(cm23)

# rda
save(cm23, file="C:\\Users\\vidigar\\Documents\\GitHub\\lapop\\data\\cm23.rda")

# # -----------------------------------------------------------------------
# MULTI-COUNTRY SINGLE-YEAR AB 2023 MERGE
# # -----------------------------------------------------------------------
# GRAND MERGE
#gm <- haven::read_dta("C:/users/rob/Box/LAPOP Shared/2_Projects/2023 AB/Core_Regional/Data Processing/GM/Grand Merge 2004-2023 LAPOP AmericasBarometer (v1.1s).dta") # grand-merge v1.1 wealth variable fix

# Excluding countries and selecting waves
ym23 <- gm %>%
  filter(!(pais %in% c(26, 40, 41)) & wave %in% c(2018, 2023))

ym23 <- ym23 %>% select("b12", "b18",
                        "wave", "pais", "pais_lab",
                        "year",
                        "ing4", "pn4",
                        "vb21n", "q14f",
                        "edre", "wealth", "q1tc_r",
                        "upm", "strata", "weight1500")

#rda
save(ym23, file="C:\\Users\\vidigar\\Documents\\GitHub\\lapop\\data\\ym23.rda")

# # -----------------------------------------------------------------------
# EXPORTING
# # -----------------------------------------------------------------------

# CHECK FILES SIZE
tools::checkRdaFiles("data/")

# COMPRESS TO XZ FOR BEST FILESIZE
tools::resaveRdaFiles("data/", compress = "xz")
tools::checkRdaFiles("data/")
