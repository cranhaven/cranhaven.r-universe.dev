## ----setup_general, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
   echo = TRUE,
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup_lib, include=FALSE-------------------------------------------------
if (!requireNamespace("htmltools", quietly = TRUE)) {
  stop("Package 'htmltools' is required to run this function. Please install it.")
}
library(growthTrendR)
library(data.table)
library(ggplot2)
library(sf)



## ----mod_data,  message = FALSE, warning = FALSE, results = 'hide'------------


# loading processed ring measurement
dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

# climate
dt.clim <- fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))

# merge data
dt.samples_clim <- merge(dt.samples_trt$tr_all_wide[, c("uid_site", "site_id","latitude", "longitude",  "species", "uid_tree", "uid_radius")], dt.samples_trt$tr_all_long$tr_7_ring_widths, by = "uid_radius")

# # Calculate BAI
dt.samples_clim <- calc_bai(dt.samples_clim)

dt.samples_clim <- merge(dt.samples_clim, dt.clim, by = c("site_id", "year"))

## ----mod_fitting,  message = FALSE, warning = FALSE, results = 'hide'---------
setorder(dt.samples_clim, uid_tree, year)

# Remove ageC == 1 prior to fitting log-scale models.
dt.samples_clim <- dt.samples_clim[ageC > 1]
m.sp <- gamm_spatial(data = dt.samples_clim, resp_scale = "resp_log",
                     m.candidates =c( "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
                                      "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD")
)

## ----mod_report_demo, message = FALSE, warning = FALSE------------------------
# generate_report(robj = m.sp)
gam_model <- m.sp$model$gam

# summary of the model
  summary(gam_model)
# smooth term importance
 term_important <- sterm_imp( gam_model)
 print(term_important)
# 


