## ----include = FALSE----------------------------------------------------------
Sys.setenv(R_USER_LIBS = tempdir())  #Just in case for CRAN
library(biodosetools)
knitr::opts_chunk$set(
  fig.dpi = 96,
  collapse = TRUE,
  comment = "#>"
)

## ----sc-dic-limits-01, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Data input options' in the characteristic limits module - Compare proband vs control method."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-01.png")

## ----dic-limits-results-01, tidy=TRUE, tidy.opts=list(width.cutoff=60)--------
control_data <- c(aberr = 1,
                  cells = 1000)

proband_data <- c(aberr = 13,
                  cells = 1000)
        

result_data <- matrix(c(
    control_data["aberr"] / control_data["cells"],
    proband_data["aberr"] / proband_data["cells"],
    stats::poisson.test(c(proband_data["aberr"], control_data["aberr"]),
                        c(proband_data["cells"], control_data["cells"]))$p.value
      ), ncol = 3, nrow = 1)
    
    colnames(result_data) <- c("dics/cell (control)", "dics/cell (case)", "P-value")

## -----------------------------------------------------------------------------
result_data

## ----sc-dic-limits-02, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Data input options' in the characteristic limits module - Characteristic limits."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-02.png")

## ----sc-dic-limits-03, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Data input options' in the characteristic limits module - Characteristic limits - Without curve data option."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-03.png")

## ----sc-dic-limits-04, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Data input options' in the characteristic limits module - Characteristic limits - Loaded data .rds option."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-04.png")

## ----sc-dic-limits-05, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Data input options' in the characteristic limits module - Characteristic limits - Manually entered curve data option."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-05.png")

## ----dic-limits-results-02----------------------------------------------------
cells_proband <- c(20, 50, 100, 200, 500, 1000)
control_data <- c(aberr = 4,
                  cells = 1000)

c_limits <- sapply(cells_proband, function(x) calculate_characteristic_limits(
  y0 = control_data["aberr"],
  n0 = control_data["cells"],
  n1 = x,
  alpha = 0.05, 
  beta = 0.1, 
  ymax = 100, 
  type = "var"
  ))
  

## -----------------------------------------------------------------------------
c_limits

## ----dic-limits-results-03----------------------------------------------------
fit_results_list <- system.file("extdata", "dicentrics-fitting-results.rds", package = "biodosetools")%>%
  readRDS()
fit_coeffs <- fit_results_list$fit_coeffs[, "estimate"]

 est_dec <- sapply((unlist(c_limits["decision_threshold", ]) + 1) / cells_proband, function(x) project_yield(
   yield = x,
   type = "estimate",
   general_fit_coeffs = fit_coeffs,
   general_fit_var_cov_mat = NULL,
   protracted_g_value = 1,
   conf_int = 0))
  
 est_det <- sapply(unlist(c_limits["detection_limit", ]) / cells_proband, function(x) project_yield(
   yield = x,
   type = "estimate",
   general_fit_coeffs = fit_coeffs,
   general_fit_var_cov_mat = NULL,
   protracted_g_value = 1,
   conf_int = 0))                

## -----------------------------------------------------------------------------
est_dec
est_det

## ----sc-dic-limits-07, echo=FALSE, out.width='100%', fig.align='center', fig.cap="'Results' tabbed box in the characteristic limits module - Characteristic limits."----
knitr::include_graphics("figures/screenshot-dicentrics-limits-06.png")

