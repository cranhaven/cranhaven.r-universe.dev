## ----knitrsetup, include = FALSE----------------------------------------------

library(reticulate)
# this vignette requires python 3.7 or newer to run
eval <- tryCatch({
  numeric_version(py_config()$version) >= "3.7" && py_numpy_available() && 
    py_module_available("scipy") && py_module_available("sklearn")
}, error = function(e) FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = eval
)

## ----eval=TRUE----------------------------------------------------------------
library(CytOpT)
data("HIPC_Stanford")

## -----------------------------------------------------------------------------
knitr::kable(head(HIPC_Stanford_1228_1A))

## -----------------------------------------------------------------------------
gold_standard_manual_prop <- c(table(HIPC_Stanford_1369_1A_labels)/length(HIPC_Stanford_1369_1A_labels))

## -----------------------------------------------------------------------------
set.seed(123)
res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
              Lab_source = HIPC_Stanford_1228_1A_labels,
              theta_true = gold_standard_manual_prop,
              method='both', monitoring = TRUE)

## -----------------------------------------------------------------------------
summary(res)

## ----fig.width = 7, fig.asp = .8, fig.retina=2--------------------------------
plot(res)

## ----BA, fig.width = 7, fig.asp = .6, fig.retina = 2--------------------------
Bland_Altman(res$proportions)

