## ----include=FALSE------------------------------------------------------------
is_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  message = FALSE, warning = FALSE,
  fig.path = "figures/"
)
# Si el cómputo es pesado, deja los chunks como eval = !is_cran

