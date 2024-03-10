params <-
list(EVAL = FALSE)

## ----setup, include=FALSE---------------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::knit_hooks$set(purl = knitr::hook_purl)
options(width = 90)
# fig.width = 6, fig.height=4, fig.asp = 1.0,
knitr::opts_chunk$set(
  echo = FALSE,
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  purl = NOT_CRAN,
  eval = NOT_CRAN,
  dev = "jpeg",
  dpi = 30,
  fig.cap = "",
  fig.asp = 0.8,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center"
)

