## ---- include = FALSE---------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
opts_chunk$set(purl = NOT_CRAN)

