## ----configuracion, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE,
  error = FALSE,
  warning = FALSE,
  message = FALSE
)

## ----configuracion-sivirep, include = FALSE-----------------------------------
library(sivirep)

## ----import-sivirep, eval = FALSE---------------------------------------------
#  library(sivirep)

## ----list-events-ms, eval = FALSE---------------------------------------------
#  lista_eventos <- list_events()
#  knitr::kable(lista_eventos)

## ----list-events-rs, echo = FALSE---------------------------------------------
lista_eventos <- NULL
tryCatch({
  lista_eventos <- list_events()
  knitr::kable(lista_eventos,
             col.names = c("Codigo", "Enfermedad", "Año"),
             row.names = FALSE, align = "l")
}, error = function(e) {
  lista_eventos <- NULL
})

