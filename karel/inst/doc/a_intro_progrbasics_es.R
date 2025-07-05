## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- out.width='100%', fig.cap='Captura de pantalla de RStudio', echo=FALSE, fig.align="center"----
knitr::include_graphics('abrir_rstudio.png')

## -----------------------------------------------------------------------------
1 + 2
5 * 3
exp(2)
sqrt(100)
1 / 0
(2 + 3i) * (3 + 6i)
1i ^ 2

## -----------------------------------------------------------------------------
5^1
# 5^2
5^3

## ---- eval=FALSE--------------------------------------------------------------
#  # Instalar desde CRAN el paquete karel
#  install.packages("karel")

## ---- eval=FALSE--------------------------------------------------------------
#  # Instalar desde CRAN el paquete devtools
#  install.packages("devtools")
#  # Instalar desde GitHub el paquete karel
#  devtools::install_github("mpru/karel")

