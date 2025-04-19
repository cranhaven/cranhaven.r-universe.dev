## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)

## ----example pinamar, echo = TRUE, eval = FALSE, message=FALSE, warning=FALSE----
#  library(sgat)
#  initialization_sgat()
#  
#  rest.pinamar <- restaurantes_general("Pinamar, argentina")
#  
#  rest.pinamar <- paste(rest.pinamar, ", Pinamar, Argentina", sep = "")
#  
#  for(i in rest.pinamar){
#    sgat(i, tiempo.espera = 20, carpeta.guardado = "Pinamar")
#  }
#  

