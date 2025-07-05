## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width='60%'
)

## ---- eval=F------------------------------------------------------------------
#  library(karel)

## ---- eval=F------------------------------------------------------------------
#  generar_mundo("mundo001")

## ---- echo=FALSE, fig.align="center"------------------------------------------
knitr::include_graphics('13.png')

## ---- eval=FALSE--------------------------------------------------------------
#  # Este programa hace que Karel avance un espacio, junte un coso y vuelva a avanzar
#  generar_mundo("mundo001")
#  avanzar()
#  juntar_coso()
#  avanzar()
#  ejecutar_acciones()

## ---- echo=FALSE, fig.align="center"------------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('14.gif')

## ---- out.width='100%', echo=FALSE, fig.align="center"------------------------
knitr::include_graphics('15.png')

## ---- echo=FALSE, fig.align="center"------------------------------------------
knitr::include_graphics('16.png')

## ---- eval=FALSE--------------------------------------------------------------
#  generar_mundo("mundo001")
#  avanzar()
#  juntar_coso()
#  avanzar()
#  girar_izquierda()
#  avanzar()
#  girar_izquierda()
#  girar_izquierda()
#  girar_izquierda()
#  avanzar()
#  avanzar()
#  poner_coso()
#  avanzar()
#  ejecutar_acciones()

## ----  echo=FALSE, fig.align="center"-----------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('18.gif')

