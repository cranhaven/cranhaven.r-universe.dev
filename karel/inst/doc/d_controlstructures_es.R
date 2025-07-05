## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "60%", fig.align = "center"
)

## ---- eval=FALSE--------------------------------------------------------------
#  if (condición) {
#  	...código para ejecutar acciones...
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------ Definición de funciones auxiliares-----------
#  
#  llenar_agujero <- function() {
#    girar_derecha()
#    avanzar()
#    if (no_hay_cosos()) {
#      poner_coso()
#    }
#    darse_vuelta()
#    avanzar()
#    girar_derecha()
#  }
#  
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo002")
#  avanzar()
#  llenar_agujero()
#  ejecutar_acciones()

## ---- eval=FALSE--------------------------------------------------------------
#  if (condición) {
#  	...código para ejecutar acciones...
#  } else {
#  	...código para ejecutar acciones...
#  }

## ----  echo=FALSE, fig.align="center", eval=TRUE------------------------------
knitr::include_graphics('51.png')

## ---- eval=FALSE--------------------------------------------------------------
#  generar_mundo("mundo001")
#  if (hay_cosos()) {
#    juntar_coso()
#  } else {
#    poner_coso()
#  }
#  avanzar()
#  if (hay_cosos()) {
#    juntar_coso()
#  } else {
#    poner_coso()
#  }
#  avanzar()
#  if (hay_cosos()) {
#    juntar_coso()
#  } else {
#    poner_coso()
#  }
#  ejecutar_acciones()

## ----  echo=FALSE, fig.align="center"-----------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('52.gif')

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------ Definición de funciones auxiliares-----------
#  
#  invertir <- function() {
#    if (hay_cosos()) {
#      juntar_coso()
#    } else {
#      poner_coso()
#    }
#  }
#  
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo001")
#  invertir()
#  avanzar()
#  invertir()
#  avanzar()
#  invertir()
#  ejecutar_acciones()

## ---- eval=FALSE--------------------------------------------------------------
#  if (condición 1) {
#  	...Primer conjunto de acciones...
#  } else if (condición 2) {
#  	...Segundo conjunto de acciones...
#  } else {
#  	...Tercer conjunto de acciones...
#  }

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('21.png')

## ---- eval=FALSE, highlight=FALSE---------------------------------------------
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo003")
#  avanzar()
#  llenar_agujero()
#  avanzar()
#  
#  avanzar()
#  llenar_agujero()
#  avanzar()
#  
#  avanzar()
#  llenar_agujero()
#  avanzar()
#  
#  avanzar()
#  llenar_agujero()
#  avanzar()
#  
#  avanzar()
#  llenar_agujero()
#  avanzar()
#  ejecutar_acciones()

## ---- eval=FALSE--------------------------------------------------------------
#  generar_mundo("mundo003")
#  for (i in 1:5) {
#    avanzar()
#    llenar_agujero()
#    avanzar()
#  }
#  ejecutar_acciones()

## ---- eval=FALSE, highlight=FALSE---------------------------------------------
#  for (<variable> in <valor1>:<valor2>) {
#  	...Acción/es...
#  }

## ---- echo=FALSE, fig.align="center"------------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('22.gif')

## ---- eval=FALSE--------------------------------------------------------------
#  while (<condición>) {
#  	...Acción/es a repetir...
#  }

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('23.png')

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------------- Programa principal -------------------
#  generar_mundo("mundo003")
#  while (frente_abierto()) {
#    avanzar()
#    llenar_agujero()
#    avanzar()
#  }
#  ejecutar_acciones()

## ---- eval=FALSE--------------------------------------------------------------
#  # No correr esto! (o sí, para ver cómo no anda!)
#  generar_mundo("mundo003")
#  while (no_hay_cosos()) {
#  	girar_izquierda()
#  }
#  ejecutar_acciones()

