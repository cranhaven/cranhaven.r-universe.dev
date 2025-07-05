## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "60%", fig.align = "center"
)

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------ Definición de funciones auxiliares-----------
#  
#  # Función: llenar_agujero
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
#  generar_mundo("mundo003")
#  while (frente_abierto()) {
#    avanzar()
#    llenar_agujero()
#    avanzar()
#  }
#  ejecutar_acciones()

## ---- eval=FALSE--------------------------------------------------------------
#  generar_mundo("mundo006")

## ---- out.width='50%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('24a.png')

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo106")
#  while (frente_abierto()) {
#    if (derecha_abierto()) {
#    	llenar_agujero()
#    }
#  	avanzar()
#  }
#  ejecutar_acciones()

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('24.png')

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo007")
#  while (frente_abierto()) {
#    if (derecha_abierto()) {
#    	llenar_agujero()
#    }
#  	avanzar()
#  }
#  ejecutar_acciones()

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('25.png')

## ---- eval = FALSE------------------------------------------------------------
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo007")
#  while (frente_abierto()) {
#    if (derecha_abierto()) {
#    	llenar_agujero()
#    }
#  	avanzar()
#  }
#  if (derecha_abierto()) {
#  	llenar_agujero()
#  }
#  ejecutar_acciones()

## ---- echo=FALSE, fig.align="center"------------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('26.gif')

## -----------------------------------------------------------------------------
# Función: avanzar_hasta_pared()
# Descripción: permite que Karel avance hasta que encuentre una pared
# Condición inicial: ninguna
# Condición final: Karel queda enfrentada a una pared
avanzar_hasta_pared <- function() {
  while (frente_abierto()) {
    avanzar()
  }
}

## ---- eval=FALSE--------------------------------------------------------------
#  # Función: recolectar_linea()
#  # Descripción: permite recolectar una línea consecutiva de cosos. La línea
#  # termina en la primera celda que no tiene cosos.
#  # Condición inicial: ninguna
#  # Condición final: Karel está en el final de la línea con la misma dirección que
#  # al inicio
#  recolectar_linea <- function() {
#    while (hay_cosos()) {
#      juntar_coso()
#      if (frente_abierto()) {
#        avanzar()
#      }
#    }
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  # Función: colocar_todo()
#  # Descripción: Karel coloca todos los cosos que tiene en su mochila en su
#  # posición actual
#  # Condición inicial: ninguna
#  # Condición final: ninguna
#  colocar_todo <- function() {
#    while (karel_tiene_cosos()) {
#      poner_coso()
#    }
#  }

## ---- echo=FALSE, fig.align="center"------------------------------------------
knitr::include_graphics('27.png')

## ---- eval = FALSE------------------------------------------------------------
#  generar_mundo("mundo008")
#  recolectar_todo()
#  colocar_todo()
#  volver_inicio()
#  ejecutar_acciones()

## ---- eval = FALSE------------------------------------------------------------
#  recolectar_todo <- function() {
#    while (frente_abierto()) {
#      recolectar_una_columna()
#      avanzar()
#    }
#    recolectar_una_columna()
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  recolectar_una_columna <- function() {
#    girar_izquierda()
#    recolectar_linea()
#    darse_vuelta()
#    avanzar_hasta_pared()
#    girar_izquierda()
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  # ------------ Definición de otras funciones auxiliares-----------
#  
#  # Función: recolectar_todo()
#  # Descripción: permite recolectar todos los cosos de cada columna moviéndose a
#  # lo largo de la primera calle.
#  # Condición inicial: Karel está mirando al este en la posición (1, 1)
#  # Condición final: Karel está mirando al este en la posición del extremo derecho
#  # de la primera fila
#  recolectar_todo <- function() {
#    while (frente_abierto()) {
#      recolectar_una_columna()
#      avanzar()
#    }
#    recolectar_una_columna()
#  }
#  
#  # Función: recolectar_una_columna()
#  # Descripción: permite recolectar todos los cosos en una única columna
#  # Condición inicial: Karel debe estar en la base de la columna mirando al este
#  # Condición final: Karel está en la misma posición y dirección que al inicio
#  recolectar_una_columna <- function() {
#    girar_izquierda()
#    recolectar_linea()
#    darse_vuelta()
#    avanzar_hasta_pared()
#    girar_izquierda()
#  }
#  
#  # Función: volver_inicio()
#  # Descripción: permite que Karel regrese a la posición (1, 1)
#  # Condición inicial: Karel mira al este en algún lugar de la primera fila
#  # Condición final: Karel está en la posición (1, 1) mirando al este
#  volver_inicio <- function() {
#    darse_vuelta()
#    avanzar_hasta_pared()
#    darse_vuelta()
#  }
#  
#  # ------------------- Programa principal -------------------
#  generar_mundo("mundo008")
#  recolectar_todo()
#  colocar_todo()
#  volver_inicio()
#  ejecutar_acciones()

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
if (knitr::is_html_output()) knitr::include_graphics('28.gif')

## ----  echo=FALSE, fig.align="center"-----------------------------------------
knitr::include_graphics('29.png')

## ---- eval = F----------------------------------------------------------------
#  # ------------------- Programa principal -------------------
#  
#  generar_mundo("mundo009")
#  while (no_hay_cosos()) {
#    girar_derecha()
#    while (frente_cerrado()) {
#      girar_izquierda()
#    }
#    avanzar()
#  }
#  ejecutar_acciones()

## ---- echo=FALSE, fig.align="center"------------------------------------------
if (knitr::is_html_output()) knitr::include_graphics('30.gif')

## ---- out.width='50%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('problema_1_1.png')

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('problema_1_3.png')

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('problema_1_4.png')

## ---- out.width='80%', echo=FALSE, fig.align="center"-------------------------
knitr::include_graphics('problema_1_5.png')

## ---- out.width='80%', echo=FALSE, fig.align="center", eval=TRUE--------------
knitr::include_graphics('problema1.png')

## ---- out.width='80%', echo=FALSE, fig.align="center", eval=TRUE--------------
knitr::include_graphics('problema2.png')

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # Programa principal
#  generar_mundo("mundo021")
#  avanzar()
#  duplicar_cosos()
#  avanzar()
#  ejecutar_acciones()

## ---- out.width='80%', echo=FALSE, fig.align="center", eval=TRUE--------------
knitr::include_graphics('problema3.png')

