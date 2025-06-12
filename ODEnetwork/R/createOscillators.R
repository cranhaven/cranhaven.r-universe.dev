#' Creates Set of Differential Equations
#' 
#' Creates the set of differential equations of order one from the \code{\link{ODEnetwork}}.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return a function with a set of differential equations of order one to use in a numerical step
#' @export
#' @examples
#' if (interactive()) {
#'   masses <- c(1, 2)
#'   dampers <- diag(c(0.1, 0.5))
#'   dampers[1, 2] <- 0.05
#'   springs <- diag(c(4, 10))
#'   springs[1, 2] <- 6
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   createOscillators(odenet)
#' }
createOscillators <- function(odenet) {
  UseMethod("createOscillators")
}

#' @method createOscillators ODEnetwork
#' @export
createOscillators.ODEnetwork <- function(odenet) {
  # Funktion ZeroDeriv falls vorhanden auslesen ansonsten immer NA zurueckgeben
  if (is.null(odenet$events$zeroderiv)) {
    fktZeroDeriv <- function(cState) NA
  } else {
    fktZeroDeriv <- odenet$events$zeroderiv
  }
  # Quelltext erstellen
  strFun <- "with(as.list(c(cState, cParameters)), {"
  # walk through the nodes and check for forcings
  for (i in 1:length(odenet$masses)) {
    for (strVar in c("x.", "v.")) {
      if (!is.null(odenet$events$linear[[paste(strVar, i, sep = "")]])) {
        # linear (...$linear$.. returns NA or number if it exist, NULL otherwise)
        strFun <- c(strFun, paste("if (!is.na(odenet$events$linear$", strVar, i, "(cTime)))", sep = ""))
        strFun <- c(strFun, paste(strVar, i, " <- odenet$events$linear$", strVar, i, "(cTime)", sep = ""))
      }
    }
  }
  # Einzelnen Knoten durchgehen und die Differentialgleichungen erstellen
  for (i in 1:length(odenet$masses)) {
    # keine aeussere Anregung mehr vorhanden, alle F. sind 0
    # Erstelle das Differentialgleichungs-System der Feder-Masse-Schwingers
    # Es werden nur Differentialgleichungen der 1. Ordnung verwendet
    # dx <- v
    strTemp <- paste("dx.", i, " <- v.", i, sep = "")
    # constant (fktZeroDeriv returns boolean if it exist, NA otherwise)
    if (is.na(fktZeroDeriv(paste("x.", i, sep = "")))) {
      # dirac, linear or no events
      strFun <- c(strFun, strTemp)
    } else {
      strFun <- c(strFun, paste("if (odenet$events$zeroderiv(\"x.", i, "\", cTime)) {", sep = "")) # if (zeroderiv(x.1)) {
      strFun <- c(strFun, paste("dx.", i, " <- 0", sep = ""))
      strFun <- c(strFun, "} else {")   # } else {
      strFun <- c(strFun, strTemp)
      strFun <- c(strFun, "}")
    }
    # dv1 <- (F1 - d*v1 - k*(x1+r1) - d12*(v1-v2) - k12*(x1-x2+r12)) / m1
    # nur falls am Knoten eine aussere Anregung oder Anregungsaenderung vorliegt die Anregung einbauen
    # r1 wird addiert, da r1 negiert abgelegt wird. Entsprechend gilt r21=-r12, damit das plus-Zeichen
    # bei den Verbindungsfedern bleibt
    strTemp <- paste("dv.", i, " <- (", sep = "")
    # Daempfer der aktuellen Masse
    strTemp <- paste(strTemp, " - d.", i, "*v.", i, sep = "")
    # Feder der aktuellen Masse
    if (odenet$distances[i, i] == 0) {
      strTemp <- paste(strTemp, " - k.", i, "*x.", i, sep = "")
    } else {
      strTemp <- paste(strTemp, " - k.", i, "*(x.", i, "+r.", i, ")", sep = "")
    }
    # Feder und Daempfer aller Koppelelemente
    for (j in 1:length(odenet$masses)) {
      # aktuellen Knoten ueberspringen
      if (j == i)
        next
      # Daempfer und Feder, falls vorhanden
      if (odenet$dampers[i, j] != 0) {
        strTemp <- paste(strTemp, " - d.", i, ".", j, "*(v.", i, "-v.", j, ")", sep = "")
      }
      if (odenet$springs[i, j] != 0) {
        strTemp <- paste(strTemp, " - k.", i, ".", j, "*(x.", i, "-x.", j, sep = "")
        if (odenet$distances[i, j] == 0) {
          strTemp <- paste(strTemp, ")", sep = "")
        } else {
          strTemp <- paste(strTemp, "+r.", i, ".", j, ")", sep = "")
        }
      }
    }
    # Abschluss: Klammer schliessen und durch Masse teilen
    strTemp <- paste(strTemp, ")/m.", i, sep = "")
    if (is.na(fktZeroDeriv(paste("v.", i, sep = "")))) {
      strFun <- c(strFun, strTemp)
    } else {
      strFun <- c(strFun, paste("if (odenet$events$zeroderiv(\"v.", i, "\", cTime)) {", sep = "")) # if (zeroderiv(v.1)) {
      strFun <- c(strFun, paste("dv.", i, " <- 0", sep = ""))
      strFun <- c(strFun, "} else {")   # } else {
      strFun <- c(strFun, strTemp)
      strFun <- c(strFun, "}")
    }
  }
  
  # Rueckgabeliste der Werte des Zustandsraumes
  strTemp <- "list(c("
  for (i in 1:length(odenet$masses)) {
    # Auslenkung und Geschwindigkeit der Feder-Masse-Schwinger
    strTemp <- paste(strTemp, "dx.", i, ", dv.", i, sep = "")
    if (i < length(odenet$masses))
      strTemp <- paste(strTemp, ",")
    else
      strTemp <- paste(strTemp, ")", sep = "")
  }
  # add return of forcings
  for (i in 1:length(odenet$masses)) {
    for (strVar in c("x.", "v.")) {
      if (!is.null(odenet$events$linear[[paste(strVar, i, sep = "")]])) {
        strTemp <- paste(strTemp, ", ", strVar, i, "Force = odenet$events$linear$", strVar, i, "(cTime)", sep = "")
      }
    }
  }
  # close list
  strTemp <- paste(strTemp, ")", sep = "")
  strFun <- c(strFun, strTemp)
  
  # Ende von 'with(as.list(
  strFun <- c(strFun, "})")
  
  # leere Funktion erstellen
  fktOszillator <- function() {}
  # Eingabeparameter einstellen
  formals(fktOszillator) <- alist(cTime=, cState=, cParameters=)
  # Funktionstext in Funktion verpacken
  expstrFun <- parse(text = strFun)
  # Funktion in den Koerper der leeren Funktion packen
  body(fktOszillator) <- as.call(c(as.name("{"), expstrFun))
  
  # Fertige Funktion des DGL-Systems ausgeben
  return(fktOszillator)
}
