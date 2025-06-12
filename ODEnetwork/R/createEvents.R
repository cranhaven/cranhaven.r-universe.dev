#' Creates Events
#' 
#' Creates functions for constant and linear events of the given \code{\link{ODEnetwork}}
#' to know when events have to be replaced or forced.
#'
#' @param odenet [\code{ODEnetwork}]\cr
#'   List of class \code{\link{ODEnetwork}}.
#' @return an extended list of class [\code{\link{ODEnetwork}}].
#' @examples
#' if (interactive()) {
#'   masses <- 1
#'   dampers <- as.matrix(1.5)
#'   springs <- as.matrix(4)
#'   odenet <- ODEnetwork(masses, dampers, springs)
#'   eventdat <- data.frame(  var = c("x.1", "x.1")
#'                          , time = c(1, 3)
#'                          , value = c(1, 3)
#'                          , stringsAsFactors = TRUE
#'                          )
#'   odenet <- setState(odenet, 0, 0)
#'   odenet <- setEvents(odenet, eventdat)
#'   createEvents(odenet)
#' }
createEvents <- function(odenet) {
  UseMethod("createEvents")
}

#' @method createEvents ODEnetwork
#' @export
createEvents.ODEnetwork <- function(odenet) {
  if (is.null(odenet$events))
    return(odenet)
  
  # https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  var <- NULL
  
  # read events data
  dfEvents <- odenet$events$data
  # throw warning
  if (odenet$coordtype == "polar" && odenet$events$type != "linear")
    warning("Dirac and constant events are not converted from polar to cartesian.")
  # calc events or forcings
  if (odenet$events$type == "constant") {
    # add function to know when the oscillator can move free
    strFun <- ""
    for (strVar in levels(dfEvents$var)) {
      strFun <- paste(strFun, "if (cState == \"", strVar, "\") {", sep = "")
      cTemp <- range(subset(dfEvents, var == strVar)$time)
      strFun <- paste(strFun, "ifelse (cTime < ", cTemp[1], " || cTime > ", cTemp[2], ", FALSE, TRUE)" , sep = "")
      strFun <- paste(strFun, "} else ", sep = "")
    }
    strFun <- paste(strFun, "{ NA }", sep = "")
    # leere Funktion erstellen
    fktDerivZero <- function() {}
    # Eingabeparameter einstellen
    formals(fktDerivZero) <- alist(cState=, cTime=0)
    # Funktionstext in Funktion verpacken
    expstrFunktion <- parse(text = strFun)
    # Funktion in den K?rper der leeren Funktion packen
    body(fktDerivZero) <- as.call(c(as.name("{"), expstrFunktion))
    # save in odenet
    odenet$events$zeroderiv <- fktDerivZero
  } else {
    # true for dirac and linear (redundant, but playing safe)
    odenet$events$zeroderiv <- NULL
  }
  
  if (odenet$events$type == "linear") {
    if (odenet$coordtype == "polar") {
      # pairwise polar coordinates are necessary, delete other variable columns
      dfEventsR <- stats::reshape(dfEvents, v.names = "value", idvar = "time", timevar = "var", direction = "wide")
      # pairwise check
      for (i in 1:length(odenet$masses)) {
        strSubs <- paste(c("value.m", "value.a"), i, sep = ".")
        # check pairwise variables available
        switch(sum(strSubs %in% colnames(dfEventsR))+1
               , next
               , stop(paste("Cannot convert: The linear events for oscillator", i, "are not pairwise."))
               )
        # check pairwise coordinates within the current vector
        # (NA, NA) and (5, 6) coordinates are ok, NAs are inherited by reshape
        # => xor(col1, col2) should all be false
        # this is checked by prod(!xor())
        if (!prod(!xor(is.na(dfEventsR[, strSubs[1]]), is.na(dfEventsR[, strSubs[2]])))) {
          stop(paste("Connot convert: Coordinates for oscillator", i, "are not completely pairwise."))
        }
        # convert to cartesian coordinates
        blnNA <- is.na(dfEventsR[, strSubs[1]])
        dfEventsR[!blnNA, strSubs] <- convertCoordinates(as.matrix(dfEventsR[!blnNA, strSubs]))
      }
      # reshape and order to origin format
      dfEvents <- stats::reshape(dfEventsR, direction = "long")
      dfEvents <- dfEvents[, c("var", "time", "value", "method")]
      # replace "m" with "x" and "a" with "v"
      levels(dfEvents$var) <- gsub("m", "x", levels(dfEvents$var))
      levels(dfEvents$var) <- gsub("a", "v", levels(dfEvents$var))
    }    
    # create empty function
    fktLinInter <- function() {}
    # initialise this part to make it accessable for odenet$events$linear[[strVar]]
    odenet$events$linear$complete <- fktLinInter
    # add linear interpolated function (forcings)
    strFun <- "switch(cState"
    for (strVar in levels(dfEvents$var)) {
      # no interpolation with one point
      if (table(dfEvents$var)[strVar] == 1)
        next
      # create function
      odenet$events$linear[[strVar]] <- stats::approxfun(subset(dfEvents, var == strVar)[, c("time", "value")])
      # add link to switch statement
      strFun <- paste(strFun, ", ", strVar, " = odenet$events$linear$", strVar, "(cTime)", sep = "")
    }
    strFun <- paste(strFun, ")", sep = "")
    # add parameters
    formals(fktLinInter) <- alist(cState=, cTime=0)
    # parse function text to build a function
    expstrFunktion <- parse(text = strFun)
    # add function text to body
    body(fktLinInter) <- as.call(c(as.name("{"), expstrFunktion))
    # save in odenet
    odenet$events$linear$complete <- fktLinInter
  } else {
    # (redundant, but playing safe)
    odenet$events$linear <- NULL
  }
  
  return(odenet)
}
