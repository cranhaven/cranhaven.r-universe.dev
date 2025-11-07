new_LongData <- function(myData, ID, DV) {
  stopifnot(
    is.data.frame(myData),
    is.character(ID), length(ID) == 1,
    is.character(DV), length(DV) == 1
  )
  structure(myData,
            class = c("LongData", class(myData)),
            ID = ID,
            DV = DV)
}

as_LongData <- function(myData, ID, DV) {
  if (!inherits(myData, "LongData")) {
    return(new_LongData(myData, ID, DV))
  }

  if (!missing(ID)) {
    attr(myData, "ID") <- ID
  }
  if (!missing(DV)) {
    attr(myData, "DV") <- DV
  }

  myData
}

getID <- function(longData) {
  stopifnot(inherits(longData, "LongData"))
  attr(longData, "ID")
}

getDV <- function(longData) {
  stopifnot(inherits(longData, "LongData"))
  attr(longData, "DV")
}
