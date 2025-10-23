#' Find models for species
#'
#' @details The function looks for the species in the database and returns
#'   the number of occurrences for each model.
#' @param orderSP Find models by Order.
#' @param familySP Find models by Family.
#' @param species Find models by species (Genus species).
#' @return A data.frame with the name of the equations, the number of
#'   occurrences in the database, and the number of parameters for each
#'   equation.
#' @examples
#' devRateFind(orderSP = "Lepidoptera")
#' devRateFind(familySP = "Gelechiidae")
#' ## detailed example:
#' devRateFind(species = "Tuta absoluta")
#' ## campbell_74 model has been used for T. absoluta
#' ## Parameters from the campbell equation can be accessed by:
#' ## campbell_74$startVal[campbell_74$startVal["genSp"] == "Tuta absoluta",]
#' @export
devRateFind <- function(orderSP = "", familySP = "", species = ""){
  devRateEqList <- devRateEqList # avoid "no visible binding for global variable devRateEqList" Note
  if(length(c(orderSP, familySP, species)) == 3 &&
     is.character(orderSP) &&
     is.character(familySP) &&
     is.character(species)){
    vFind <- vector()
    vEq <- vector()
    for(i in names(devRateEqList)){
      eq <- get(i)
      if(orderSP != "" & familySP == "" & species == ""){
        if(orderSP %in% eq$startVal[,"ordersp"]){ #  == TRUE
          occu <- sum(as.character(eq$startVal[,"ordersp"]) == orderSP)
          vFind <- c(vFind, occu)
          vEq <- c(vEq, i)
        }
      }
      if(familySP != "" & species == ""){
        if(familySP %in% eq$startVal[,"familysp"]){ # == TRUE
          occu <- sum(as.character(eq$startVal[,"familysp"]) == familySP)
          vFind <- c(vFind, occu)
          vEq <- c(vEq, i)
        }
      }
      if(species != ""){
        if(species %in% eq$startVal[,"genSp"]){ # == TRUE
          occu <- sum(as.character(eq$startVal[,"genSp"]) == species)
          vFind <- c(vFind, occu)
          vEq <- c(vEq, i)
        }
      }
    }
    vParam <- sapply(vEq, function(j){
      sum(grepl(names(get(j)$startVal), pattern = "^(param)"))
    })
    dfFind <- data.frame(equation = vEq, occu = vFind, paramNumb = vParam)
    dfFind <- dfFind[order(dfFind[, 2], decreasing = TRUE), ]
    rownames(dfFind) <- NULL
    return(dfFind)
  } else {
    return("Error in arguments provided: only one argument of type character is allowed")
  }
}
