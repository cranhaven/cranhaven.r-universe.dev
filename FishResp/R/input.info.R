#' Input the Information about Individuals and Chambers
#'
#' The function is used to input manually the information required for metabolic rate calculations: ID and wet mass of individuals, volume of chambers. Values of those parameters should be filled in the same order in a vector format replacing default NA values in the template. In addition, specify which unit has been used to measure dissolved oxygen concentration.
#'
#' @usage
#' input.info(ID = c(NA, NA, NA, NA, NA, NA, NA, NA),
#'            Mass = c(NA, NA, NA, NA, NA, NA, NA, NA),
#'            Volume = c(NA, NA, NA, NA, NA, NA, NA, NA),
#'            DO.unit = c("mg/L", "mmol/L", "ml/L"))
#'
#' @param ID  string: ID of fish or another aquatic organism
#' @param Mass  numeric: wet mass of an individual in grams (g)
#' @param Volume  numeric: the volume of a chamber in milliliters (mL) or the whole respirometry loop (if measured)
#' @param DO.unit  character: dissolved oxygen used in raw data should be measured in 'mg/L', 'mmol/L' or 'ml/L'. If other measurement units were used, convert them to 'mg/L', 'mmol/L' or 'ml/L' using the function \code{\link{convert.respirometry}} or \code{\link{convert.rMR}}.
#'
#'
#' @details It is especially important to keep such format of vectors when not the full number of individuals is in a multi-channel respirometry system. E.g.: if you use a 4-channel respirometry system with three fish and only Chamber 1 is empty, but data are still collected from there, do not remove NA values for that chamber to prevent the shift of actual data between the chambers.
#'
#' @return The function returns a data frame with four columns: "ID", "Mass", "Volume", "DO.unit". The data frame is used in the functions \code{\link{import.test}}, \code{\link{import.meas}}, and \code{\link{correct.meas}}.
#'
#' @importFrom grDevices dev.new
#' @importFrom graphics abline legend par plot
#' @importFrom stats coef lm predict.lm
#' @importFrom utils head read.table tail write.table
#'
#' @examples
#' # Four sticklebacks in a 4-channel respirometry system
#' info <- input.info(ID = c("Stickleback_1", "Stickleback_2",
#'                           "Stickleback_3", "Stickleback_4"),
#'                    Mass = c(1.86, 1.92, 2.23, 1.80),
#'                    Volume = c(250, 250, 250, 250),
#'                    DO.unit = "mg/L")
#'
#' @export

input.info <- function(ID = c(NA, NA, NA, NA, NA, NA, NA, NA),
                 Mass = c(NA, NA, NA, NA, NA, NA, NA, NA),
                 Volume = c(NA, NA, NA, NA, NA, NA, NA, NA),
                 DO.unit = c("mg/L", "mmol/L", "ml/L")){

  info.data <- as.data.frame(cbind(ID, Mass, Volume))
  info.data$Mass <- as.numeric(as.character(info.data$Mass))
  info.data$Volume <- as.numeric(as.character(info.data$Volume))

  if(DO.unit == "mg/L"){
    DO <- "mg O2"
  }

  else if(DO.unit == "mmol/L"){
    DO <- "mmol O2"
  }

  else if(DO.unit == "ml/L"){
    DO <- "ml O2"
  }

  else{
    print("Please, choose dissolved oxygen unit: mg/L, mmol/L or ml/L")
  }

  info.data$DO.unit <- as.character(DO)

  print(info.data)
  return(info.data)
}
