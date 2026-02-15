# Functions for organizing package data

#' Package Data Environment
#'
#' Data Environment for storage and variable exchange between functions
packageDataEnv <- new.env()

#' Access a variable in the packageData Environment
#'
#' @param strName String/Character vector. Name of variable that is to be accessed.
#'
#' @return current value of the variable
#' @keywords internal
getEnvData <- function(strName){
    if(!exists(strName,envir = packageDataEnv)){
        return(NULL)
    }
    get(strName,envir = packageDataEnv)
}

#' Set a value to a variable in the packageData Environment
#'
#' @param strName String/Character vector. Name of variable that is to be accessed.
#' @param value Any value that shall be set to the respective variable
#'
#' @keywords internal
#' @return None
setEnvData <- function(strName, value){
    assign(strName,value,envir = packageDataEnv)
}
