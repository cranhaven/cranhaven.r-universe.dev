#' Choosing the input columns
#' @keywords internal
#' @return number of columns in the packageData Environment
getAmountCsvCheckBoxes <- function(){
    return(ncol(getEnvData("csvData")))
}

createCsvCheckBoxes <- function(){
    csvdata <- getEnvData("csvData")
    checkboxGroupInput(inputId = "csvColumnCheckBox",label = "",
                       choices = colnames(csvdata)[-1],
                       selected = colnames(csvdata)[-1],
                       inline = TRUE)
}

setAllCsvCheckBoxes <- function(session){
    csvdata <- getEnvData("csvData")
    updateCheckboxGroupInput(session,"csvColumnCheckBox",selected = colnames(csvdata)[-1])
}

resetAllCsvCheckBoxes <- function(session){
    updateCheckboxGroupInput(session,"csvColumnCheckBox",selected = character(0))
}

getCSVCols <- function(input){
    cols <- which(names(getEnvData("csvData")[-1]) %in% input$csvColumnCheckBox)
    cols <- cols + 1
    if(length(cols) >= 1){
        cols <- c(1,cols)
    }
    return(cols)
}
