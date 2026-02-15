#' checkInputCorrectness
#'
#' Main Input checking function, calls all subChecks and creates User-Dialogs for the respective problems.
#'
#' @param input GUi Inputs
#'
#' @return TRUE for correct user input, FALSE for a faulty configuration
checkInputCorrectness <- function(input){
    inputsCorrect <- 0

    # if(!checkInputBounds(input)){
    #     inputsCorrect <- 1
    # }
    #
    # if(!checkInputObjectiveFunction(input)){
    #     inputsCorrect <- 2
    # }

    if(inputsCorrect == 0){
        return(TRUE)
    }else if(inputsCorrect == 1){
        showModal(modalDialog(title="Configuration Error","There is an error in your objective
                              function configuration!\nPlease Check for typos etc.
                              in your bounds and dimension amount."
                              ,footer=NULL,easyClose=TRUE))
        return(FALSE)
    }else if(inputsCorrect == 2){
        showModal(modalDialog(title="Configuration Error","There is an error in the objective
                              function you specified!\nMaybe it does not exist or is faulty, please check for typos."
                              ,footer=NULL,easyClose=TRUE))
        return(FALSE)
    }else{
        showModal(modalDialog(title="Configuration Error","There seems to be an error in your setup
                              ,please make sure that your configuration is correct."
                              ,footer=NULL,easyClose=TRUE))
        return(FALSE)
    }
}
