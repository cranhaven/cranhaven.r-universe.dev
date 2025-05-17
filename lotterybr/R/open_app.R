#' Open Shiny app for data visualization
#'
#' This function opens the Shiny application corresponding to the specified language for the Brazilian lottery data.
#'
#' @param language select which language the app will have. Options are "pt" for Portuguese and "en" for English.
#'
#' @return None. This function launches the Shiny application.
#' @description This function opens a shiny app that uses the datasets provided
#'
#' @import shiny
#' @export
#'

#'

open_app = function(language = c("eng")){
  if(language == "ptbr"){
    app_ptbr()
  }else if(language == "eng"){
    app_eng()
  }
}
