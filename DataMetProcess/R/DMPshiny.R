#' Launch DataMetProcess Shiny Application
#'
#' @description
#' The `DMPshiny` function is used to start the Shiny application of the `DataMetProcess` package.
#' It allows configuring the host address, port, whether to launch the browser automatically,
#' and the maximum upload size.
#'
#' @param host Character. The host address where the application will run. Default is "127.0.0.1".
#' @param port Integer. The port on which the application will run. If NULL, a random port will be used.
#' @param launch.browser Logical. Indicates whether the browser should be launched automatically. Default is TRUE.
#' @param maxUploadSize Numeric. Maximum upload file size in megabytes. Default is 200.
#'
#' @details
#' The function sets Shiny options, such as the maximum upload size, and then runs the Shiny application located in the `DataMetProcess_Shiny/App.R` directory of the package.
#'
#' @return
#' This function does not return a value. It starts the Shiny server and opens the application in the specified browser.
#'
#' @examples
#' \dontrun{
#'   DMPshiny()
#' }
#'
#' @export

DMPshiny <- function(host = "127.0.0.1",
                     port = NULL,
                     launch.browser = TRUE,
                     maxUploadSize=200
){

  shiny::shinyOptions(maxUploadSize = maxUploadSize)

  shiny::runApp(system.file("DataMetProcess_Shiny/app.R",package="DataMetProcess"),
         launch.browser = launch.browser,
         port = port,
         host = getOption("shiny.host", host))

}

