#' Automated server creation
#'
#' @return "Driver connection"
#' @export
#'
#' @examples
#' \dontrun{
#' initialization_sgat()
#' }
initialization_sgat <- function() {
  driver <- RSelenium::rsDriver(browser = "firefox", geckover = "0.28.0")
  driver$client$close()
  assign_to_global <- function(func, pos = 1) {
    assign("remDr", func, envir = as.environment(pos))
  }
  assign_to_global(func = {
    driver[["client"]]
  })
  assign_to_global(func = {
    RSelenium::remoteDriver(
      remoteServerAddr = "localhost",
      port = 4567,
      browserName = "firefox"
    )
  })
}
