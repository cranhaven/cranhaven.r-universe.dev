#' @importFrom utils packageVersion
StartupMessage <- function()
{
  # Startup message obtained as
  msg <- c(paste0("Credit Model version ", packageVersion("creditmodel"),"
  ____ __  __ 
 / ___|  \\/  |
| |   | |\\/| |
| |___| |  | |
 \\____|_|  |_| \n","WeChatPlatform: hansenmode"))
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- StartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'creditmodel' version", packageVersion("creditmodel"))
  packageStartupMessage(msg)
  invisible()
}

