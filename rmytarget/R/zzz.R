.onLoad <- function(libname, pkgname) {
  
  ## login
  if ( Sys.getenv("RMT_LOGIN") != "" ) {
    
    rmt_login <- Sys.getenv("RMT_LOGIN")
    
  } else {
    
    rmt_login <- NULL
    
  }
  
  ## token path
  if ( Sys.getenv("RMT_TOKEN_PATH") != "" ) {
    
    rmt_token_path <- Sys.getenv("RMT_TOKEN_PATH")
    
  } else {
    
    rmt_token_path <- NULL
    
  }
  
  op <- options()
  op.rmytarget <- list(rmytarget.url                 = "https://target.my.com/",
                       rmytarget.client_id           = "hiM3gdQP04UE8DFR",
                       rmytarget.client_secret       = "Jh5TZlHGKA9GarW8jYzOYsgXnlbnMtSRODkRmHhyFdfpRsG4WbUDK0bFwVm2tK4YJHCtqdyfAYZJEnwHcag9u2jsOXpRkhoGudloum1ueOsZ1KzNAvCYyExZU0HUrZK7TH4VZgx9mh6ZvJS5mRMkcwCTWLOoNWGDwKCFsYmIEwwybgj8HFmWjRdP8bOkGed6kpMPPoo7xqtVsq0U9OyBfbNEAEKHwqWXm4G",
                       rmytarget.code_grant_auth     = TRUE,
                       rmytarget.stat_api_version    = 'v2',
                       rmytarget.login               = rmt_login,
                       rmytarget.token_path          = rmt_token_path)
  toset <- !(names(op.rmytarget) %in% names(op))
  if (any(toset)) options(op.rmytarget[toset])
  
  invisible()
}


.onAttach <- function(lib, pkg,...){
  packageStartupMessage(ryandexdirectWelcomeMessage())
}

#
#

ryandexdirectWelcomeMessage <- function(){
  # library(utils)
  
  paste0("\n",
         "---------------------\n",
         "Welcome to rmytarget version ", utils::packageDescription("rmytarget")$Version, "\n",
         "\n",
         "Author:           Alexey Seleznev (Head of analytics dept at Netpeak).\n",
         "Telegram channel: https://t.me/R4marketing \n",
         "YouTube channel:  https://www.youtube.com/R4marketing/?sub_confirmation=1 \n",
         "Email:            selesnow@gmail.com\n",
         "Site:             https://selesnow.github.io \n",
         "Blog:             https://alexeyseleznev.wordpress.com \n",
         "Facebook:         https://facebook.com/selesnown \n",
         "Linkedin:         https://www.linkedin.com/in/selesnow \n",
         "\n",
         "Type ?rmytarget for the main documentation.\n",
         "Web page for main documentation: https://selesnow.github.io/rmytarget",
         "The github page is: https://github.com/selesnow/rmytarget/\n",
         "\n",
         "Suggestions and bug-reports can be submitted at: https://github.com/selesnow/rmytarget/issues\n",
         "Or contact: <selesnow@gmail.com>\n",
         "\n",
         "\tTo suppress this message use:  ", "suppressPackageStartupMessages(library(rmytarget))\n",
         "---------------------\n"
  )
}
