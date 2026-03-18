welcome<- function(){
msg <- c(paste0(
"Welcome to
 _____                     _____       _____                      ____________________                
|     |                   |     |     |     |                    |                    |                
|     |                   |_____|     |     |                    |                    |               
|     |                    _____      |     |                    |      ______________|               
|     |                   |     |     |     |                    |     |               
|     |                   |     |     |     |                    |     |               
|     |______________     |     |     |     |______________      |     |______________ 
|                    |    |     |     |                    |     |                    |
|                    |    |     |     |                    |     |                    |
|      ________      |    |     |     |      ________      |     |______________      |
|     |        |     |    |     |     |     |        |     |                    |     |
|     |        |     |    |     |     |     |        |     |                    |     |
|     |________|     |    |     |     |     |________|     |      ______________|     |
|                    |    |     |     |                    |     |                    |
|                    |    |     |     |                    |     |                    |
|____________________|    |_____|     |____________________|     |____________________|version ",

packageVersion("bibs")),"\nType 'citation(\"bibs\")' for citing this R package in publications.")
 return(msg)
}
.onAttach <- function(libname, pkgname) {
  mess <- welcome()
  if(!interactive())
    mess[1] <- paste("Package 'bibs' version", packageVersion("bibs"))
    packageStartupMessage(mess)
  invisible()
  }

