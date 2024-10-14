.onAttach<-function(libname, pkgname){

  v <- utils::packageVersion("mra")  # this requires utils package, which is base
  
  # Startup message
  packageStartupMessage(paste("mra (version ", v ,")", sep=""))
}


