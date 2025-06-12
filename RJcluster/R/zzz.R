# # .onLoad <- function(libname, pkgname) 
# # {
# #   library.dynam("RJcluster", pkgname, libname)
# # }
# 
RJclusterStartupMessage <- function()
{
  # Startup message obtained as
  # > figlet -f slant RJcluster
  msg <- c(paste("version", utils::packageVersion("RJcluster")), "\nType 'citation(\"RJcluster\")' for citing this R package in publications")
  # msg <- paste("Package 'RJcluster' version", utils::packageVersion("RJcluster"), "is no longer supported and has migrated to GMcluster. Please run install.packages('GMcluster')")
  
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .RJcluster variable allowing its modification
  # unlockBinding(".RJcluster", asNamespace("RJcluster"))
  # startup message
  msg <- RJclusterStartupMessage()
  if (!interactive())
    msg[1] <- paste("Package 'RJcluster' version", utils::packageVersion("RJcluster"), "is no longer supported and has migrated to GMcluster.. Please install package 'GMcluster' instead")
  packageStartupMessage(msg)
  invisible()
}

# .onLoad = function(libname, pkgname)
# {
#   msg = RJclusterStartupMessage()
#   packageStartupMessage(msg)      
#   invisible()
# }

