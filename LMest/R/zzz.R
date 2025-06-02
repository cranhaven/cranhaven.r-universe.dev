.onLoad <- function(libname, pkgname)
{
  library.dynam("LMest", pkgname, libname)
}

.onUnload <- function (lib)
{
  library.dynam.unload("LMest", lib)
}

Startup.Message <- function()
{
  msg <- c(paste0("LMest: Generalized Latent Markov Models",
                  " ","(Version", " ", packageVersion("LMest"),")",
                 # "\nLatent Markov models for longitudinal continuous and categorical data.",
  "\nType 'citation(\"LMest\")' for citing this package."))
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  msg <- Startup.Message()
  if(!interactive())
    msg[1] <- paste("Package 'LMest' version", packageVersion("LMest"))
  packageStartupMessage(msg)
  invisible()
}


