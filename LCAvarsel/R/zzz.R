.onAttach <- function(lib, pkg)
{
  version <- read.dcf( file.path(lib, pkg, "DESCRIPTION"), "Version" )
  if ( interactive() )
  { # nancyj
    packageStartupMessage("
 _     ____    _                           _ 
| |   / ___|  / \\__   ____ _ _ __ ___  ___| |    Variable 
| |  | |     / _ \\ \\ / / _` | '__/ __|/ _ \\ |    Selection for
| |__| |___ / ___ \\ V / (_| | |  \\__ \\  __/ |    Latent Class
|_____\\____/_/   \\_\\_/ \\__,_|_|  |___/\\___|_|    Analysis
                                                
                                                 Version ", version, "\n" )
  }
  else
  { packageStartupMessage("Package 'LCAvarsel' version ", version) }

  packageStartupMessage("Type 'citation(\"LCAvarsel\")' for citing this R package in publications.")
  invisible()
}
